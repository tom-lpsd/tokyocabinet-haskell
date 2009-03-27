module Main where

import TestUtil
import Test.HUnit hiding (path)
import Database.TokyoCabinet
import qualified Database.TokyoCabinet.BDB as B

import Data.Maybe (catMaybes)
import Data.List (sort)

import Control.Monad
import Control.Exception
import Control.Monad.Trans (liftIO)

withoutFileM :: String -> (String -> TCM a) -> TCM a
withoutFileM fn action = liftIO $ bracket (setupFile fn) teardownFile
                         (runTCM . action)

withOpenedTC :: (TCDB tc) => String -> tc -> (tc -> TCM a) -> TCM a
withOpenedTC name tc action = do
  open tc name [OREADER, OWRITER, OCREAT]
  res <- action tc
  close tc
  return res

tcdb :: (TCDB tc) => (tc -> TCM a) -> TCM a
tcdb = (new >>=)

bdb :: (BDB -> TCM a) -> TCM a
bdb = tcdb

hdb :: (HDB -> TCM a) -> TCM a
hdb = tcdb

fdb :: (FDB -> TCM a) -> TCM a
fdb = tcdb

bbdb :: (B.BDB -> TCM a) -> TCM a
bbdb = tcdb

dbname tc = "foo" ++ (defaultExtension tc)

test_new_delete tc = delete tc

e @=?: a = liftIO $ e @=? a
e @?=: a = liftIO $ e @?= a
e @?: msg = liftIO $ runTCM e @? msg

test_ecode tc =
    withoutFileM (dbname tc) $ \fn -> do
        open tc fn [OREADER]
        ecode tc >>= (ENOFILE @=?:)

test_open_close tc =
    withoutFileM (dbname tc) $ \fn -> do
      not `liftM` open tc fn [OREADER] @?: "file does not exist"
      open tc fn [OREADER, OWRITER, OCREAT] @?: "open"
      close tc @?: "close"
      not `liftM` close tc @?: "cannot close closed file"

test_putxx tc =
    withoutFileM (dbname tc) $ \fn ->
        withOpenedTC fn tc $ \tc' -> do
          put tc' "1" "bar"
          get tc' "1" >>= (Just "bar" @=?:)
          putkeep tc' "1" "baz"
          get tc' "1" >>= (Just "bar" @=?:)
          putcat tc' "1" "baz"
          get tc' "1" >>= (Just "barbaz" @=?:)

test_out tc =
    withoutFileM (dbname tc) $ \fn ->
        withOpenedTC fn tc $ \tc' -> do
          put tc' "1" "bar"
          get tc' "1" >>= (Just "bar" @=?:)
          out tc' "1" @?: "out succeeded"
          get tc' "1" >>= ((Nothing :: Maybe String) @=?:)

test_put_get tc =
    withoutFileM (dbname tc) $ \fn ->
        withOpenedTC fn tc $ \tc' -> do
          put tc' "1" "foo"
          put tc' "2" "bar"
          put tc' "3" "baz"
          get tc' "1" >>= (Just "foo" @=?:)
          get tc' "2" >>= (Just "bar" @=?:)
          get tc' "3" >>= (Just "baz" @=?:)

test_vsiz tc =
    withoutFileM (dbname tc) $ \fn ->
        withOpenedTC fn tc $ \tc' -> do
          put tc' "1" "bar"
          vsiz tc' "1" >>= (Just 3 @=?:)
          vsiz tc' "2" >>= ((Nothing :: Maybe Int) @=?:)

test_iterate tc =
    withoutFileM (dbname tc) $ \fn ->
        withOpenedTC fn tc $ \tc' -> do
          let keys = [1..3] :: [Int]
              vals = ["foo", "bar", "baz"]
          zipWithM_ (put tc') keys vals
          iterinit tc'
          keys' <- sequence $ replicate (length keys) (iternext tc')
          (sort $ catMaybes keys') @?=: (sort keys)

test_fwmkeys tc =
    withoutFileM (dbname tc) $ \fn ->
        withOpenedTC fn tc $ \tc' -> do
          mapM_ (uncurry (put tc')) ([ ("foo", 100)
                                     , ("bar", 200)
                                     , ("baz", 201)
                                     , ("jkl", 300)] :: [(String, Int)])
          fwmkeys tc' "ba" 10 >>= (["bar", "baz"] @=?:) . sort
          fwmkeys tc' "ba" 1 >>= (["bar"] @=?:)
          fwmkeys tc' "" 10 >>= (["bar", "baz", "foo", "jkl"] @=?:) . sort

test_fwmkeys_fdb tc =
    withoutFileM (dbname tc) $ \fn ->
        withOpenedTC fn tc $ \tc' -> do
          zipWithM_ (put tc') ([1..10] :: [Int]) ([100, 200..1000] :: [Int])
          fwmkeys tc' "[min,max]" 10 >>= (([1..10] :: [Int]) @=?:) . sort

test_addint tc =
    withoutFileM (dbname tc) $ \fn ->
        withOpenedTC fn tc $ \tc' -> do
          let ini = 32 :: Int
          put tc' "100" ini
          get tc' "100" >>= (Just ini @=?:)
          addint tc' "100" 3
          get tc' "100" >>= (Just (ini+3) @=?:)
          addint tc' "200" 1 >>= (Just 1 @=?:)
          put tc' "200" "foo"
          addint tc' "200" 1 >>= (Nothing @=?:)

test_adddouble tc =
    withoutFileM (dbname tc) $ \fn ->
        withOpenedTC fn tc $ \tc' -> do
          let ini = 0.003 :: Double
          put tc' "100" ini
          get tc' "100" >>= (Just ini @=?:)
          adddouble tc' "100" 0.3
          (get tc' "100" >>= (return . isIn (ini+0.3))) @?: "isIn"
          adddouble tc' "200" 0.5 >>= (Just 0.5 @=?:)
          put tc' "200" "foo"
          adddouble tc' "200" 1.2 >>= (Nothing @=?:)
    where
      margin = 1e-30
      isIn :: Double -> (Maybe Double) -> Bool
      isIn expected (Just actual) =
          let diff = expected - actual
          in abs diff <= margin

test_vanish tc =
    withoutFileM (dbname tc) $ \fn ->
        withOpenedTC fn tc $ \tc' -> do
            put tc' "100" "111"
            put tc' "200" "222"
            put tc' "300" "333"
            rnum tc' >>= (3 @=?:)
            vanish tc'
            rnum tc' >>= (0 @=?:)

test_copy tc =
    withoutFileM (dbname tc) $ \fns ->    
        withoutFileM ("bar" ++ defaultExtension tc) $ \fnd ->
            withOpenedTC fns tc $ \tc' -> do
                put tc' "100" "bar"
                copy tc' fnd
                close tc'
                open tc' fns [OREADER]
                get tc' "100" >>= (Just "bar" @=?:)

test_path tc =
    withoutFileM (dbname tc) $ \fn ->
        withOpenedTC fn tc $ \tc' ->
            path tc' >>= (Just (dbname tc) @=?:)

test_util tc =
    withoutFileM (dbname tc) $ \fn -> do
      open tc fn [OWRITER, OCREAT]
      path tc >>= (Just fn @=?:)
      rnum tc >>= (0 @=?:)
      ((> 0) `liftM` size tc) @?: "fsiz"
      sync tc @?: "sync"
      close tc

tests = test [
          "new delete BDB" ~: (runTCM $ bdb test_new_delete)
        , "new delete HDB" ~: (runTCM $ hdb test_new_delete)
        , "new delete FDB" ~: (runTCM $ fdb test_new_delete)
        , "new delete B.BDB" ~: (runTCM $ bbdb test_new_delete)
        , "ecode BDB" ~: (runTCM $ bdb test_ecode)
        , "ecode HDB" ~: (runTCM $ hdb test_ecode)
        , "ecode FDB" ~: (runTCM $ fdb test_ecode)
        , "ecode B.BDB" ~: (runTCM $ bbdb test_ecode)
        , "open close BDB" ~: (runTCM $ bdb test_open_close)
        , "open close HDB" ~: (runTCM $ hdb test_open_close)
        , "open close FDB" ~: (runTCM $ fdb test_open_close)
        , "open close B.BDB" ~: (runTCM $ bbdb test_open_close)
        , "putxxx BDB" ~: (runTCM $ bdb test_putxx)
        , "putxxx HDB" ~: (runTCM $ hdb test_putxx)
        , "putxxx FDB" ~: (runTCM $ fdb test_putxx)
        , "putxxx B.BDB" ~: (runTCM $ bbdb test_putxx)
        , "out BDB" ~: (runTCM $ bdb test_out)
        , "out HDB" ~: (runTCM $ hdb test_out)
        , "out FDB" ~: (runTCM $ fdb test_out)
        , "out B.BDB" ~: (runTCM $ bbdb test_out)
        , "put get BDB" ~: (runTCM $ bdb test_put_get)
        , "put get HDB" ~: (runTCM $ hdb test_put_get)
        , "put get FDB" ~: (runTCM $ fdb test_put_get)
        , "put get B.BDB" ~: (runTCM $ bbdb test_put_get)
        , "vsiz BDB" ~: (runTCM $ bdb test_vsiz)
        , "vsiz HDB" ~: (runTCM $ hdb test_vsiz)
        , "vsiz FDB" ~: (runTCM $ fdb test_vsiz)
        , "vsiz B.BDB" ~: (runTCM $ bbdb test_vsiz)
        , "iterate BDB" ~: (runTCM $ bdb test_iterate)
        , "iterate HDB" ~: (runTCM $ hdb test_iterate)
        , "iterate FDB" ~: (runTCM $ fdb test_iterate)
        , "fwmkeys BDB" ~: (runTCM $ bdb test_fwmkeys)
        , "fwmkeys HDB" ~: (runTCM $ hdb test_fwmkeys)
        , "fwmkeys FDB" ~: (runTCM $ fdb test_fwmkeys_fdb)
        , "fwmkeys B.BDB" ~: (runTCM $ bbdb test_fwmkeys_fdb)
        , "addint BDB" ~: (runTCM $ bdb test_addint)
        , "addint HDB" ~: (runTCM $ hdb test_addint)
        , "addint FDB" ~: (runTCM $ fdb test_addint)
        , "addint B.BDB" ~: (runTCM $ bbdb test_addint)
        , "adddouble BDB" ~: (runTCM $ bdb test_adddouble)
        , "adddouble HDB" ~: (runTCM $ hdb test_adddouble)
        , "adddouble FDB" ~: (runTCM $ fdb test_adddouble)
        , "adddouble B.BDB" ~: (runTCM $ bbdb test_adddouble)
        , "vanish BDB" ~: (runTCM $ bdb test_vanish)
        , "vanish HDB" ~: (runTCM $ hdb test_vanish)
        , "vanish FDB" ~: (runTCM $ fdb test_vanish)
        , "vanish B.BDB" ~: (runTCM $ bbdb test_vanish)
        , "copy BDB" ~: (runTCM $ bdb test_copy)
        , "copy HDB" ~: (runTCM $ hdb test_copy)
        , "copy FDB" ~: (runTCM $ fdb test_copy)
        , "copy B.BDB" ~: (runTCM $ bbdb test_copy)
        , "path BDB" ~: (runTCM $ bdb test_path)
        , "path HDB" ~: (runTCM $ hdb test_path)
        , "path FDB" ~: (runTCM $ fdb test_path)
        , "path B.BDB" ~: (runTCM $ bbdb test_path)
        , "util BDB" ~: (runTCM $ bdb test_util)
        , "util HDB" ~: (runTCM $ hdb test_util)
        , "util FDB" ~: (runTCM $ fdb test_util)
        , "util B.BDB" ~: (runTCM $ bbdb test_util)
        ]

main = runTestTT tests
