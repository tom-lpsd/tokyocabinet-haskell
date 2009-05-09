module Main where

import Test.HUnit hiding (path)
import TestUtil
import Database.TokyoCabinet.HDB

import Data.Maybe (catMaybes)
import Data.List (sort)
import Control.Monad

dbname :: String
dbname = "foo.tch"

withOpenedHDB :: String -> (HDB -> IO a) -> IO a
withOpenedHDB name action = do
  h <- new
  open h name [OREADER, OWRITER, OCREAT]
  res <- action h
  close h
  return res

test_ecode =
    withoutFile dbname $ \fn -> do
        h <- new
        open h fn [OREADER]
        ecode h >>= (ENOFILE @=?)

test_new_delete = do
  hdb <- new
  delete hdb

test_open_close =
    withoutFile dbname $ \fn -> do
      hdb <- new
      not `fmap` open hdb fn [OREADER] @? "file does not exist"
      open hdb fn [OREADER, OWRITER, OCREAT] @? "open"
      close hdb @? "close"
      not `fmap` close hdb @? "cannot close closed file"

test_putxx =
    withoutFile dbname $ \fn ->
        withOpenedHDB fn $ \hdb -> do
          put hdb "foo" "bar"
          get hdb "foo" >>= (Just "bar" @=?)
          putkeep hdb "foo" "baz"
          get hdb "foo" >>= (Just "bar" @=?)
          putcat hdb "foo" "baz"
          get hdb "foo" >>= (Just "barbaz" @=?)
          putasync hdb "bar" "baz"
          sync hdb
          get hdb "bar" >>= (Just "baz" @=?)

test_out =
    withoutFile dbname $ \fn ->
        withOpenedHDB fn $ \hdb -> do
          put hdb "foo" "bar"
          get hdb "foo" >>= (Just "bar" @=?)
          out hdb "foo" @? "out succeeded"
          get hdb "foo" >>= ((Nothing :: Maybe String) @=?)

test_put_get =
    withoutFile dbname $ \fn ->
        withOpenedHDB fn $ \hdb -> do
          put hdb "1" "foo"
          put hdb "2" "bar"
          put hdb "3" "baz"
          get hdb "1" >>= (Just "foo" @=?)
          get hdb "2" >>= (Just "bar" @=?)
          get hdb "3" >>= (Just "baz" @=?)

test_vsiz =
    withoutFile dbname $ \fn ->
        withOpenedHDB fn $ \hdb -> do
          put hdb "foo" "bar"
          vsiz hdb "foo" >>= (Just 3 @=?)
          vsiz hdb "bar" >>= ((Nothing :: Maybe Int) @=?)

test_iterate =
    withoutFile dbname $ \fn ->
        withOpenedHDB fn $ \hdb -> do
          let keys = [1, 2, 3] :: [Int]
              vals = ["foo", "bar", "baz"]
          zipWithM_ (put hdb) keys vals
          iterinit hdb
          keys' <- sequence $ replicate (length keys) (iternext hdb)
          (sort $ catMaybes keys') @?= (sort keys)

test_fwmkeys =
    withoutFile dbname $ \fn ->
        withOpenedHDB fn $ \hdb -> do
          mapM_ (uncurry (put hdb)) ([ ("foo", 100)
                                     , ("bar", 200)
                                     , ("baz", 201)
                                     , ("jkl", 300)] :: [(String, Int)])
          fwmkeys hdb "ba" 10 >>= (["bar", "baz"] @=?) . sort
          fwmkeys hdb "ba" 1 >>= (["bar"] @=?)
          fwmkeys hdb "" 10 >>= (["bar", "baz", "foo", "jkl"] @=?) . sort

test_addint =
    withoutFile dbname $ \fn ->
        withOpenedHDB fn $ \hdb -> do
          let ini = 32 :: Int
          put hdb "foo" ini
          get hdb "foo" >>= (Just ini @=?)
          addint hdb "foo" 3
          get hdb "foo" >>= (Just (ini+3) @=?)
          addint hdb "bar" 1 >>= (Just 1 @=?)
          put hdb "bar" "foo"
          addint hdb "bar" 1 >>= (Nothing @=?)

test_adddouble =
    withoutFile dbname $ \fn ->
        withOpenedHDB fn $ \hdb -> do
          let ini = 0.003 :: Double
          put hdb "foo" ini
          get hdb "foo" >>= (Just ini @=?)
          adddouble hdb "foo" 0.3
          (get hdb "foo" >>= (isIn (ini+0.3))) @? "isIn"
          adddouble hdb "bar" 0.5 >>= (Just 0.5 @=?)
          put hdb "bar" "foo"
          adddouble hdb "bar" 1.2 >>= (Nothing @=?)
    where
      margin = 1e-30
      isIn :: Double -> (Maybe Double) -> IO Bool
      isIn expected (Just actual) =
          let diff = expected - actual
          in return $ abs diff <= margin

test_vanish =
    withoutFile dbname $ \fn ->
        withOpenedHDB fn $ \hdb -> do
            put hdb "foo" "111"
            put hdb "bar" "222"
            put hdb "baz" "333"
            rnum hdb >>= (3 @=?)
            vanish hdb
            rnum hdb >>= (0 @=?)

test_copy =
    withoutFile dbname $ \fns ->
        withoutFile "bar.tch" $ \fnd ->
            withOpenedHDB fns $ \hdb -> do
                put hdb "foo" "bar"
                copy hdb fnd
                close hdb
                open hdb fnd [OREADER]
                get hdb "foo" >>= (Just "bar" @=?)

test_txn =
    withoutFile dbname $ \fn ->
        withOpenedHDB fn $ \hdb -> do
          tranbegin hdb
          put hdb "foo" "bar"
          get hdb "foo" >>= (Just "bar" @=?)
          tranabort hdb
          get hdb "foo" >>= ((Nothing :: Maybe String) @=?)
          tranbegin hdb
          put hdb "foo" "baz"
          get hdb "foo" >>= (Just "baz" @=?)
          trancommit hdb
          get hdb "foo" >>= (Just "baz" @=?)

test_path =
    withoutFile dbname $ \fn ->
        withOpenedHDB fn $ \hdb ->
            path hdb >>= (Just dbname @=?)

test_util =
    withoutFile dbname $ \fn -> do
      hdb <- new
      setcache hdb 1000000 @? "setcache"
      setxmsiz hdb 1000000 @? "setxmsiz"
      tune hdb 150000 5 11 [TLARGE, TBZIP] @? "tune"
      open hdb fn [OREADER, OWRITER, OCREAT]
      path hdb >>= (Just fn @=?)
      rnum hdb >>= (0 @=?)
      ((> 0) `fmap` fsiz hdb) @? "fsiz"
      sync hdb @? "sync"
      optimize hdb 0 (-1) (-1) [] @? "optimize"
      close hdb

tests = test [
          "new delete" ~: test_new_delete
        , "ecode" ~: test_ecode
        , "open close"  ~: test_open_close
        , "put get" ~: test_put_get
        , "out" ~: test_out
        , "putxx" ~: test_putxx
        , "copy" ~: test_copy
        , "transaction" ~: test_txn
        , "fwmkeys" ~: test_fwmkeys
        , "path" ~: test_path
        , "addint" ~: test_addint
        , "adddouble" ~: test_adddouble
        , "util" ~: test_util
        , "vsiz" ~: test_vsiz
        , "vanish" ~: test_vanish
        , "iterate" ~: test_iterate
        ]

main = runTestTT tests
