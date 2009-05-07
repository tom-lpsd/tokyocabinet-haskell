module Main where

import Test.HUnit hiding (path)
import TestUtil
import Database.TokyoCabinet.ADB

import Data.Maybe (catMaybes)
import Data.List (sort)
import Control.Monad

dbname :: String
dbname = "+"

withOpenedADB :: String -> (ADB -> IO a) -> IO a
withOpenedADB name action = do
  a <- new
  open a name
  res <- action a
  close a
  return res

test_new_delete = new >>= delete

{-
test_open_close =
    withoutFile dbname $ \fn -> do
      adb <- new
      not `fmap` open adb fn [OREADER] @? "file does not exist"
      open adb fn [OREADER, OWRITER, OCREAT] @? "open"
      close adb @? "close"
      not `fmap` close adb @? "cannot close closed file"

test_putxx =
    withoutFile dbname $ \fn ->
        withOpenedADB fn $ \adb -> do
          put adb "foo" "bar"
          get adb "foo" >>= (Just "bar" @=?)
          putkeep adb "foo" "baz"
          get adb "foo" >>= (Just "bar" @=?)
          putcat adb "foo" "baz"
          get adb "foo" >>= (Just "barbaz" @=?)
          putasync adb "bar" "baz"
          sync adb
          get adb "bar" >>= (Just "baz" @=?)

test_out =
    withoutFile dbname $ \fn ->
        withOpenedADB fn $ \adb -> do
          put adb "foo" "bar"
          get adb "foo" >>= (Just "bar" @=?)
          out adb "foo" @? "out succeeded"
          get adb "foo" >>= ((Nothing :: Maybe String) @=?)

test_put_get =
    withoutFile dbname $ \fn ->
        withOpenedADB fn $ \adb -> do
          put adb "1" "foo"
          put adb "2" "bar"
          put adb "3" "baz"
          get adb "1" >>= (Just "foo" @=?)
          get adb "2" >>= (Just "bar" @=?)
          get adb "3" >>= (Just "baz" @=?)

test_vsiz =
    withoutFile dbname $ \fn ->
        withOpenedADB fn $ \adb -> do
          put adb "foo" "bar"
          vsiz adb "foo" >>= (Just 3 @=?)
          vsiz adb "bar" >>= ((Nothing :: Maybe Int) @=?)

test_iterate =
    withoutFile dbname $ \fn ->
        withOpenedADB fn $ \adb -> do
          let keys = [1, 2, 3] :: [Int]
              vals = ["foo", "bar", "baz"]
          zipWithM_ (put adb) keys vals
          iterinit adb
          keys' <- sequence $ replicate (length keys) (iternext adb)
          (sort $ catMaybes keys') @?= (sort keys)

test_fwmkeys =
    withoutFile dbname $ \fn ->
        withOpenedADB fn $ \adb -> do
          mapM_ (uncurry (put adb)) ([ ("foo", 100)
                                     , ("bar", 200)
                                     , ("baz", 201)
                                     , ("jkl", 300)] :: [(String, Int)])
          fwmkeys adb "ba" 10 >>= (["bar", "baz"] @=?) . sort
          fwmkeys adb "ba" 1 >>= (["bar"] @=?)
          fwmkeys adb "" 10 >>= (["bar", "baz", "foo", "jkl"] @=?) . sort

test_addint =
    withoutFile dbname $ \fn ->
        withOpenedADB fn $ \adb -> do
          let ini = 32 :: Int
          put adb "foo" ini
          get adb "foo" >>= (Just ini @=?)
          addint adb "foo" 3
          get adb "foo" >>= (Just (ini+3) @=?)
          addint adb "bar" 1 >>= (Just 1 @=?)
          put adb "bar" "foo"
          addint adb "bar" 1 >>= (Nothing @=?)

test_adddouble =
    withoutFile dbname $ \fn ->
        withOpenedADB fn $ \adb -> do
          let ini = 0.003 :: Double
          put adb "foo" ini
          get adb "foo" >>= (Just ini @=?)
          adddouble adb "foo" 0.3
          (get adb "foo" >>= (isIn (ini+0.3))) @? "isIn"
          adddouble adb "bar" 0.5 >>= (Just 0.5 @=?)
          put adb "bar" "foo"
          adddouble adb "bar" 1.2 >>= (Nothing @=?)
    where
      margin = 1e-30
      isIn :: Double -> (Maybe Double) -> IO Bool
      isIn expected (Just actual) =
          let diff = expected - actual
          in return $ abs diff <= margin

test_vanish =
    withoutFile dbname $ \fn ->
        withOpenedADB fn $ \adb -> do
            put adb "foo" "111"
            put adb "bar" "222"
            put adb "baz" "333"
            rnum adb >>= (3 @=?)
            vanish adb
            rnum adb >>= (0 @=?)

test_copy =
    withoutFile dbname $ \fns ->
        withoutFile "bar.tch" $ \fnd ->
            withOpenedADB fns $ \adb -> do
                put adb "foo" "bar"
                copy adb fnd
                close adb
                open adb fns [OREADER]
                get adb "foo" >>= (Just "bar" @=?)

test_txn =
    withoutFile dbname $ \fn ->
        withOpenedADB fn $ \adb -> do
          tranbegin adb
          put adb "foo" "bar"
          get adb "foo" >>= (Just "bar" @=?)
          tranabort adb
          get adb "foo" >>= ((Nothing :: Maybe String) @=?)
          tranbegin adb
          put adb "foo" "baz"
          get adb "foo" >>= (Just "baz" @=?)
          trancommit adb
          get adb "foo" >>= (Just "baz" @=?)

test_path =
    withoutFile dbname $ \fn ->
        withOpenedADB fn $ \adb ->
            path adb >>= (Just dbname @=?)

test_util =
    withoutFile dbname $ \fn -> do
      adb <- new
      setcache adb 1000000 @? "setcache"
      setxmsiz adb 1000000 @? "setxmsiz"
      tune adb 150000 5 11 [TLARGE, TBZIP] @? "tune"
      open adb fn [OREADER, OWRITER, OCREAT]
      path adb >>= (Just fn @=?)
      rnum adb >>= (0 @=?)
      ((> 0) `fmap` fsiz adb) @? "fsiz"
      sync adb @? "sync"
      optimize adb 0 (-1) (-1) [] @? "optimize"
      close adb
-}

tests = test [
          "new delete" ~: test_new_delete
--        , "open close"  ~: test_open_close
--        , "put get" ~: test_put_get
--        , "out" ~: test_out
--        , "putxx" ~: test_putxx
--        , "copy" ~: test_copy
--        , "transaction" ~: test_txn
--        , "fwmkeys" ~: test_fwmkeys
--        , "path" ~: test_path
--        , "addint" ~: test_addint
--        , "adddouble" ~: test_adddouble
--        , "util" ~: test_util
--        , "vsiz" ~: test_vsiz
--        , "vanish" ~: test_vanish
--        , "iterate" ~: test_iterate
        ]

main = runTestTT tests
