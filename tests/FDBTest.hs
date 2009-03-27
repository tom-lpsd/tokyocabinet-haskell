module Main where

import Test.HUnit hiding (path)
import TestUtil
import Database.TokyoCabinet.FDB

import Data.Maybe (catMaybes, fromJust)
import Data.List (sort)
import Control.Monad

dbname :: String
dbname = "foo.tcf"

withOpenedFDB :: String -> (FDB -> IO a) -> IO ()
withOpenedFDB name action = do
  h <- new
  open h name [OREADER, OWRITER, OCREAT]
  res <- action h
  close h
  return ()

test_ecode =
    withoutFile dbname $ \fn -> do
        h <- new
        open h fn [OREADER]
        ecode h >>= (ENOFILE @=?)

test_new_delete = do
  fdb <- new
  delete fdb

test_open_close =
    withoutFile dbname $ \fn -> do
      fdb <- new
      not `fmap` open fdb fn [OREADER] @? "file does not exist"
      open fdb fn [OREADER, OWRITER, OCREAT] @? "open"
      close fdb @? "close"
      not `fmap` close fdb @? "cannot close closed file"

test_putxx =
    withoutFile dbname $ \fn ->
        withOpenedFDB fn $ \fdb -> do
          put fdb "1" "bar"
          get fdb "1" >>= (Just "bar" @=?)
          putkeep fdb "1" "baz"
          get fdb "1" >>= (Just "bar" @=?)
          putcat fdb "1" "baz"
          get fdb "1" >>= (Just "barbaz" @=?)

test_out =
    withoutFile dbname $ \fn ->
        withOpenedFDB fn $ \fdb -> do
          put fdb "1" "bar"
          get fdb "1" >>= (Just "bar" @=?)
          out fdb "1" @? "out succeeded"
          get fdb "1" >>= ((Nothing :: Maybe String) @=?)

test_put_get =
    withoutFile dbname $ \fn ->
        withOpenedFDB fn $ \fdb -> do
          put fdb "2" "foo"
          put fdb "3" "bar"
          put fdb "4" "baz"
          put fdb IDNEXT "hoge"
          put fdb IDPREV "fuga"
          get fdb "2" >>= (Just "foo" @=?)
          get fdb "3" >>= (Just "bar" @=?)
          get fdb "4" >>= (Just "baz" @=?)
          get fdb (5 :: Int) >>= (Just "hoge" @=?)
          get fdb (1 :: Int) >>= (Just "fuga" @=?)
          put fdb IDMAX "max"
          put fdb IDMIN "min"
          get fdb (5 :: Int) >>= (Just "max" @=?)
          get fdb (1 :: Int) >>= (Just "min" @=?)

test_iterate =
    withoutFile dbname $ \fn ->
        withOpenedFDB fn $ \fdb -> do
          let keys = [1, 2, 3] :: [Int]
              vals = ["foo", "bar", "baz"]
          zipWithM_ (put fdb) keys vals
          iterinit fdb
          keys' <- sequence $ replicate (length keys) (iternext fdb)
          (sort $ catMaybes keys') @?= (sort keys)
          iternext fdb >>= ((Nothing :: Maybe String) @=?)

test_vsiz =
    withoutFile dbname $ \fn ->
        withOpenedFDB fn $ \fdb -> do
          put fdb "123" "bar"
          vsiz fdb "123" >>= (Just 3 @=?)
          vsiz fdb "234" >>= ((Nothing :: Maybe Int) @=?)

test_range =
    withoutFile dbname $ \fn ->
        withOpenedFDB fn $ \fdb -> do
          zipWithM_ (put fdb) ([1..10] :: [Int]) ([100, 200..1000] :: [Int])
          range fdb (2 :: Int) (5 :: Int) (-1)
                    >>= (([2..5] :: [Int]) @=?)
          range fdb IDMIN IDMAX (-1)
                    >>= (([1..10] :: [Int]) @=?)
          range fdb IDMIN IDMAX 2
                    >>= (([1..2] :: [Int]) @=?)

test_fwmkeys =
    withoutFile dbname $ \fn ->
        withOpenedFDB fn $ \fdb -> do
          zipWithM_ (put fdb) ([1..10] :: [Int]) ([100, 200..1000] :: [Int])
          fwmkeys fdb "[min,max]" 10 >>= ((map show [1..10]) @=?)

test_addint =
    withoutFile dbname $ \fn ->
        withOpenedFDB fn $ \fdb -> do
          let ini = 32 :: Int
          put fdb "10" ini
          get fdb "10" >>= (Just ini @=?)
          addint fdb "10" 3
          get fdb "10" >>= (Just (ini+3) @=?)
          addint fdb "20" 1 >>= (Just 1 @=?)
          put fdb "20" "foo"
          addint fdb "20" 1 >>= (Nothing @=?)

test_adddouble =
    withoutFile dbname $ \fn ->
        withOpenedFDB fn $ \fdb -> do
          let ini = 0.003 :: Double
          put fdb "1" ini
          get fdb "1" >>= (Just ini @=?)
          adddouble fdb "1" 0.3
          (get fdb "1" >>= (isIn (ini+0.3))) @? "isIn"
          adddouble fdb "2" 0.5 >>= (Just 0.5 @=?)
          put fdb "2" "foo"
          adddouble fdb "2" 1.2 >>= (Nothing @=?)
    where
      margin = 1e-30
      isIn :: Double -> (Maybe Double) -> IO Bool
      isIn expected (Just actual) =
          let diff = expected - actual
          in return $ abs diff <= margin

test_vanish =
    withoutFile dbname $ \fn ->
        withOpenedFDB fn $ \fdb -> do
            put fdb "1" "111"
            put fdb "2" "222"
            put fdb "3" "333"
            rnum fdb >>= (3 @=?)
            vanish fdb
            rnum fdb >>= (0 @=?)

test_copy =
    withoutFile dbname $ \fns ->
        withoutFile "bar.tcf" $ \fnd ->
            withOpenedFDB fns $ \fdb -> do
                put fdb "1" "bar"
                copy fdb fnd
                close fdb
                open fdb fns [OREADER]
                get fdb "1" >>= (Just "bar" @=?)

test_path =
    withoutFile dbname $ \fn ->
        withOpenedFDB fn $ \fdb ->
            path fdb >>= (Just dbname @=?)

test_util =
    withoutFile dbname $ \fn -> do
      fdb <- new
      tune fdb 0 100000000 @? "tune"
      open fdb fn [OREADER, OWRITER, OCREAT]
      path fdb >>= (Just fn @=?)
      rnum fdb >>= (0 @=?)
      ((> 0) `fmap` fsiz fdb) @? "fsiz"
      sync fdb @? "sync"
      put fdb (1 :: Int) "dummy record"
      optimize fdb 0 0 @? "optimize"
      close fdb

tests = test [
          "new delete" ~: test_new_delete
        , "ecode" ~: test_ecode
        , "open close"  ~: test_open_close
        , "put get" ~: test_put_get
        , "out" ~: test_out
        , "putxx" ~: test_putxx
        , "copy" ~: test_copy
        , "range" ~: test_range
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
