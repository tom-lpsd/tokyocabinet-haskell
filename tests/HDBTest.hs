module Main where

import Test.HUnit hiding (path)
import TestUtil
import Database.TokyoCabinet.HDB

import Data.Maybe (catMaybes)
import Data.List (sort)
import Control.Monad

test_new_delete = do
  hdb <- new
  delete hdb

test_open_close =
    withFile "foo.tch" $ \fn -> do
      hdb <- new
      not `fmap` open hdb fn [OREADER] @? "file does not exist"
      open hdb fn [OREADER, OWRITER, OCREAT] @? "open"
      close hdb @? "close"
      not `fmap` close hdb @? "cannot close closed file"

test_put_get =
    withFile "foo.tch" $ \fn -> do
      hdb <- new
      open hdb fn [OWRITER, OCREAT, OREADER]
      put hdb "1" "foo"
      put hdb "2" "bar"
      put hdb "3" "baz"
      get hdb "1" >>= (Just "foo" @=?)
      get hdb "2" >>= (Just "bar" @=?)
      get hdb "3" >>= (Just "baz" @=?)
      close hdb

test_out =
    withFile "foo.tch" $ \fn -> do
      hdb <- new
      open hdb fn [OREADER, OWRITER, OCREAT]
      put hdb "foo" "bar"
      get hdb "foo" >>= (Just "bar" @=?)
      out hdb "foo" @? "out succeeded"
      get hdb "foo" >>= ((Nothing :: Maybe String) @=?)
      close hdb             

test_putxx =
    withFile "foo.tch" $ \fn -> do
      hdb <- new
      open hdb fn [OREADER, OWRITER, OCREAT]
      put hdb "foo" "bar"
      get hdb "foo" >>= (Just "bar" @=?)
      putkeep hdb "foo" "baz"
      get hdb "foo" >>= (Just "bar" @=?)
      putcat hdb "foo" "baz"
      get hdb "foo" >>= (Just "barbaz" @=?)
      close hdb

test_txn =
    withFile "foo.tch" $ \fn -> do
      hdb <- new
      open hdb fn [OREADER, OWRITER, OCREAT]
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
      close hdb

test_addint =
    withFile "foo.tch" $ \fn -> do
      hdb <- new
      open hdb fn [OREADER, OWRITER, OCREAT]
      let ini = 32 :: Int
      put hdb "foo" ini
      get hdb "foo" >>= (Just ini @=?)
      addint hdb "foo" 3
      get hdb "foo" >>= (Just (ini+3) @=?)
      close hdb

test_adddouble =
    withFile "foo.tch" $ \fn -> do
      hdb <- new
      open hdb fn [OREADER, OWRITER, OCREAT]
      let ini = 0.003 :: Double
      put hdb "foo" ini
      get hdb "foo" >>= (Just ini @=?)
      adddouble hdb "foo" 0.3
      (get hdb "foo" >>= (isIn (ini+0.3))) @? "isIn"
      close hdb
    where
      margin = 1e-30
      isIn :: Double -> (Maybe Double) -> IO Bool
      isIn expected (Just actual) =
          let diff = expected - actual
          in return $ abs diff <= margin

test_util =
    withFile "foo.tch" $ \fn -> do
      hdb <- new
      open hdb fn [OREADER, OWRITER, OCREAT]
      path hdb >>= (Just fn @=?)
      rnum hdb >>= (0 @=?)
      ((> 0) `fmap` fsiz hdb) @? "fsiz"
      close hdb

test_vsiz =
    withFile "foo.tch" $ \fn -> do
      hdb <- new
      open hdb fn [OREADER, OCREAT, OWRITER]
      put hdb "foo" "bar"
      vsiz hdb "foo" >>= (Just 3 @=?)
      close hdb

test_vanish =
    withFile "foo.tch" $ \fn -> do
      hdb <- new
      open hdb fn [OREADER, OCREAT, OWRITER]
      put hdb "foo" "111"
      put hdb "bar" "222"
      put hdb "baz" "333"
      rnum hdb >>= (3 @=?)
      vanish hdb
      rnum hdb >>= (0 @=?)
      close hdb

test_iterate =
    withFile "foo.tch" $ \fn -> do
      hdb <- new
      open hdb fn [OREADER, OWRITER, OCREAT]
      let keys = [1, 2, 3] :: [Int]
          vals = ["foo", "bar", "baz"]
      zipWithM_ (put hdb) keys vals
      iterinit hdb
      keys' <- sequence $ replicate (length keys) (iternext hdb)
      (sort $ catMaybes keys') @?= (sort keys)
      close hdb

tests = test [
          "new delete" ~: test_new_delete
        , "open close"  ~: test_open_close
        , "put get" ~: test_put_get
        , "out" ~: test_out
        , "putxx" ~: test_putxx
        , "transaction" ~: test_txn
        , "addint" ~: test_addint
        , "adddouble" ~: test_adddouble
        , "util" ~: test_util
        , "vsiz" ~: test_vsiz
        , "vanish" ~: test_vanish
        , "iterate" ~: test_iterate
        ]

main = runTestTT tests
