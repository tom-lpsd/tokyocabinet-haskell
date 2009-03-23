module Main where

import Test.HUnit hiding (path)
import TestUtil
import Database.TokyoCabinet.BDB
import qualified Database.TokyoCabinet.BDB.Cursor as C

import Data.Maybe (catMaybes)
import Data.List (sort)
import Control.Monad

dbname :: String
dbname = "foo.tcb"

withOpendBDB :: String -> (TCBDB -> IO a) -> IO ()
withOpendBDB name action = do
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
  bdb <- new
  delete bdb

test_open_close =
    withoutFile dbname $ \fn -> do
      bdb <- new
      not `fmap` open bdb fn [OREADER] @? "file does not exist"
      open bdb fn [OREADER, OWRITER, OCREAT] @? "open"
      close bdb @? "close"
      not `fmap` close bdb @? "cannot close closed file"

test_putxx =
    withoutFile dbname $ \fn ->
        withOpendBDB fn $ \bdb -> do
          put bdb "foo" "bar"
          get bdb "foo" >>= (Just "bar" @=?)
          putkeep bdb "foo" "baz"
          get bdb "foo" >>= (Just "bar" @=?)
          putcat bdb "foo" "baz"
          get bdb "foo" >>= (Just "barbaz" @=?)

test_out =
    withoutFile dbname $ \fn ->
        withOpendBDB fn $ \bdb -> do
          put bdb "foo" "bar"
          get bdb "foo" >>= (Just "bar" @=?)
          out bdb "foo" @? "out succeeded"
          get bdb "foo" >>= ((Nothing :: Maybe String) @=?)

test_put_get =
    withoutFile dbname $ \fn ->
        withOpendBDB fn $ \bdb -> do
          put bdb "1" "foo"
          put bdb "2" "bar"
          put bdb "3" "baz"
          get bdb "1" >>= (Just "foo" @=?)
          get bdb "2" >>= (Just "bar" @=?)
          get bdb "3" >>= (Just "baz" @=?)

test_vsiz =
    withoutFile dbname $ \fn ->
        withOpendBDB fn $ \bdb -> do
          put bdb "foo" "bar"
          vsiz bdb "foo" >>= (Just 3 @=?)
          vsiz bdb "bar" >>= ((Nothing :: Maybe Int) @=?)

test_iterate =
    withoutFile dbname $ \fn ->
        withOpendBDB fn $ \bdb -> do
          mapM_ (uncurry (put bdb)) ([ ("foo", 100)
                                     , ("bar", 200)
                                     , ("baz", 201)
                                     , ("jkl", 300)] :: [(String, Int)])
          cur <- C.new bdb
          C.first cur
          C.key cur >>= (Just "bar" @=?)
          C.val cur >>= (Just (200 :: Int) @=?)
          C.out cur @? "cursor out"
          get bdb "bar" >>= ((Nothing :: Maybe String) @=?)
          C.key cur >>= (Just "baz" @=?)
          C.val cur >>= (Just (201 :: Int) @=?)
          C.next cur @? "cursor next"
          C.key cur >>= (Just "foo" @=?)
          C.val cur >>= (Just (100 :: Int) @=?)
          C.prev cur @? "cursor prev"
          C.key cur >>= (Just "baz" @=?)
          C.val cur >>= (Just (201 :: Int) @=?)
          C.jump cur "b" @? "cursor jump"
          C.key cur >>= (Just "baz" @=?)
          C.put cur (111 :: Int) C.CPAFTER @? "cursor put"
          getlist bdb "baz" >>= (([201, 111] :: [Int]) @=?)

test_fwmkeys =
    withoutFile dbname $ \fn ->
        withOpendBDB fn $ \bdb -> do
          mapM_ (uncurry (put bdb)) ([ ("foo", 100)
                                     , ("bar", 200)
                                     , ("baz", 201)
                                     , ("jkl", 300)] :: [(String, Int)])
          fwmkeys bdb "ba" 10 >>= (["bar", "baz"] @=?) . sort
          fwmkeys bdb "ba" 1 >>= (["bar"] @=?)
          fwmkeys bdb "" 10 >>= (["bar", "baz", "foo", "jkl"] @=?) . sort

test_addint =
    withoutFile dbname $ \fn ->
        withOpendBDB fn $ \bdb -> do
          let ini = 32 :: Int
          put bdb "foo" ini
          get bdb "foo" >>= (Just ini @=?)
          addint bdb "foo" 3
          get bdb "foo" >>= (Just (ini+3) @=?)
          addint bdb "bar" 1 >>= (Just 1 @=?)
          put bdb "bar" "foo"
          addint bdb "bar" 1 >>= (Nothing @=?)

test_adddouble =
    withoutFile dbname $ \fn ->
        withOpendBDB fn $ \bdb -> do
          let ini = 0.003 :: Double
          put bdb "foo" ini
          get bdb "foo" >>= (Just ini @=?)
          adddouble bdb "foo" 0.3
          (get bdb "foo" >>= (isIn (ini+0.3))) @? "isIn"
          adddouble bdb "bar" 0.5 >>= (Just 0.5 @=?)
          put bdb "bar" "foo"
          adddouble bdb "bar" 1.2 >>= (Nothing @=?)
    where
      margin = 1e-30
      isIn :: Double -> (Maybe Double) -> IO Bool
      isIn expected (Just actual) =
          let diff = expected - actual
          in return $ abs diff <= margin

test_vanish =
    withoutFile dbname $ \fn ->
        withOpendBDB fn $ \bdb -> do
            put bdb "foo" "111"
            put bdb "bar" "222"
            put bdb "baz" "333"
            rnum bdb >>= (3 @=?)
            vanish bdb
            rnum bdb >>= (0 @=?)

test_copy =
    withoutFile dbname $ \fns ->
        withoutFile "bar.tcb" $ \fnd ->
            withOpendBDB fns $ \bdb -> do
                put bdb "foo" "bar"
                copy bdb fnd
                close bdb
                open bdb fns [OREADER]
                get bdb "foo" >>= (Just "bar" @=?)

test_txn =
    withoutFile dbname $ \fn ->
        withOpendBDB fn $ \bdb -> do
          tranbegin bdb
          put bdb "foo" "bar"
          get bdb "foo" >>= (Just "bar" @=?)
          tranabort bdb
          get bdb "foo" >>= ((Nothing :: Maybe String) @=?)
          tranbegin bdb
          put bdb "foo" "baz"
          get bdb "foo" >>= (Just "baz" @=?)
          trancommit bdb
          get bdb "foo" >>= (Just "baz" @=?)

test_path =
    withoutFile dbname $ \fn ->
        withOpendBDB fn $ \bdb ->
            path bdb >>= (Just dbname @=?)

test_util =
    withoutFile dbname $ \fn -> do
      bdb <- new
      setcache bdb 1000000 0 @? "setcache"
      setxmsiz bdb 1000000 @? "setxmsiz"
      tune bdb 0 0 0 (-1) (-1) [TLARGE, TBZIP] @? "tune"
      open bdb fn [OREADER, OWRITER, OCREAT]
      path bdb >>= (Just fn @=?)
      rnum bdb >>= (0 @=?)
      ((> 0) `fmap` fsiz bdb) @? "fsiz"
      sync bdb @? "sync"
      optimize bdb 0 0 0 (-1) (-1) [] @? "optimize"
      close bdb

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
