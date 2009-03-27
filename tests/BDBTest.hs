module Main where

import Test.HUnit hiding (path)
import TestUtil
import Database.TokyoCabinet.BDB
import qualified Database.TokyoCabinet.BDB.Cursor as C

import Data.Maybe (catMaybes, fromJust)
import Data.List (sort)
import Control.Monad

dbname :: String
dbname = "foo.tcb"

withOpenedBDB :: String -> (BDB -> IO a) -> IO ()
withOpenedBDB name action = do
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
        withOpenedBDB fn $ \bdb -> do
          put bdb "foo" "bar"
          get bdb "foo" >>= (Just "bar" @=?)
          putkeep bdb "foo" "baz"
          get bdb "foo" >>= (Just "bar" @=?)
          putcat bdb "foo" "baz"
          get bdb "foo" >>= (Just "barbaz" @=?)
          putdup bdb "foo" "bar2" @? "putdup"
          getlist bdb "foo" >>= (["barbaz", "bar2"] @=?)
          putlist bdb "bar" ["hoge", "fuga", "abc"] @? "putlist"
          getlist bdb "bar" >>= (["hoge", "fuga", "abc"] @=?)

test_out =
    withoutFile dbname $ \fn ->
        withOpenedBDB fn $ \bdb -> do
          put bdb "foo" "bar"
          get bdb "foo" >>= (Just "bar" @=?)
          out bdb "foo" @? "out succeeded"
          get bdb "foo" >>= ((Nothing :: Maybe String) @=?)
          putlist bdb "bar" ([1, 2, 3] :: [Int])
          out bdb "bar" -- first one is removed
          get bdb "bar" >>= ((Just 2 :: Maybe Int) @=?)
          outlist bdb "bar"
          get bdb "bar" >>= ((Nothing :: Maybe Int) @=?)

test_put_get =
    withoutFile dbname $ \fn ->
        withOpenedBDB fn $ \bdb -> do
          put bdb "1" "foo"
          put bdb "2" "bar"
          put bdb "3" "baz"
          get bdb "1" >>= (Just "foo" @=?)
          get bdb "2" >>= (Just "bar" @=?)
          get bdb "3" >>= (Just "baz" @=?)
          putdup bdb "1" "foo2"
          get bdb "4" >>= ((Nothing :: Maybe String) @=?)
          getlist bdb "1" >>= (["foo", "foo2"] @=?)
          getlist bdb "4" >>= (([] :: [String]) @=?)

test_vnum =
    withoutFile dbname $ \fn ->
        withOpenedBDB fn $ \bdb -> do
          putlist bdb "foo" ["bar", "baz", "hoge", "fuga"]
          vnum bdb "foo" >>= (Just 4 @=?)
          vnum bdb "bar" >>= (Nothing @=?)

test_vsiz =
    withoutFile dbname $ \fn ->
        withOpenedBDB fn $ \bdb -> do
          put bdb "foo" "bar"
          vsiz bdb "foo" >>= (Just 3 @=?)
          vsiz bdb "bar" >>= ((Nothing :: Maybe Int) @=?)

test_iterate =
    withoutFile dbname $ \fn ->
        withOpenedBDB fn $ \bdb -> do
          let keys  = ["foo", "bar", "baz", "jkl"]
              vals  = [100, 200 ..] :: [Int]
              kvs = sort $ zip keys vals
              destkey = "baz"
              destval = fromJust $ lookup destkey kvs
          zipWithM (put bdb) keys vals
          cur <- C.new bdb
          C.first cur
          C.key cur >>= (Just (fst . head $ kvs) @=?)
          C.val cur >>= (Just (snd . head $ kvs) @=?)
          C.out cur @? "cursor out"
          get bdb (fst . head $ kvs) >>= ((Nothing :: Maybe String) @=?)
          C.key cur >>= (Just (fst . (!! 1) $ kvs) @=?)
          C.val cur >>= (Just (snd . (!! 1) $ kvs) @=?)
          C.next cur @? "cursor next"
          C.key cur >>= (Just (fst . (!! 2) $ kvs) @=?)
          C.val cur >>= (Just (snd . (!! 2) $ kvs) @=?)
          C.prev cur @? "cursor prev"
          C.key cur >>= (Just (fst . (!! 1) $ kvs) @=?)
          C.val cur >>= (Just (snd . (!! 1) $ kvs) @=?)
          C.jump cur "b" @? "cursor jump"
          C.key cur >>= (Just destkey @=?)
          C.put cur (100 :: Int) C.CPAFTER @? "cursor put"
          getlist bdb destkey >>= (([destval, 100] :: [Int]) @=?)
          C.last cur @? "cursor last"
          C.key cur >>= (Just (fst . last $ kvs) @=?)
          C.delete cur

test_range =
    withoutFile dbname $ \fn ->
        withOpenedBDB fn $ \bdb -> do
          let keys = ["abc", "abd", "bcd", "bcz", "fgh", "ghjc", "ziji"]
          zipWithM (put bdb) keys ([1..] :: [Int])
          range bdb (Just "a") True (Just "abz") True 10
                    >>= (["abc", "abd"] @=?)
          range bdb (Just "a") True (Just "abd") False 10
                    >>= (["abc"] @=?)
          range bdb (Just "abc") False (Just "fgh") False 3
                    >>= (["abd", "bcd", "bcz"] @=?)
          range bdb (Just "a") False (Just "ab") False 10
                    >>= (([] :: [String]) @=?)
          range bdb Nothing False Nothing False (-1) >>= (keys @=?)


test_fwmkeys =
    withoutFile dbname $ \fn ->
        withOpenedBDB fn $ \bdb -> do
          mapM_ (uncurry (put bdb)) ([ ("foo", 100)
                                     , ("bar", 200)
                                     , ("baz", 201)
                                     , ("jkl", 300)] :: [(String, Int)])
          fwmkeys bdb "ba" 10 >>= (["bar", "baz"] @=?) . sort
          fwmkeys bdb "ba" 1 >>= (["bar"] @=?)
          fwmkeys bdb "" 10 >>= (["bar", "baz", "foo", "jkl"] @=?) . sort

test_addint =
    withoutFile dbname $ \fn ->
        withOpenedBDB fn $ \bdb -> do
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
        withOpenedBDB fn $ \bdb -> do
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
        withOpenedBDB fn $ \bdb -> do
            put bdb "foo" "111"
            put bdb "bar" "222"
            put bdb "baz" "333"
            rnum bdb >>= (3 @=?)
            vanish bdb
            rnum bdb >>= (0 @=?)

test_copy =
    withoutFile dbname $ \fns ->
        withoutFile "bar.tcb" $ \fnd ->
            withOpenedBDB fns $ \bdb -> do
                put bdb "foo" "bar"
                copy bdb fnd
                close bdb
                open bdb fns [OREADER]
                get bdb "foo" >>= (Just "bar" @=?)

test_txn =
    withoutFile dbname $ \fn ->
        withOpenedBDB fn $ \bdb -> do
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
        withOpenedBDB fn $ \bdb ->
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
        , "vnum" ~: test_vnum
        , "out" ~: test_out
        , "putxx" ~: test_putxx
        , "copy" ~: test_copy
        , "transaction" ~: test_txn
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
