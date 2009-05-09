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

test_open_close =
    do adb <- new
       not `fmap` open adb "foo.tch#mode=r" @? "file does not exist"
       open adb "+" @? "open"
       close adb @? "close"
       not `fmap` close adb @? "cannot close closed file"

test_putxx =
    do withOpenedADB dbname $ \adb -> do
         put adb "foo" "bar"
         get adb "foo" >>= (Just "bar" @=?)
         putkeep adb "foo" "baz"
         get adb "foo" >>= (Just "bar" @=?)
         putcat adb "foo" "baz"
         get adb "foo" >>= (Just "barbaz" @=?)

test_out =
    do withOpenedADB dbname $ \adb -> do
         put adb "foo" "bar"
         get adb "foo" >>= (Just "bar" @=?)
         out adb "foo" @? "out succeeded"
         get adb "foo" >>= ((Nothing :: Maybe String) @=?)

test_put_get =
    do withOpenedADB dbname $ \adb -> do
         put adb "1" "foo"
         put adb "2" "bar"
         put adb "3" "baz"
         get adb "1" >>= (Just "foo" @=?)
         get adb "2" >>= (Just "bar" @=?)
         get adb "3" >>= (Just "baz" @=?)

test_vsiz =
    do withOpenedADB dbname $ \adb -> do
         put adb "foo" "bar"
         vsiz adb "foo" >>= (Just 3 @=?)
         vsiz adb "bar" >>= ((Nothing :: Maybe Int) @=?)

test_iterate =
    do withOpenedADB dbname $ \adb -> do
         let keys = [1, 2, 3] :: [Int]
             vals = ["foo", "bar", "baz"]
         zipWithM_ (put adb) keys vals
         iterinit adb
         keys' <- sequence $ replicate (length keys) (iternext adb)
         (sort $ catMaybes keys') @?= (sort keys)

test_fwmkeys =
    do withOpenedADB dbname $ \adb -> do
         mapM_ (uncurry (put adb)) ([ ("foo", 100)
                                    , ("bar", 200)
                                    , ("baz", 201)
                                    , ("jkl", 300)] :: [(String, Int)])
         fwmkeys adb "ba" 10 >>= (["bar", "baz"] @=?) . sort
         fwmkeys adb "ba" 1 >>= (["bar"] @=?)
         fwmkeys adb "" 10 >>= (["bar", "baz", "foo", "jkl"] @=?) . sort

test_addint =
    do withOpenedADB dbname $ \adb -> do
         let ini = 32 :: Int
         put adb "foo" ini
         get adb "foo" >>= (Just ini @=?)
         addint adb "foo" 3
         get adb "foo" >>= (Just (ini+3) @=?)
         addint adb "bar" 1 >>= (Just 1 @=?)
         put adb "bar" "foo"
         addint adb "bar" 1 >>= (Nothing @=?)

test_adddouble =
    do withOpenedADB dbname $ \adb -> do
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
    withOpenedADB dbname $ \adb -> do
      put adb "foo" "111"
      put adb "bar" "222"
      put adb "baz" "333"
      rnum adb >>= (3 @=?)
      vanish adb
      rnum adb >>= (0 @=?)

test_copy =
   withoutFile "bar.tch" $ \fnd ->
        withOpenedADB dbname $ \adb -> do
          put adb "foo" "bar"
          copy adb fnd
          close adb
          open adb fnd
          get adb "foo" >>= (Just "bar" @=?)

test_txn =
    withoutFile "foo.tcb" $ \fn ->
        withOpenedADB fn $ \adb -> do
          tranbegin adb @? "tranbegin"
          put adb "foo" "bar"
          get adb "foo" >>= (Just "bar" @=?)
          tranabort adb @? "tranabort"
          get adb "foo" >>= ((Nothing :: Maybe String) @=?)
          tranbegin adb @? "tranbegin2"
          put adb "foo" "baz"
          get adb "foo" >>= (Just "baz" @=?)
          trancommit adb @? "trancommit"
          get adb "foo" >>= (Just "baz" @=?)

test_path =
    withOpenedADB dbname $ \adb ->
        path adb >>= (Just dbname @=?)

test_util =
    do adb <- new
       open adb dbname
       put adb "foo" "bar"
       path adb >>= (Just dbname @=?)
       rnum adb >>= (1 @=?)
       ((> 0) `fmap` size adb) @? "size"
       sync adb @? "sync"
       optimize adb "capnum=1000" @? "optimize"
       close adb

test_misc =
    withoutFile "foo.tct" $ \fn ->
        withOpenedADB fn $ \adb -> do
          misc adb "put" ["1", "foo", "100", "bar", "200"] >>= (([] :: String) @=?)
          misc adb "get" ["1"] >>= (["foo", "100", "bar", "200"] @=?)

tests = test [
          "new delete" ~: test_new_delete
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
        , "misc" ~: test_misc
        ]

main = runTestTT tests
