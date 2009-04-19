module Main where

import Test.HUnit hiding (path)
import TestUtil
import System.Directory
import Database.TokyoCabinet.TDB

dbname :: String
dbname = "foo.tct"

withOpenedTDB :: String -> (TDB -> IO a) -> IO ()
withOpenedTDB name action = do
  h <- new
  open h name [OWRITER, OCREAT]
  action h
  close h
  return ()

test_ecode =
    withoutFile dbname $ \fn -> do
        h <- new
        open h fn [OREADER]
        ecode h >>= (ENOFILE @=?)

test_new_delete = do
  tdb <- new
  delete tdb

test_open_close =
    withoutFile dbname $ \fn -> do
      tdb <- new
      not `fmap` open tdb fn [OREADER] @? "file does not exist"
      open tdb fn [OWRITER, OCREAT] @? "open"
      close tdb @? "close"
      not `fmap` close tdb @? "cannot close closed file"

test_util =
    withoutFile dbname $ \fn -> do
      tdb <- new
      setcache tdb 10000 4096 512 @? "setcache"
      setxmsiz tdb 67108864 @? "setxmsiz"
      tune tdb 100000 (-1) (-1) [TLARGE, TBZIP] @? "tune"
      open tdb fn [OWRITER, OCREAT] @? "open"
      path tdb >>= (Just fn @=?)
      rnum tdb >>= (0 @=?)
      ((> 0) `fmap` fsiz tdb) @? "fsiz"
      sync tdb @? "sync"
      optimize tdb 0 0 0 [] @? "optimize"
      close tdb

samples :: [ (String, AssocList String String) ]
samples = [ ("1", AssocList [("foo", "1"), ("bar", "2")])
          , ("2", AssocList [("foo", "1"), ("bar", "3")])
          , ("3", AssocList [("foo", "2"), ("bar", "1")]) ]

samples' :: [(String, String)]
samples' = [ ("1", "foo\0" ++ "1\0bar\0" ++ "2")
           , ("2", "foo\0" ++ "1\0bar\0" ++ "3")
           , ("3", "foo\0" ++ "2\0bar\0" ++ "1") ]

test_put =
    withoutFile dbname $ \fn -> do
      withOpenedTDB fn $ \tdb ->
          do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
             get tdb "1" >>= (hd @=?)
             putkeep tdb "4" (AssocList [("foo", "10"), ("bar", "20")]) @? "putkeep"
             putkeep tdb "1" (AssocList [("foo", "10"), ("bar", "20")])
             get tdb "1" >>= (hd @=?)
             putcat tdb "1" (AssocList [("baz", "3")]) @? "putcat"
             get tdb "1" >>= (AssocList [("foo", "1"), ("bar", "2"), ("baz", "3")] @=?)
          where
            hd = snd (head samples)

test_put' =
    withoutFile dbname $ \fn -> do
      withOpenedTDB fn $ \tdb ->
          do all id `fmap` mapM (uncurry $ put' tdb) samples' @? "put'"
             get' tdb "1" >>= (Just hd @=?)
             putkeep' tdb "4" ("foo\0" ++ "10\0bar\0" ++ "20") @? "putkeep'"
             putkeep' tdb "1" ("foo\0" ++ "10\0bar\0" ++ "20")
             get' tdb "1" >>= (Just hd @=?)
             putcat' tdb "1" ("baz\0" ++ "3") @? "putcat'"
             get' tdb "1" >>= (Just ("foo\0" ++ "1\0bar\0" ++ "2\0baz\0" ++ "3") @=?)
          where
            hd = snd (head samples')
test_out =
    withoutFile dbname $ \fn -> do
      withOpenedTDB fn $ \tdb ->
          do put tdb "1" (AssocList [("foo", "100"), ("bar", "200")]) @? "put"
             get tdb "1" >>= (AssocList [("foo", "100"), ("bar", "200")] @=?)
             out tdb "1" @? "out"
             get tdb "1" >>= (AssocList ([] :: [(String, String)]) @=?)

test_txn =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb -> do
          let sample = AssocList [("foo", "100"), ("bar", "200")]
              empty = AssocList [] :: AssocList String String
          tranbegin tdb
          put tdb "1" sample
          get tdb "1" >>= (sample @=?)
          tranabort tdb
          get tdb "1" >>= (empty @=?)
          tranbegin tdb
          put tdb "1" sample
          get tdb "1" >>= (sample @=?)
          trancommit tdb
          get tdb "1" >>= (sample @=?)

test_iter =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb ->
            do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
               iterinit tdb @? "iterinit"
               iternext tdb >>= (Just "1" @=?)
               iternext tdb >>= (Just "2" @=?)
               iternext tdb >>= (Just "3" @=?)
               iternext tdb >>= ((Nothing :: Maybe String) @=?)

test_vanish =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb ->
            do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
               get tdb "1" >>= (snd (head samples) @=?)
               vanish tdb
               get tdb "1" >>= ((AssocList [] :: AssocList String String) @=?)

test_copy =
    withoutFile dbname $ \fns ->
        withoutFile "bar.tcb" $ \fnd ->
            withOpenedTDB fns $ \tdb ->
                do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
                   copy tdb fnd @? "copy"
                   close tdb
                   open tdb fnd [OREADER]
                   get tdb "1" >>= (snd (head samples) @=?)

test_vsiz =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb ->
            do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
               vsiz tdb "1" >>= (Just 12 @=?)
               vsiz tdb "10" >>= (Nothing @=?)

test_add =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb ->
            do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
               addint tdb "1" 1 >>= (Just 1 @=?)
               addint tdb "1" 1 >>= (Just 2 @=?)
               v <- get tdb "1"
               lookup "_num" (unAssocList v) @?= Just "2"
               adddouble tdb "2" 1.2 >>= (Just 1.2 @=?)
               adddouble tdb "2" 1.2 >>= (Just 2.4 @=?)
               v <- get tdb "2"
               lookup "_num" (unAssocList v) @?= Just "2.4"

test_fwmkeys =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb ->
            do put tdb "foo" (AssocList [("foo", "100")])
               put tdb "bar" (AssocList [("foo", "100")])
               put tdb "baz" (AssocList [("foo", "100")])
               fwmkeys tdb "ba" 10 >>= (["bar", "baz"] @=?)
               fwmkeys tdb "bak" 10 >>= (([] :: [String]) @=?)
               fwmkeys tdb "" 2 >>= (["foo", "bar"] @=?)

test_genuid =
    withoutFile dbname $ \fn -> do
      tdb <- new
      genuid tdb >>= (Nothing @=?)
      open tdb fn [OWRITER, OCREAT]
      genuid tdb >>= (Just 1 @=?)
      close tdb

test_setindex =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb ->
            do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
               withoutFile (dbname ++ ".idx.foo.dec") $ \ifn -> do
                   setindex tdb "foo" ITDECIMAL
                   doesFileExist ifn @? "index"

tests = test [
          "new delete" ~: test_new_delete
        , "ecode" ~: test_ecode
        , "open close"  ~: test_open_close
        , "util" ~: test_util
        , "put" ~: test_put
        , "put'" ~: test_put'
        , "out" ~: test_out
        , "transaction" ~: test_txn
        , "iterate" ~: test_iter
        , "vanish" ~: test_vanish
        , "copy" ~: test_copy
        , "vsiz" ~: test_vsiz
        , "add" ~: test_add
        , "fwmkeys" ~: test_fwmkeys
        , "genuid" ~: test_genuid
        , "setindex" ~: test_setindex
        ]

main = runTestTT tests
