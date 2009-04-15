module Main where

import Test.HUnit hiding (path)
import TestUtil
import Data.IORef
import Data.List (isPrefixOf)
import Database.TokyoCabinet.TDB
import Database.TokyoCabinet.TDB.Query hiding (new, delete)
import qualified Database.TokyoCabinet.TDB.Query as Q (new, delete)

dbname :: String
dbname = "foo.tct"

withOpenedTDB :: String -> (TDB -> IO a) -> IO ()
withOpenedTDB name action = do
  h <- new
  open h name [OWRITER, OCREAT]
  action h
  close h
  return ()

test_new_delete = do
  tdb <- new
  qry <- Q.new tdb
  Q.delete qry
  delete tdb

samples :: [ (String, AssocList String String) ]
samples = [ ("hop" , AssocList [("foo", "1"), ("bar", "abc")])
          , ("step", AssocList [("foo", "2"), ("bar", "def")])
          , ("jump", AssocList [("foo", "3"), ("bar", "deg")]) ]


test_addcond =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb ->
            do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
               qry <- Q.new tdb
               addcond qry "foo" QCNUMGT "1"
               search qry >>= (["step", "jump"] @=?)
               qry <- Q.new tdb
               addcond qry "bar" QCSTRBW "de"
               search qry >>= (["step", "jump"] @=?)
               qry <- Q.new tdb
               addcond qry "bar" QCSTRBW "de"
               search qry >>= (["step", "jump"] @=?)
               qry <- Q.new tdb
               addcond qry "bar" (QCNEGATE QCSTRBW) "de"
               search qry >>= (["hop"] @=?)
               qry <- Q.new tdb
               addcond qry "bar" QCSTRRX ".e."
               search qry >>= (["step", "jump"] @=?)

test_setorder =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb ->
            do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
               qry <- Q.new tdb
               addcond qry "foo" QCNUMGT "0"
               setorder qry "bar" QOSTRDESC
               search qry >>= (["jump", "step", "hop"] @=?)
               qry <- Q.new tdb
               addcond qry "foo" QCNUMGT "0"
               setorder qry "bar" QOSTRASC
               search qry >>= (["hop", "step", "jump"] @=?)
               qry <- Q.new tdb
               addcond qry "bar" QCSTRBW ""
               setorder qry "foo" QONUMDESC
               search qry >>= (["jump", "step", "hop"] @=?)
               qry <- Q.new tdb
               addcond qry "bar" QCSTRBW ""
               setorder qry "foo" QONUMASC
               search qry >>= (["hop", "step", "jump"] @=?)

test_setlimit =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb ->
            do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
               qry <- Q.new tdb
               addcond qry "foo" QCNUMGT "0"
               setlimit qry 1 0
               search qry >>= (["hop"] @=?)
               setlimit qry 1 1
               search qry >>= (["step"] @=?)
               setlimit qry 2 2
               search qry >>= (["jump"] @=?)
               setlimit qry 2 1
               search qry >>= (["step", "jump"] @=?)

test_searchout =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb ->
            do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
               qry <- Q.new tdb
               addcond qry "foo" QCNUMGT "2"
               searchout qry @? "searchout"
               fwmkeys tdb "" (-1) >>= (["hop", "step"] @=?)

test_hint =
    withoutFile dbname $ \fn ->
    withoutFile (dbname ++ ".idx.foo.dec") $ \_ ->
        withOpenedTDB fn $ \tdb ->
            do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
               qry <- Q.new tdb
               addcond qry "foo" QCNUMGT "2"
               search qry :: IO [String]
               ("scanning the whole table" `isPrefixOf`) `fmap` hint qry @? "whole"
               setindex tdb "foo" ITDECIMAL
               search qry :: IO [String]
               ("using an index: \"foo\"" `isPrefixOf`) `fmap` hint qry @? "index"

test_proc1 =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb ->
            do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
               qry <- Q.new tdb
               addcond qry "foo" QCNUMGE "2"
               (proc qry $ \pkey (AssocList cols) -> do
                 let val = [(k, v ++ "!") | (k, v) <- cols] :: [(String, String)]
                 return $ QPPUT (AssocList val)) @? "proc"
               (AssocList res) <- get tdb "step"
               lookup "foo" res @?= Just "2!"
               (AssocList res) <- get tdb "jump"
               lookup "foo" res @?= Just "3!"

test_proc2 =
    withoutFile dbname $ \fn ->
        withOpenedTDB fn $ \tdb ->
            do all id `fmap` mapM (uncurry $ put tdb) samples @? "put"
               acc <- newIORef [] :: IO (IORef [String])
               qry <- Q.new tdb
               addcond qry "foo" QCNUMGE "1"
               proc qry $ \pkey (AssocList cols) -> do
                 let Just foo = lookup "foo" cols
                 writeIORef acc . (foo:) =<< readIORef acc
                 return $ if foo == "2" then QPSTOP else QPNOP
               readIORef acc >>= (["2", "1"] @=?)

tests = test [
          "new delete" ~: test_new_delete
        , "addcond" ~: test_addcond
        , "setorder" ~: test_setorder
        , "setlimit" ~: test_setlimit
        , "searchout" ~: test_searchout
        , "hint" ~: test_hint
        , "proc1" ~: test_proc1
        , "proc2" ~: test_proc2
        ]

main = runTestTT tests
