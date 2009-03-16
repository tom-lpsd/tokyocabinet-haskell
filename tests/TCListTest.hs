module Main where
import Test.HUnit
import Data.ByteString.Char8 (pack)

import Data.TCList

tests = test [do { h <- new;
                   push h (pack "");
                   push h (pack "");
                   (==2) `fmap` num h
                 } ~? "num",
              do { h <- new;
                   push h (pack "foo");
                   push h (pack "bar");
                   (== Just (pack "bar")) `fmap` get h 1
                 } ~? "get"
             ]

main = runTestTT tests
