module Main where

import Test.HUnit
import Database.TokyoCabinet.FDB

test_new_delete = do
  fdb <- new
  delete fdb

tests = test [
          "new delete" ~: test_new_delete
        ]

main = runTestTT tests
