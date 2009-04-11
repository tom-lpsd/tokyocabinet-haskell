module Main where
import Test.HUnit

import Control.Monad
import Database.TokyoCabinet.Map

new_delete_test = do
  m <- new
  delete m
  m' <- new2 10
  delete m'

dup_test = do
  m1 <- new
  put m1 "foo" "bar"
  m2 <- dup m1
  get m2 "foo" >>= (Just "bar" @=?)

putxx_test = do
  m <- new
  put m "foo" "bar"
  get m "foo" >>= (Just "bar" @=?)
  putkeep m "foo" "baz"
  get m "foo" >>= (Just "bar" @=?)
  putcat m "foo" "baz"
  get m "foo" >>= (Just "barbaz" @=?)
  get m "hoge" >>= (Nothing @=?)

out_test = do
  m <- new
  put m "foo" "bar"
  get m "foo" >>= (Just "bar" @=?)
  out m "foo"
  get m "foo" >>= (Nothing @=?)

move_test = do
  m <- new
  put m "foo" "100"
  put m "bar" "200"
  put m "baz" "300"
  keys m >>= ("foo" @=?) . head
  move m "bar" True
  keys m >>= ("bar" @=?) . head
  move m "foo" False
  keys m >>= ("foo" @=?) . last

iter_test = do
  m <- new
  zipWithM_ (put m) ["foo", "bar", "baz"] ([1..] :: [Int])
  iterinit m
  iternext m >>= (Just "foo" @=?)
  iternext m >>= (Just "bar" @=?)
  iternext m >>= (Just "baz" @=?)

util_test = do
  m <- new
  zipWithM_ (put m) ["foo", "bar", "baz"] ([1..] :: [Int])
  rnum m >>= (3 @=?)
  siz <- msiz m
  (siz > 0) @? "msiz returns positive int value"
  m' <- new
  rnum m' >>= (0 @=?)

keys_vals_test = do
  m <- new
  zipWithM_ (put m) ["foo", "bar", "baz"] ([1..] :: [Int])
  keys m >>= (["foo", "bar", "baz"] @=?)
  vals m >>= ([1..3] @=?)
  m' <- new :: IO (Map String String)
  keys m' >>= ([] @=?)
  vals m' >>= ([] @=?)

add_test = do
  mi <- new
  put mi "foo" (1 :: Int)
  addint mi "foo" 23 >>= (Just 24 @=?)
  addint mi "bar" 23 >>= (Just 23 @=?)
  md <- new
  put md "foo" (1 :: Double)
  adddouble md "foo" 2.0 >>= (Just 3.0 @=?)
  adddouble md "bar" 2.0 >>= (Just 2.0 @=?)

clear_test = do
  m <- new
  zipWithM_ (put m) ["foo", "bar", "baz"] ([1..] :: [Int])
  rnum m >>= (3 @=?)
  get m "foo" >>= (Just 1 @=?)
  clear m
  rnum m >>= (0 @=?)
  get m "foo" >>= (Nothing @=?)

cutfront_test = do
  m <- new
  zipWithM_ (put m) ["foo", "bar", "baz"] ([1..] :: [Int])
  keys m >>= ("foo" @=?) . head
  cutfront m 2
  keys m >>= ("baz" @=?) . head
  m' <- new :: IO (Map Int Int)
  cutfront m' 10
  keys m' >>= ([] @=?)

dump_load_test = do
  m <- new
  zipWithM_ (put m) ["foo", "bar", "baz"] ([1..] :: [Int])
  bytes <- dump m
  m' <- load bytes
  liftM2 (==) (keys m) (keys m') @? "original and dumped have the same keys"
  liftM2 (==) (vals m) (vals m') @? "original and dumped have the same vals"
  delete m

tests = test [
          "new delete test" ~: new_delete_test
        , "dup test" ~: dup_test
        , "putxx test" ~: putxx_test
        , "out test" ~: out_test
        , "move test" ~: move_test
        , "iter test" ~: iter_test
        , "util test" ~: util_test
        , "keys vals test" ~: keys_vals_test
        , "add test" ~: add_test
        , "clear test" ~: clear_test
        , "cutfront test" ~: cutfront_test
        , "dump load test" ~: dump_load_test
        ]

main = runTestTT tests
