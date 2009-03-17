module Main where
import Test.HUnit
import Control.Monad

import Database.TokyoCabinet.List

len_test = do
  xs <- new
  (==0) `fmap` len xs @? "empty"
  push xs "foo"
  (==1) `fmap` len xs @? "1 element"
  push xs "baz"
  (==2) `fmap` len xs @? "2 elements"
  pop xs :: IO (Maybe String)
  (==1) `fmap` len xs @? "1 element again"
  pop xs :: IO (Maybe String)
  (==0) `fmap` len xs @? "empty again"
  delete xs

push_pop_test = do
  xs <- new
  push xs "foo"
  push xs "bar"
  push xs "baz"
  mapM_ join [(@?= (Just "baz")) `fmap` pop xs,
              (@?= (Just "bar")) `fmap` pop xs,
              (@?= (Just "foo")) `fmap` pop xs,
              (@?= (Nothing :: Maybe String)) `fmap` pop xs]
  delete xs

unshift_shift_test = do
  xs <- new
  unshift xs "foo"
  unshift xs "bar"
  unshift xs "baz"
  mapM_ join [(@?= (Just "baz")) `fmap` shift xs,
              (@?= (Just "bar")) `fmap` shift xs,
              (@?= (Just "foo")) `fmap` shift xs,
              (@?= (Nothing :: Maybe String)) `fmap` shift xs]
  delete xs

clear_test = do
  xs <- new
  push xs ""
  push xs ""
  push xs ""
  join $ (@?= 3) `fmap` len xs
  clear xs
  join $ (@?= 0) `fmap` len xs
  delete xs

insert_remove_test = do
  xs <- new
  join $ (@?= (Nothing :: Maybe String)) `fmap` remove xs 0
  insert xs 0 "foo"
  insert xs 0 "bar"
  insert xs 1 "baz"
  forM_ [ (@?= 3) `fmap` len xs
        , (@?= (Just "bar")) `fmap` get xs 0
        , (@?= (Just "baz")) `fmap` get xs 1
        , (@?= (Just "foo")) `fmap` get xs 2
        , (@?= (Nothing :: Maybe String)) `fmap` get xs 3 ] join
  remove xs 0 :: IO (Maybe String)
  forM_ [ (@?= 2) `fmap` len xs
        , (@?= (Just "foo")) `fmap` get xs 1 ] join
  remove xs 1 :: IO (Maybe String)
  forM_ [ (@?= 1) `fmap` len xs
        , (@?= (Just "baz")) `fmap` get xs 0
        , (@?= (Nothing :: Maybe String)) `fmap` get xs 1
        , (@?= (Nothing :: Maybe String)) `fmap` remove xs 1 ] join
  delete xs

sort_test = do
  xs <- new
  let x = 12 :: Int
      y = 49 :: Int
      z = 5  :: Int
  push xs x
  push xs y
  push xs z
  sort xs
  forM_ [ (@?= 3) `fmap` len xs
        , (@?= (Just z)) `fmap` get xs 0
        , (@?= (Just x)) `fmap` get xs 1
        , (@?= (Just y)) `fmap` get xs 2 ] join  
  delete xs

lsearch_test = do
  xs <- new
  let x = 12 :: Int
      y = 49 :: Int
      z = 5  :: Int
  push xs x
  push xs y
  push xs z
  forM_ [ (@?= 2) `fmap` lsearch xs z
        , (@?= 0) `fmap` lsearch xs x
        , (@?= 1) `fmap` lsearch xs y ] join  
  delete xs

bsearch_test = do
  xs <- new
  let x = 12 :: Int
      y = 49 :: Int
      z = 5  :: Int
  push xs z
  push xs x
  push xs y
  forM_ [ (@?= 0) `fmap` bsearch xs z
        , (@?= 1) `fmap` bsearch xs x
        , (@?= 2) `fmap` bsearch xs y ] join  
  delete xs

tests = test [
          len_test
        , push_pop_test
        , unshift_shift_test
        , clear_test
        , insert_remove_test
        , sort_test
        , lsearch_test
        , bsearch_test
        ]

main = runTestTT tests
