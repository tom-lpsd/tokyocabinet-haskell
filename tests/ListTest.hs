module Main where
import Test.HUnit
import Control.Monad

import Data.Int
import Data.Word
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

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

int_test = do
  xs <- new
  let num_int   = [ 1..10] :: [Int]
      num_int8  = [11..20] :: [Int8]
      num_int16 = [21..30] :: [Int16]
      num_int32 = [31..40] :: [Int32]
      num_int64 = [41..50] :: [Int64]
  pushlist xs num_int
  pushlist xs num_int8
  pushlist xs num_int16
  pushlist xs num_int32
  pushlist xs num_int64
  join $ (@?= (Just (num_int   !! 0))) `fmap` get xs 0
  join $ (@?= (Just (num_int   !! 9))) `fmap` get xs 9
  join $ (@?= (Just (num_int8  !! 0))) `fmap` get xs 10
  join $ (@?= (Just (num_int8  !! 9))) `fmap` get xs 19
  join $ (@?= (Just (num_int16 !! 0))) `fmap` get xs 20
  join $ (@?= (Just (num_int16 !! 9))) `fmap` get xs 29
  join $ (@?= (Just (num_int32 !! 0))) `fmap` get xs 30
  join $ (@?= (Just (num_int32 !! 9))) `fmap` get xs 39
  join $ (@?= (Just (num_int64 !! 0))) `fmap` get xs 40
  join $ (@?= (Just (num_int64 !! 9))) `fmap` get xs 49
    where
      pushlist xs ys = mapM_ (push xs) ys

word_test = do
  xs <- new
  let num_word8   = [ 1..10] :: [Word8]
      num_word16  = [11..20] :: [Word16]
      num_word32  = [21..30] :: [Word32]
      num_word64  = [31..40] :: [Word64]
  pushlist xs num_word8
  pushlist xs num_word16
  pushlist xs num_word32
  pushlist xs num_word64
  join $ (@?= (Just (num_word8  !! 0))) `fmap` get xs 0
  join $ (@?= (Just (num_word8  !! 9))) `fmap` get xs 9
  join $ (@?= (Just (num_word16 !! 0))) `fmap` get xs 10
  join $ (@?= (Just (num_word16 !! 9))) `fmap` get xs 19
  join $ (@?= (Just (num_word32 !! 0))) `fmap` get xs 20
  join $ (@?= (Just (num_word32 !! 9))) `fmap` get xs 29
    where
      pushlist xs ys = mapM_ (push xs) ys

strict_bytestring_test = do
  xs <- new
  let vals = map S.pack ["foo", "bar", "baz"]
  mapM_ (push xs) vals
  join $ (@?= (Just (vals  !! 0))) `fmap` get xs 0
  join $ (@?= (Just (vals  !! 1))) `fmap` get xs 1
  join $ (@?= (Just (vals  !! 2))) `fmap` get xs 2  

lazy_bytestring_test = do
  xs <- new
  let vals = map L.pack ["foo", "bar", "baz"]
  mapM_ (push xs) vals
  join $ (@?= (Just (vals  !! 0))) `fmap` get xs 0
  join $ (@?= (Just (vals  !! 1))) `fmap` get xs 1
  join $ (@?= (Just (vals  !! 2))) `fmap` get xs 2  

dump_load_test = do
  xs <- new
  mapM_ (push xs) ["foo", "bar", "baz"]
  bytes <- dump xs
  ys <- load bytes
  xsiz <- len xs
  ysiz <- len ys
  xsiz @?= ysiz
  tclist_equal xs ys 0 xsiz
  where
    tclist_equal xs ys ix num =
        do if ix == num
             then return ()
             else do liftM2 (@?=) (get xs ix :: IO (Maybe String))
                                  (get ys ix :: IO (Maybe String))
                     tclist_equal xs ys (1 + ix) num

tests = test [
          "len test" ~: len_test
        , "push pop test" ~: push_pop_test
        , "unshift shift test" ~: unshift_shift_test
        , "clear test" ~: clear_test
        , "insert remove test" ~: insert_remove_test
        , "sort test" ~: sort_test
        , "lsearch test" ~: lsearch_test
        , "bsearch test" ~: bsearch_test
        , "int_test" ~: int_test
        , "word_test" ~: word_test
        , "strict_bytestring_test" ~: strict_bytestring_test
        , "lazy_bytestring_test" ~: lazy_bytestring_test
        , "dump_load_test" ~: dump_load_test
        ]

main = runTestTT tests
