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
  (@?= (Just "baz")) =<< pop xs
  (@?= (Just "bar")) =<< pop xs
  (@?= (Just "foo")) =<< pop xs
  (@?= (Nothing :: Maybe String)) =<< pop xs
  delete xs

unshift_shift_test = do
  xs <- new
  unshift xs "foo"
  unshift xs "bar"
  unshift xs "baz"
  (@?= (Just "baz")) =<< shift xs
  (@?= (Just "bar")) =<< shift xs
  (@?= (Just "foo")) =<< shift xs
  (@?= (Nothing :: Maybe String)) =<< shift xs
  delete xs

clear_test = do
  xs <- new
  push xs ""
  push xs ""
  push xs ""
  (@?= 3) =<< len xs
  clear xs
  (@?= 0) =<< len xs
  delete xs

insert_remove_test = do
  xs <- new
  (@?= (Nothing :: Maybe String)) =<< remove xs 0
  insert xs 0 "foo"
  insert xs 0 "bar"
  insert xs 1 "baz"
  (@?= 3)            =<< len xs
  (@?= (Just "bar")) =<< get xs 0
  (@?= (Just "baz")) =<< get xs 1
  (@?= (Just "foo")) =<< get xs 2
  (@?= (Nothing :: Maybe String)) =<< get xs 3
  remove xs 0 :: IO (Maybe String)
  (@?= 2) =<< len xs
  (@?= (Just "foo")) =<< get xs 1
  remove xs 1 :: IO (Maybe String)
  (@?= 1)                         =<< len xs
  (@?= (Just "baz"))              =<< get xs 0
  (@?= (Nothing :: Maybe String)) =<< get xs 1
  (@?= (Nothing :: Maybe String)) =<< remove xs 1
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
  (@?= 3)        =<< len xs
  (@?= (Just z)) =<< get xs 0
  (@?= (Just x)) =<< get xs 1
  (@?= (Just y)) =<< get xs 2
  delete xs

lsearch_test = do
  xs <- new
  let x = 12 :: Int
      y = 49 :: Int
      z = 5  :: Int
  push xs x
  push xs y
  push xs z
  (@?= 2) =<< lsearch xs z
  (@?= 0) =<< lsearch xs x
  (@?= 1) =<< lsearch xs y
  delete xs

bsearch_test = do
  xs <- new
  let x = 12 :: Int
      y = 49 :: Int
      z = 5  :: Int
  push xs z
  push xs x
  push xs y
  (@?= 0) =<< bsearch xs z
  (@?= 1) =<< bsearch xs x
  (@?= 2) =<< bsearch xs y
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
  get xs 0  >>= (@?= (Just (num_int   !! 0)))
  get xs 9  >>= (@?= (Just (num_int   !! 9)))
  get xs 10 >>= (@?= (Just (num_int8  !! 0)))
  get xs 19 >>= (@?= (Just (num_int8  !! 9)))
  get xs 20 >>= (@?= (Just (num_int16 !! 0)))
  get xs 29 >>= (@?= (Just (num_int16 !! 9)))
  get xs 30 >>= (@?= (Just (num_int32 !! 0)))
  get xs 39 >>= (@?= (Just (num_int32 !! 9)))
  get xs 40 >>= (@?= (Just (num_int64 !! 0)))
  get xs 49 >>= (@?= (Just (num_int64 !! 9)))
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
  get xs 0  >>= (@?= (Just (num_word8  !! 0)))
  get xs 9  >>= (@?= (Just (num_word8  !! 9)))
  get xs 10 >>= (@?= (Just (num_word16 !! 0)))
  get xs 19 >>= (@?= (Just (num_word16 !! 9)))
  get xs 20 >>= (@?= (Just (num_word32 !! 0)))
  get xs 29 >>= (@?= (Just (num_word32 !! 9)))
    where
      pushlist xs ys = mapM_ (push xs) ys

strict_bytestring_test = do
  xs <- new
  let vals = map S.pack ["foo", "bar", "baz"]
  mapM_ (push xs) vals
  get xs 0 >>= (@?= (Just (vals  !! 0)))
  get xs 1 >>= (@?= (Just (vals  !! 1)))
  get xs 2 >>= (@?= (Just (vals  !! 2)))

lazy_bytestring_test = do
  xs <- new
  let vals = map L.pack ["foo", "bar", "baz"]
  mapM_ (push xs) vals
  get xs 0 >>= (@?= (Just (vals  !! 0)))
  get xs 1 >>= (@?= (Just (vals  !! 1)))
  get xs 2 >>= (@?= (Just (vals  !! 2)))

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
