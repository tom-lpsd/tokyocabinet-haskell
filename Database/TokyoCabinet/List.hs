module Database.TokyoCabinet.List
    (
      new
    , new2
    , delete
    , len
    , get
    , push
    , pop
    , unshift
    , shift
    , insert
    , remove
    , over
    , sort
    , lsearch
    , bsearch
    , clear
    , dump
    , load
    , TCList
    ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (peek)
import Foreign.Marshal (alloca, free, mallocBytes)
import Foreign.Marshal.Utils (maybePeek, copyBytes)

import Database.TokyoCabinet.List.C
import qualified Database.TokyoCabinet.Storable as S

import Data.ByteString.Unsafe
import qualified Data.ByteString as B

data TCList = TCList !(ForeignPtr LIST)

new :: IO TCList
new = c_tclistnew >>= newForeignPtr tclistFinalizer >>= return . TCList

new2 :: Int -> IO TCList
new2 n = do
  l <- c_tclistnew2 (fromIntegral n)
  p <- newForeignPtr tclistFinalizer l
  return $ TCList p

copy :: TCList -> IO TCList
copy (TCList fptr) =
    withForeignPtr fptr $ \p -> do
      l <- c_tclistdup p
      TCList `fmap` newForeignPtr tclistFinalizer l

delete :: TCList -> IO ()
delete (TCList fptr) = finalizeForeignPtr fptr

len :: TCList -> IO Int
len (TCList fptr) =
    withForeignPtr fptr $ \p -> do
        n <- c_tclistnum p
        return $ fromIntegral n

get :: (S.Storable a) => TCList -> Int -> IO (Maybe a)
get (TCList fptr) index =
    withForeignPtr fptr $ \p ->
        alloca $ \sizbuf -> do
            vbuf <- c_tclistval p (fromIntegral index) sizbuf
            flip maybePeek vbuf $ \vp ->
              do siz <- peek sizbuf
                 buf <- mallocBytes (fromIntegral siz)
                 copyBytes buf vp (fromIntegral siz)
                 S.peekPtrLen (buf, fromIntegral siz)

push :: (S.Storable a) => TCList -> a -> IO ()
push (TCList fptr) val =
    withForeignPtr fptr $ \p ->
        S.withPtrLen val $ \(vbuf, vsiz) ->
            c_tclistpush p (castPtr vbuf) (fromIntegral vsiz)

pop :: (S.Storable a) => TCList -> IO (Maybe a)
pop (TCList fptr) =
    withForeignPtr fptr $ \p ->
        alloca $ \sizbuf -> do
          vbuf <- c_tclistpop p sizbuf
          flip maybePeek vbuf $ \vp ->
            do siz <- peek sizbuf
               S.peekPtrLen (vp, fromIntegral siz)

unshift :: (S.Storable a) => TCList -> a -> IO ()
unshift (TCList fptr) val =
    withForeignPtr fptr $ \p ->
        S.withPtrLen val $ \(vbuf, vsiz) ->
            c_tclistunshift p (castPtr vbuf) (fromIntegral vsiz)

shift :: (S.Storable a) => TCList -> IO (Maybe a)
shift (TCList fptr) =
    withForeignPtr fptr $ \p ->
        alloca $ \sizbuf -> do
          vbuf <- c_tclistshift p sizbuf
          flip maybePeek vbuf $ \vp ->
            do siz <- peek sizbuf
               S.peekPtrLen (vp, fromIntegral siz)

insert :: (S.Storable a) => TCList -> Int -> a -> IO ()
insert (TCList fptr) index val =
    withForeignPtr fptr $ \p ->
        S.withPtrLen val $ \(vbuf, vsiz) ->
            c_tclistinsert p (fromIntegral index) (castPtr vbuf)
                             (fromIntegral vsiz)

remove :: (S.Storable a) => TCList -> Int -> IO (Maybe a)
remove (TCList fptr) index =
    withForeignPtr fptr $ \p ->
        alloca $ \sizbuf -> do
          vbuf <- c_tclistremove p (fromIntegral index) sizbuf
          flip maybePeek vbuf $ \vp ->
            do siz <- peek sizbuf
               S.peekPtrLen (vp, fromIntegral siz)

over :: (S.Storable a) => TCList -> Int -> a -> IO ()
over (TCList fptr) index val =
    withForeignPtr fptr $ \p ->
        S.withPtrLen val $ \(vbuf, vsiz) ->
            c_tclistover p (fromIntegral index) (castPtr vbuf)
                           (fromIntegral vsiz)

sort :: TCList -> IO ()
sort (TCList fptr) = withForeignPtr fptr c_tclistsort

lsearch :: (S.Storable a) => TCList -> a -> IO Int
lsearch (TCList fptr) key =
    withForeignPtr fptr $ \p ->
        S.withPtrLen key $ \(kbuf, ksiz) ->
            fmap fromIntegral $
                 c_tclistlsearch p (castPtr kbuf) (fromIntegral ksiz)

bsearch :: (S.Storable a) => TCList -> a -> IO Int
bsearch (TCList fptr) key =
    withForeignPtr fptr $ \p ->
        S.withPtrLen key $ \(kbuf, ksiz) ->
            fmap fromIntegral $
                 c_tclistbsearch p (castPtr kbuf) (fromIntegral ksiz)

clear :: TCList -> IO ()
clear (TCList fptr) = withForeignPtr fptr c_tclistclear

dump :: TCList -> IO B.ByteString
dump (TCList fptr) =
    withForeignPtr fptr $ \p ->
        alloca $ \sizbuf -> do
            c_str <- c_tclistdump p sizbuf
            size <- fromIntegral `fmap` peek sizbuf
            unsafePackCStringFinalizer (castPtr c_str) size (free c_str)

load :: B.ByteString -> IO TCList
load bytes =
    unsafeUseAsCStringLen bytes $ \(buf, siz) -> do
      tclis <- c_tclistload (castPtr buf) (fromIntegral siz)
      TCList `fmap` newForeignPtr tclistFinalizer tclis
