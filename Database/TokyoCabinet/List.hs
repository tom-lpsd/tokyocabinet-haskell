module Database.TokyoCabinet.List
    (
      new
    , new2
    , copy
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
    , List
    ) where

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (peek)
import Foreign.Marshal (alloca, free, mallocBytes)
import Foreign.Marshal.Utils (maybePeek, copyBytes)

import Database.TokyoCabinet.List.C
import Database.TokyoCabinet.Storable

import Data.ByteString.Unsafe
import qualified Data.ByteString as B

new :: IO (List a)
new = c_tclistnew >>= newForeignPtr tclistFinalizer >>= return . List

new2 :: Int -> IO (List a)
new2 n = do
  l <- c_tclistnew2 (fromIntegral n)
  p <- newForeignPtr tclistFinalizer l
  return $ List p

copy :: List a -> IO (List a)
copy tcls =
    withForeignPtr (unTCList tcls) $ \p -> do
      l <- c_tclistdup p
      List `fmap` newForeignPtr tclistFinalizer l

delete :: List a -> IO ()
delete tcls = finalizeForeignPtr (unTCList tcls)

len :: List a -> IO Int
len tcls =
    withForeignPtr (unTCList tcls) $ \p -> do
        n <- c_tclistnum p
        return $ fromIntegral n

get :: (Storable a) => List a -> Int -> IO (Maybe a)
get tcls index =
    withForeignPtr (unTCList tcls) $ \p ->
        alloca $ \sizbuf -> do
            vbuf <- c_tclistval p (fromIntegral index) sizbuf
            flip maybePeek vbuf $ \vp ->
              do siz <- peek sizbuf
                 buf <- mallocBytes (fromIntegral siz)
                 copyBytes buf vp (fromIntegral siz)
                 peekPtrLen (buf, fromIntegral siz)

push :: (Storable a) => List a -> a -> IO ()
push tcls val =
    withForeignPtr (unTCList tcls) $ \p ->
        withPtrLen val $ \(vbuf, vsiz) ->
            c_tclistpush p (castPtr vbuf) (fromIntegral vsiz)

pop :: (Storable a) => List a -> IO (Maybe a)
pop tcls =
    withForeignPtr (unTCList tcls) $ \p ->
        alloca $ \sizbuf -> do
          vbuf <- c_tclistpop p sizbuf
          flip maybePeek vbuf $ \vp ->
            do siz <- peek sizbuf
               peekPtrLen (vp, fromIntegral siz)

unshift :: (Storable a) => List a -> a -> IO ()
unshift tcls val =
    withForeignPtr (unTCList tcls) $ \p ->
        withPtrLen val $ \(vbuf, vsiz) ->
            c_tclistunshift p (castPtr vbuf) (fromIntegral vsiz)

shift :: (Storable a) => List a -> IO (Maybe a)
shift tcls =
    withForeignPtr (unTCList tcls) $ \p ->
        alloca $ \sizbuf -> do
          vbuf <- c_tclistshift p sizbuf
          flip maybePeek vbuf $ \vp ->
            do siz <- peek sizbuf
               peekPtrLen (vp, fromIntegral siz)

insert :: (Storable a) => List a -> Int -> a -> IO ()
insert tcls index val =
    withForeignPtr (unTCList tcls) $ \p ->
        withPtrLen val $ \(vbuf, vsiz) ->
            c_tclistinsert p (fromIntegral index) (castPtr vbuf)
                             (fromIntegral vsiz)

remove :: (Storable a) => List a -> Int -> IO (Maybe a)
remove tcls index =
    withForeignPtr (unTCList tcls) $ \p ->
        alloca $ \sizbuf -> do
          vbuf <- c_tclistremove p (fromIntegral index) sizbuf
          flip maybePeek vbuf $ \vp ->
            do siz <- peek sizbuf
               peekPtrLen (vp, fromIntegral siz)

over :: (Storable a) => List a -> Int -> a -> IO ()
over tcls index val =
    withForeignPtr (unTCList tcls) $ \p ->
        withPtrLen val $ \(vbuf, vsiz) ->
            c_tclistover p (fromIntegral index) (castPtr vbuf)
                           (fromIntegral vsiz)

sort :: List a -> IO ()
sort tcls = withForeignPtr (unTCList tcls) c_tclistsort

lsearch :: (Storable a) => List a -> a -> IO Int
lsearch tcls key =
    withForeignPtr (unTCList tcls) $ \p ->
        withPtrLen key $ \(kbuf, ksiz) ->
            fmap fromIntegral $
                 c_tclistlsearch p (castPtr kbuf) (fromIntegral ksiz)

bsearch :: (Storable a) => List a -> a -> IO Int
bsearch tcls key =
    withForeignPtr (unTCList tcls) $ \p ->
        withPtrLen key $ \(kbuf, ksiz) ->
            fmap fromIntegral $
                 c_tclistbsearch p (castPtr kbuf) (fromIntegral ksiz)

clear :: List a -> IO ()
clear tcls = withForeignPtr (unTCList tcls) c_tclistclear

dump :: List a -> IO B.ByteString
dump tcls =
    withForeignPtr (unTCList tcls) $ \p ->
        alloca $ \sizbuf -> do
            c_str <- c_tclistdump p sizbuf
            size <- fromIntegral `fmap` peek sizbuf
            unsafePackCStringFinalizer (castPtr c_str) size (free c_str)

load :: B.ByteString -> IO (List a)
load bytes =
    unsafeUseAsCStringLen bytes $ \(buf, siz) -> do
      tclis <- c_tclistload (castPtr buf) (fromIntegral siz)
      List `fmap` newForeignPtr tclistFinalizer tclis
