{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Database.TokyoCabinet.List where

import Foreign.C.Types
import Foreign.C.String

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable
import Foreign.Marshal
import Foreign.Marshal.Utils (maybePeek, copyBytes)

import Data.Word

import qualified Database.TokyoCabinet.Storable as S

import qualified Data.ByteString as B
import Data.ByteString.Unsafe

#include <tcutil.h>

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

data LIST

foreign import ccall safe "tclistnew"
  c_tclistnew :: IO (Ptr LIST)

foreign import ccall safe "tclistnew2"
  c_tclistnew2 :: CInt -> IO (Ptr LIST)

foreign import ccall safe "tclistdup"
  c_tclistdup :: Ptr LIST -> IO (Ptr LIST)

foreign import ccall safe "tclistdel"
  c_tclistdel :: Ptr LIST -> IO ()

foreign import ccall "&tclistdel"
  tclistFinalizer :: FunPtr (Ptr LIST -> IO ())

foreign import ccall safe "tclistnum"
  c_tclistnum :: Ptr LIST -> IO CInt

foreign import ccall safe "tclistval"
  c_tclistval :: Ptr LIST -> CInt -> Ptr CInt -> IO CString

foreign import ccall safe "tclistval2"
  c_tclistval2 :: Ptr LIST -> CInt -> IO CString

foreign import ccall safe "tclistpush"
  c_tclistpush :: Ptr LIST -> Ptr Word8 -> CInt -> IO ()

foreign import ccall safe "tclistpush2"
  c_tclistpush2 :: Ptr LIST -> CString -> IO ()

foreign import ccall safe "tclistpop"
  c_tclistpop :: Ptr LIST -> Ptr CInt -> IO CString

foreign import ccall safe "tclistpop2"
  c_tclistpop2 :: Ptr LIST -> IO CString

foreign import ccall safe "tclistunshift"
  c_tclistunshift :: Ptr LIST -> Ptr Word8 -> CInt -> IO ()

foreign import ccall safe "tclistunshift2"
  c_tclistunshift2 :: Ptr LIST -> Ptr Word8 -> IO ()

foreign import ccall safe "tclistshift"
  c_tclistshift :: Ptr LIST -> Ptr CInt -> IO CString

foreign import ccall safe "tclistshift2"
  c_tclistshift2 :: Ptr LIST -> IO CString

foreign import ccall safe "tclistinsert"
  c_tclistinsert :: Ptr LIST -> CInt -> Ptr Word8 -> CInt -> IO ()

foreign import ccall safe "tclistinsert2"
  c_tclistinsert2 :: Ptr LIST -> CInt -> Ptr Word8 -> IO ()

foreign import ccall safe "tclistremove"
  c_tclistremove :: Ptr LIST -> CInt -> Ptr CInt -> IO CString

foreign import ccall safe "tclistremove2"
  c_tclistremove2 :: Ptr LIST -> CInt -> IO CString

foreign import ccall safe "tclistover"
  c_tclistover :: Ptr LIST -> CInt -> Ptr Word8 -> CInt -> IO ()

foreign import ccall safe "tclistover2"
  c_tclistover2 :: Ptr LIST -> CInt -> Ptr Word8 -> IO ()

foreign import ccall safe "tclistsort"
  c_tclistsort :: Ptr LIST -> IO ()

foreign import ccall safe "tclistlsearch"
  c_tclistlsearch :: Ptr LIST -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall safe "tclistbsearch"
  c_tclistbsearch :: Ptr LIST -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall safe "tclistclear"
  c_tclistclear :: Ptr LIST -> IO ()

foreign import ccall safe "tclistdump"
  c_tclistdump :: Ptr LIST -> Ptr CInt -> IO CString

foreign import ccall safe "tclistload"
  c_tclistload :: Ptr Word8 -> CInt -> IO (Ptr LIST)
