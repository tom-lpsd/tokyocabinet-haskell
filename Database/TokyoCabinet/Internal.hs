module Database.TokyoCabinet.Internal where

import Database.TokyoCabinet.List.C
import Database.TokyoCabinet.Storable
import Database.TokyoCabinet.Sequence

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable (peek)
import Foreign.Marshal (alloca, copyBytes, mallocBytes)
import Foreign.Marshal.Utils (maybePeek)

import Data.Word

type Lifter ptr tcdb = Ptr ptr -> tcdb
type UnLifter tcdb fptr = tcdb -> ForeignPtr fptr
type Combiner mode c_mode = [mode] -> c_mode
type Caster a b = a -> b
type Checker a = a -> Bool

type FunOpen p c_mode = Ptr p -> CString -> c_mode -> IO Bool
type FunPath p = Ptr p -> IO CString
type FunCopy p = Ptr p -> CString -> IO Bool
type FunPut  p r = Ptr p -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO r
type FunGet  p = Ptr p -> Ptr Word8 -> CInt -> Ptr CInt -> IO (Ptr Word8)
type FunOut  p = Ptr p -> Ptr Word8 -> CInt -> IO Bool
type FunAdd  p n = Ptr p -> Ptr Word8 -> CInt -> n -> IO n
type FunFwm  p = Ptr p -> Ptr Word8 -> CInt -> CInt -> IO (Ptr LIST)
type FunVsiz p =  Ptr p -> Ptr Word8 -> CInt -> IO CInt
type FunIterNext p = Ptr p -> Ptr CInt -> IO (Ptr Word8)

openHelper :: FunOpen p c_mode -> UnLifter tcdb p
           -> Combiner mode c_mode -> tcdb -> String -> [mode] -> IO Bool
openHelper opener unlifter combiner tcdb name modes =
    withForeignPtr (unlifter tcdb) $ \db ->
        withCString name $ \c_name ->
            opener db c_name (combiner modes)

pathHelper :: FunPath p -> UnLifter tcdb p -> tcdb -> IO (Maybe String)
pathHelper c_path unlifter tcdb =
    withForeignPtr (unlifter tcdb) $ \db ->
        c_path db >>= (maybePeek peekCString)

copyHelper :: FunCopy p -> UnLifter tcdb p -> tcdb -> String -> IO Bool
copyHelper c_copy unlifter tcdb fpath =
    withForeignPtr (unlifter tcdb) $ \db -> withCString fpath (c_copy db)

putHelper :: (Storable a, Storable b) =>
             FunPut p r -> UnLifter tcdb p -> tcdb -> a -> b -> IO r
putHelper c_put unlifter tcdb key val =
    withForeignPtr (unlifter tcdb) $ \db ->
        withPtrLen key $ \(kbuf, ksize) ->
        withPtrLen val $ \(vbuf, vsize) -> c_put db kbuf ksize vbuf vsize

getHelper :: (Storable a, Storable b) =>
             FunGet p -> UnLifter tcdb p -> tcdb -> a -> IO (Maybe b)
getHelper c_get unlifter tcdb key =
    withForeignPtr (unlifter tcdb) $ \db ->
        withPtrLen key $ \(kbuf, ksiz) ->
            alloca $ \sizbuf -> do
              vbuf <- c_get db kbuf ksiz sizbuf
              flip maybePeek vbuf $ \vp ->
                  do siz <- peek sizbuf
                     peekPtrLen (vp, siz)

getHelper' :: (Storable a, Storable b) =>
              FunGet p -> UnLifter tcdb p -> tcdb -> a -> IO (Maybe b)
getHelper' c_get unlifter tcdb key =
    withForeignPtr (unlifter tcdb) $ \db ->
        withPtrLen key $ \(kbuf, ksiz) ->
            alloca $ \sizbuf -> do
              vbuf <- c_get db kbuf ksiz sizbuf
              flip maybePeek vbuf $ \vp ->
                  do siz <- peek sizbuf
                     buf <- mallocBytes (fromIntegral siz)
                     copyBytes buf vp (fromIntegral siz)
                     peekPtrLen (buf, siz)

outHelper :: (Storable a) =>
             FunOut p -> UnLifter tcdb p -> tcdb -> a -> IO Bool
outHelper c_out unlifter tcdb key =
    withForeignPtr (unlifter tcdb) $ \db ->
        withPtrLen key $ \(kbuf, ksize) -> c_out db kbuf ksize

addHelper :: (Storable a) =>
             FunAdd p n -> UnLifter tcdb p
                        -> Caster hv n -> Caster n hv -> Checker n
                        -> tcdb -> a -> hv -> IO (Maybe hv)
addHelper c_add unlifter cast_in cast_out check tcdb key num =
    withForeignPtr (unlifter tcdb) $ \db ->
        withPtrLen key $ \(kbuf, ksiz) -> do
            sumval <- c_add db kbuf ksiz (cast_in num)
            return $ if check sumval
                       then Nothing
                       else Just $ cast_out sumval


fwmHelper :: (Storable a, Storable b, Sequence q) =>
             FunFwm p -> UnLifter tcdb p -> tcdb -> a -> Int -> IO (q b)
fwmHelper c_fwm unlifter tcdb key maxn = 
    withForeignPtr (unlifter tcdb) $ \db ->
        withPtrLen key $ \(kbuf, ksiz) ->
            c_fwm db kbuf ksiz (fromIntegral maxn) >>= peekList'

vsizHelper :: (Storable a) =>
              FunVsiz p -> UnLifter tcdb p -> tcdb -> a -> IO (Maybe Int)
vsizHelper c_vsiz unlifter tcdb key =
    withForeignPtr (unlifter tcdb) $ \db ->
        withPtrLen key $ \(kbuf, ksiz) -> do
          vsize <- c_vsiz db kbuf ksiz
          return $ if vsize == -1
                     then Nothing
                     else Just (fromIntegral vsize)

iternextHelper :: (Storable k) =>
                  FunIterNext p -> UnLifter tcdb p -> tcdb -> IO (Maybe k)
iternextHelper c_iternext unlifter tcdb =
    withForeignPtr (unlifter tcdb) $ \p ->
        alloca $ \sizbuf -> do
            vbuf <- c_iternext p sizbuf
            flip maybePeek vbuf $ \vp ->
                do siz <- peek sizbuf
                   peekPtrLen (vp, siz)
