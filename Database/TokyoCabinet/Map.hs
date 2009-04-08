module Database.TokyoCabinet.Map
    (
      new
    , new2
    , dup
    , delete
    , put
    , putkeep
    , putcat
    , out
    , get
    , move
    , iterinit
    , iternext
    , rnum
    , msiz
    , keys
    , vals
    , addint
    , adddouble
    , clear
    , cutfront
    , dump
    , load
    , Map
    ) where

import Database.TokyoCabinet.Map.C
import Database.TokyoCabinet.Storable
import Database.TokyoCabinet.Sequence
import Database.TokyoCabinet.Internal
import Database.TokyoCabinet.Error (cINT_MIN)

import Data.Word

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (peek)
import Foreign.Marshal (alloca, mallocBytes, free)
import Foreign.Marshal.Utils (maybePeek, copyBytes)

import Data.ByteString (ByteString)
import Data.ByteString.Unsafe
    (
      unsafeUseAsCStringLen
    , unsafePackCStringFinalizer
    )

new :: IO (Map k v)
new = Map `fmap` (c_tcmapnew >>= newForeignPtr tcmapFinalizer)

new2 :: Word32 -> IO (Map k v)
new2 num = Map `fmap` (c_tcmapnew2 num >>= newForeignPtr tcmapFinalizer)

dup :: Map k v -> IO (Map k v)
dup m =
    withForeignPtr (unMap m) $ \m' ->
        Map `fmap` (c_tcmapdup m' >>= newForeignPtr tcmapFinalizer)

delete :: Map k v -> IO ()
delete m = finalizeForeignPtr (unMap m)

put :: (Storable k, Storable v) => Map k v -> k -> v -> IO ()
put = putHelper c_tcmapput unMap

putkeep :: (Storable k, Storable v) => Map k v -> k -> v -> IO Bool
putkeep = putHelper c_tcmapputkeep unMap

putcat :: (Storable k, Storable v) => Map k v -> k -> v -> IO ()
putcat = putHelper c_tcmapputcat unMap

out :: (Storable k) => Map k v -> k -> IO Bool
out = outHelper c_tcmapout unMap

get :: (Storable k, Storable v) => Map k v -> k -> IO (Maybe v)
get = getHelper' c_tcmapget unMap

move :: (Storable k) => Map k v -> k -> Bool -> IO Bool
move m key hd =
    withForeignPtr (unMap m) $ \m' ->
        withPtrLen key $ \(kbuf, ksiz) ->
            c_tcmapmove m' kbuf ksiz hd

iterinit :: Map k v -> IO ()
iterinit m = withForeignPtr (unMap m) c_tcmapiterinit

iternext :: (Storable k) => Map k v -> IO (Maybe k)
iternext m =
    withForeignPtr (unMap m) $ \p ->
        alloca $ \sizbuf -> do
            vbuf <- c_tcmapiternext p sizbuf
            flip maybePeek vbuf $ \vp ->
                do siz <- peek sizbuf
                   buf <- mallocBytes (fromIntegral siz)
                   copyBytes buf vp (fromIntegral siz)
                   peekPtrLen (buf, siz)

rnum :: Map k v -> IO Word64
rnum m = withForeignPtr (unMap m) c_tcmaprnum

msiz :: Map k v -> IO Word64
msiz m = withForeignPtr (unMap m) c_tcmapmsiz

keys :: (Storable k) => Map k v -> IO [k]
keys m = withForeignPtr (unMap m) $ (>>= peekList') . c_tcmapkeys

vals :: (Storable v) => Map k v -> IO [v]
vals m = withForeignPtr (unMap m) $ (>>= peekList') . c_tcmapvals

addint :: (Storable k) => Map k v -> k -> Int -> IO (Maybe Int)
addint = addHelper c_tcmapaddint unMap fromIntegral fromIntegral (== cINT_MIN)

adddouble :: (Storable k) => Map k v -> k -> Double -> IO (Maybe Double)
adddouble = addHelper c_tcmapadddouble unMap realToFrac realToFrac isNaN

clear :: Map k v -> IO ()
clear m = withForeignPtr (unMap m) c_tcmapclear

cutfront :: Map k v -> Int -> IO ()
cutfront m num = withForeignPtr (unMap m) $ flip c_tcmapcutfront (fromIntegral num)

dump :: Map k v -> IO ByteString
dump m =
    withForeignPtr (unMap m) $ \m' ->
        alloca $ \sizbuf -> do
            buf <- c_tcmapdump m' sizbuf
            size <- fromIntegral `fmap` peek sizbuf
            unsafePackCStringFinalizer (castPtr buf) size (free buf)

load :: ByteString -> IO (Map k v)
load bytes =
    unsafeUseAsCStringLen bytes $ \(buf, siz) -> do
      m <- c_tcmapload (castPtr buf) (fromIntegral siz)
      Map `fmap` newForeignPtr tcmapFinalizer m
