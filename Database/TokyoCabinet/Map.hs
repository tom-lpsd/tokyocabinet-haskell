module Database.TokyoCabinet.Map
    (
      Map
    , new
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
    ) where

import Prelude hiding (lookup)
import qualified Prelude as P
import Database.TokyoCabinet.Map.C
import Database.TokyoCabinet.Storable
import Database.TokyoCabinet.Internal
import Database.TokyoCabinet.Error (cINT_MIN)

import Data.Word

import Foreign.ForeignPtr
import Foreign.Storable (peek)
import Foreign.Marshal (alloca, mallocBytes)
import Foreign.Marshal.Utils (maybePeek, copyBytes)

import Data.ByteString (ByteString)

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
move = undefined

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
keys = undefined

vals :: (Storable v) => Map k v -> IO [v]
vals = undefined

addint :: (Storable k) => Map k v -> k -> Int -> IO (Maybe Int)
addint = addHelper c_tcmapaddint unMap fromIntegral fromIntegral (== cINT_MIN)

adddouble :: (Storable k) => Map k v -> k -> Double -> IO (Maybe Double)
adddouble = addHelper c_tcmapadddouble unMap realToFrac realToFrac isNaN

clear :: Map k v -> IO ()
clear m = withForeignPtr (unMap m) c_tcmapclear

cutfront :: Map k v -> Int -> IO ()
cutfront = undefined

dump :: Map k v -> Int -> IO ByteString
dump = undefined

load :: ByteString -> IO (Map k v)
load = undefined
