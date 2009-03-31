module Database.TokyoCabinet.Map
    (
      Associative
    , AssociativeList(..)
    , withMap
    , peekMap
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

import Foreign.Ptr
import Foreign.ForeignPtr

import Data.ByteString (ByteString)

data Map = Map { unMap :: !(ForeignPtr MAP) }

class Associative a where
    toList   :: (Storable k, Storable v) => a k v -> [(k, v)]
    fromList :: (Storable k, Storable v) => [(k, v)] -> a k v

newtype AssociativeList k v = AL [(k, v)] deriving (Eq, Ord, Show)

instance Associative AssociativeList where
    toList (AL xs) = xs
    fromList = AL

withMap :: (Storable k, Storable v, Associative a) => a k v -> (Ptr MAP -> IO b) -> IO b
withMap amap action = do m <- new
                         mapM_ (uncurry $ put m) (toList amap)
                         result <- withForeignPtr (unMap m) action
                         delete m
                         return result

peekMap :: (Storable k, Storable v, Associative a) => Ptr MAP -> IO (a k v)
peekMap = undefined

new :: IO Map
new = Map `fmap` (c_tcmapnew >>= newForeignPtr tcmapFinalizer)

new2 :: Word32 -> IO Map
new2 num = Map `fmap` (c_tcmapnew2 num >>= newForeignPtr tcmapFinalizer)

dup :: Map -> IO Map
dup m =
    withForeignPtr (unMap m) $ \m' ->
        Map `fmap` (c_tcmapdup m' >>= newForeignPtr tcmapFinalizer)

delete :: Map -> IO ()
delete m = finalizeForeignPtr (unMap m)

put :: (Storable k, Storable v) => Map -> k -> v -> IO ()
put = putHelper c_tcmapput unMap

putkeep :: (Storable k, Storable v) => Map -> k -> v -> IO Bool
putkeep = putHelper c_tcmapputkeep unMap

putcat :: (Storable k, Storable v) => Map -> k -> v -> IO ()
putcat = putHelper c_tcmapputcat unMap

out :: (Storable k) => Map -> k -> IO Bool
out = outHelper c_tcmapout unMap

get :: (Storable k, Storable v) => Map -> k -> IO (Maybe v)
get = getHelper' c_tcmapget unMap

move :: (Storable k) => Map -> k -> Bool -> IO Bool
move = undefined

iterinit :: Map -> IO ()
iterinit m = withForeignPtr (unMap m) c_tcmapiterinit

iternext :: (Storable k) => Map -> IO (Maybe k)
iternext = undefined

rnum :: Map -> IO Word64
rnum m = withForeignPtr (unMap m) c_tcmaprnum

msiz :: Map -> IO Word64
msiz m = withForeignPtr (unMap m) c_tcmapmsiz

keys :: (Storable k) => Map -> IO [k]
keys = undefined

vals :: (Storable v) => Map -> IO [v]
vals = undefined

addint :: (Storable k) => Map -> k -> Int -> IO (Maybe Int)
addint = addHelper c_tcmapaddint unMap fromIntegral fromIntegral (== cINT_MIN)

adddouble :: (Storable k) => Map -> k -> Double -> IO (Maybe Double)
adddouble = addHelper c_tcmapadddouble unMap realToFrac realToFrac isNaN

clear :: Map -> IO ()
clear m = withForeignPtr (unMap m) c_tcmapclear

cutfront :: Map -> Int -> IO ()
cutfront = undefined

dump :: Map -> Int -> IO ByteString
dump = undefined

load :: ByteString -> IO Map
load = undefined
