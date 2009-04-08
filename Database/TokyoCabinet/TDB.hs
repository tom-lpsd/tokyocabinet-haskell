module Database.TokyoCabinet.TDB
    (
      TDB
    , ECODE(..)
    , OpenMode(..)
    , TuningOption(..)
    , IndexType(..)
    , AssocList(..)
    , new
    , delete
    , ecode
    , errmsg
    , tune
    , setcache
    , setxmsiz
    , open
    , close
    , put
    , putkeep
    , putcat
    , out
    , get
    , vsiz
    , iterinit
    , iternext
    , fwmkeys
    , addint
    , adddouble
    , sync
    , optimize
    , vanish
    , copy
    , tranbegin
    , trancommit
    , tranabort
    , path
    , rnum
    , fsiz
    , setindex
    , genuid
    ) where

import Database.TokyoCabinet.Map.C
import Database.TokyoCabinet.TDB.C
import Database.TokyoCabinet.Error
import Database.TokyoCabinet.Internal
import Database.TokyoCabinet.Storable
import Database.TokyoCabinet.Associative

import Data.Int
import Data.Word

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String

data TDB = TDB { unTCTDB :: !(ForeignPtr TDB') }

new :: IO TDB
new = TDB `fmap` (c_tctdbnew >>= newForeignPtr tctdbFinalizer)

delete :: TDB -> IO ()
delete tdb = finalizeForeignPtr (unTCTDB tdb)

ecode :: TDB -> IO ECODE
ecode tdb = cintToError `fmap` withForeignPtr (unTCTDB tdb) c_tctdbecode

-- | Set the tuning parameters.
tune :: TDB   -- ^ TDB object
     -> Int64 -- ^ the number of elements of the bucket array
     -> Int8  -- ^ the size of record alignment by power of 2
     -> Int8  -- ^ the maximum number of elements of the free block pool by power of 2
     -> [TuningOption] -- ^ options
     -> IO Bool -- ^ if successful, the return value is True.
tune tdb bnum apow fpow opts =
    withForeignPtr (unTCTDB tdb) $ \tdb' ->
        c_tctdbtune tdb' bnum apow fpow (combineTuningOption opts)

-- | Set the caching parameters of a table database object.
setcache :: TDB   -- ^ TDB object
         -> Int32 -- ^ the maximum number of records to be cached
         -> Int32 -- ^ the maximum number of leaf nodes to be cached
         -> Int32 -- ^ the maximum number of non-leaf nodes to be cached
         -> IO Bool -- ^ if successful, the return value is True.
setcache tdb rcnum lcnum ncnum =
    withForeignPtr (unTCTDB tdb) $ \tdb' ->
        c_tctdbsetcache tdb' rcnum lcnum ncnum

-- | Set the size of the extra mapped memory of a table database object.
setxmsiz :: TDB     -- ^ TDB object
         -> Int64   -- ^ the size of the extra mapped memory
         -> IO Bool -- ^ if successful, the return value is True.
setxmsiz tdb xmsiz = withForeignPtr (unTCTDB tdb) (flip c_tctdbsetxmsiz xmsiz)

open :: TDB -> String -> [OpenMode] -> IO Bool
open = openHelper c_tctdbopen unTCTDB combineOpenMode

close :: TDB -> IO Bool
close tdb = withForeignPtr (unTCTDB tdb) c_tctdbclose

type FunPut' = Ptr TDB' -> Ptr Word8 -> CInt -> Ptr MAP -> IO Bool
putHelper' :: (Storable k, Storable v, Associative m) =>
              FunPut' -> TDB -> v -> m k v -> IO Bool
putHelper' c_putfunc tdb key vals =
    withForeignPtr (unTCTDB tdb) $ \tdb' ->
        withPtrLen key $ \(kbuf, ksize) ->
            withMap vals $ c_putfunc tdb' kbuf ksize

put :: (Storable k, Storable v, Associative m) => TDB -> v -> m k v -> IO Bool
put = putHelper' c_tctdbput

putkeep :: (Storable k, Storable v, Associative m) => TDB -> v -> m k v -> IO Bool
putkeep = putHelper' c_tctdbputkeep

putcat :: (Storable k, Storable v, Associative m) => TDB -> v -> m k v -> IO Bool
putcat = putHelper' c_tctdbputcat

out :: (Storable k) => TDB -> k -> IO Bool
out = outHelper c_tctdbout unTCTDB

get :: (Storable k, Storable v, Associative m) => TDB -> k -> IO (m k v)
get tdb key =
    withForeignPtr (unTCTDB tdb) $ \tdb' ->
        withPtrLen key $ \(kbuf, ksize) ->
            c_tctdbget tdb' kbuf ksize >>= peekMap'

vsiz :: (Storable k) => TDB -> k -> IO (Maybe Int)
vsiz = vsizHelper c_tctdbvsiz unTCTDB

iterinit :: TDB -> IO Bool
iterinit tdb = withForeignPtr (unTCTDB tdb) c_tctdbiterinit

iternext :: (Storable k) => TDB -> IO (Maybe k)
iternext = iternextHelper c_tctdbiternext unTCTDB

fwmkeys :: (Storable k1, Storable k2) => TDB -> k1 -> Int -> IO [k2]
fwmkeys = fwmHelper c_tctdbfwmkeys unTCTDB

addint :: (Storable k) => TDB -> k -> Int -> IO (Maybe Int)
addint = addHelper c_tctdbaddint unTCTDB fromIntegral fromIntegral (== cINT_MIN)

adddouble :: (Storable k) => TDB -> k -> Double -> IO (Maybe Double)
adddouble = addHelper c_tctdbadddouble unTCTDB realToFrac realToFrac isNaN

sync :: TDB -> IO Bool
sync tdb = withForeignPtr (unTCTDB tdb) c_tctdbsync

optimize :: TDB   -- ^ TDB object                                                         
         -> Int64 -- ^ the number of elements of the bucket array                         
         -> Int8  -- ^ the size of record alignment by power of 2                         
         -> Int8  -- ^ the maximum number of elements of the free block pool by power of 2
         -> [TuningOption] -- ^ options
         -> IO Bool -- ^ if successful, the return value is True.
optimize tdb bnum apow fpow opts =
    withForeignPtr (unTCTDB tdb) $ \tdb' ->
        c_tctdboptimize tdb' bnum apow fpow (combineTuningOption opts)

vanish :: TDB -> IO Bool
vanish tdb = withForeignPtr (unTCTDB tdb) c_tctdbvanish

copy :: TDB -> String -> IO Bool
copy = copyHelper c_tctdbcopy unTCTDB

tranbegin :: TDB -> IO Bool
tranbegin tdb = withForeignPtr (unTCTDB tdb) c_tctdbtranbegin

trancommit :: TDB -> IO Bool
trancommit tdb = withForeignPtr (unTCTDB tdb) c_tctdbtrancommit

tranabort :: TDB -> IO Bool
tranabort tdb = withForeignPtr (unTCTDB tdb) c_tctdbtranabort

path :: TDB -> IO (Maybe String)
path = pathHelper c_tctdbpath unTCTDB

rnum :: TDB -> IO Word64
rnum tdb = withForeignPtr (unTCTDB tdb) c_tctdbrnum

fsiz :: TDB -> IO Word64
fsiz tdb = withForeignPtr (unTCTDB tdb) c_tctdbfsiz

setindex :: TDB -> String -> IndexType -> IO Bool
setindex tdb name itype =
    withForeignPtr (unTCTDB tdb) $ \tdb' ->
        withCString name $ \c_name ->
            c_tctdbsetindex tdb' c_name (indexTypeToCInt itype)

genuid :: TDB -> IO (Maybe Int64)
genuid tdb = withForeignPtr (unTCTDB tdb) $ \tdb' -> do
               uid <- c_tctdbgenuid tdb'
               return $ if uid == (-1) then Nothing else Just uid
