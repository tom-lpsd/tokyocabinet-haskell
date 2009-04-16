-- | Interface to the table database. See also,
-- <http://tokyocabinet.sourceforge.net/spex-en.html#tctdbapi> for details
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

-- | Create the new table database object.
new :: IO TDB
new = TDB `fmap` (c_tctdbnew >>= newForeignPtr tctdbFinalizer)

-- | Free object resource forcibly.
delete :: TDB -> IO ()
delete tdb = finalizeForeignPtr (unTCTDB tdb)

-- | Get the last happened error code.
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

-- | Open the table database file
open :: TDB -> String -> [OpenMode] -> IO Bool
open = openHelper c_tctdbopen unTCTDB combineOpenMode

-- | Open the database file
close :: TDB -> IO Bool
close tdb = withForeignPtr (unTCTDB tdb) c_tctdbclose

type FunPut' = Ptr TDB' -> Ptr Word8 -> CInt -> Ptr MAP -> IO Bool
putHelper' :: (Storable k, Storable v, Associative m) =>
              FunPut' -> TDB -> v -> m k v -> IO Bool
putHelper' c_putfunc tdb key vals =
    withForeignPtr (unTCTDB tdb) $ \tdb' ->
        withPtrLen key $ \(kbuf, ksize) ->
            withMap vals $ c_putfunc tdb' kbuf ksize

-- | Store a record into a table database object.
put :: (Storable k, Storable v, Associative m) => TDB -> v -> m k v -> IO Bool
put = putHelper' c_tctdbput

-- | Store a new record into a table database object.
putkeep :: (Storable k, Storable v, Associative m) => TDB -> v -> m k v -> IO Bool
putkeep = putHelper' c_tctdbputkeep

-- | Concatenate columns of the existing record in a table database object.
putcat :: (Storable k, Storable v, Associative m) => TDB -> v -> m k v -> IO Bool
putcat = putHelper' c_tctdbputcat

-- | Remove a record of a table database object.
out :: (Storable k) => TDB -> k -> IO Bool
out = outHelper c_tctdbout unTCTDB

-- | Retrieve a record in a table database object.
get :: (Storable k, Storable v, Associative m) => TDB -> k -> IO (m k v)
get tdb key =
    withForeignPtr (unTCTDB tdb) $ \tdb' ->
        withPtrLen key $ \(kbuf, ksize) ->
            c_tctdbget tdb' kbuf ksize >>= peekMap'

-- | Get the size of the value of a record in a table database object.
vsiz :: (Storable k) => TDB -> k -> IO (Maybe Int)
vsiz = vsizHelper c_tctdbvsiz unTCTDB

-- | Initialize the iterator of a table database object.
iterinit :: TDB -> IO Bool
iterinit tdb = withForeignPtr (unTCTDB tdb) c_tctdbiterinit

-- | Get the next primary key of the iterator of a table database object.
iternext :: (Storable k) => TDB -> IO (Maybe k)
iternext = iternextHelper c_tctdbiternext unTCTDB

-- | Get forward matching primary keys in a table database object.
fwmkeys :: (Storable k1, Storable k2) => TDB -> k1 -> Int -> IO [k2]
fwmkeys = fwmHelper c_tctdbfwmkeys unTCTDB

-- | Add an integer to a column of a record in a table database object.
addint :: (Storable k) => TDB -> k -> Int -> IO (Maybe Int)
addint = addHelper c_tctdbaddint unTCTDB fromIntegral fromIntegral (== cINT_MIN)

-- | Add a real number to a column of a record in a table database object.
adddouble :: (Storable k) => TDB -> k -> Double -> IO (Maybe Double)
adddouble = addHelper c_tctdbadddouble unTCTDB realToFrac realToFrac isNaN

-- | Synchronize updated contents of a table database object with the
-- file and the device.
sync :: TDB -> IO Bool
sync tdb = withForeignPtr (unTCTDB tdb) c_tctdbsync

-- | Optimize the file of a table database object.
optimize :: TDB   -- ^ TDB object                                                         
         -> Int64 -- ^ the number of elements of the bucket array                         
         -> Int8  -- ^ the size of record alignment by power of 2                         
         -> Int8  -- ^ the maximum number of elements of the free block pool by power of 2
         -> [TuningOption] -- ^ options
         -> IO Bool -- ^ if successful, the return value is True.
optimize tdb bnum apow fpow opts =
    withForeignPtr (unTCTDB tdb) $ \tdb' ->
        c_tctdboptimize tdb' bnum apow fpow (combineTuningOption opts)

-- | Remove all records of a table database object.
vanish :: TDB -> IO Bool
vanish tdb = withForeignPtr (unTCTDB tdb) c_tctdbvanish

-- | Copy the database file of a table database object.
copy :: TDB     -- ^ TDB object
     -> String  -- ^ new file path
     -> IO Bool -- ^ if successful, the return value is True
copy = copyHelper c_tctdbcopy unTCTDB

-- | Begin the transaction of a table database object.
tranbegin :: TDB -> IO Bool
tranbegin tdb = withForeignPtr (unTCTDB tdb) c_tctdbtranbegin

-- | Commit the transaction of a table database object.
trancommit :: TDB -> IO Bool
trancommit tdb = withForeignPtr (unTCTDB tdb) c_tctdbtrancommit

-- | Abort the transaction of a table database object.
tranabort :: TDB -> IO Bool
tranabort tdb = withForeignPtr (unTCTDB tdb) c_tctdbtranabort

-- | Get the file path of a table database object.
path :: TDB -> IO (Maybe String)
path = pathHelper c_tctdbpath unTCTDB

-- | Get the number of records of a table database object.
rnum :: TDB -> IO Word64
rnum tdb = withForeignPtr (unTCTDB tdb) c_tctdbrnum

-- | Get the size of the database file of a table database object.
fsiz :: TDB -> IO Word64
fsiz tdb = withForeignPtr (unTCTDB tdb) c_tctdbfsiz

-- | Set a column index to a table database object.
setindex :: TDB -> String -> IndexType -> IO Bool
setindex tdb name itype =
    withForeignPtr (unTCTDB tdb) $ \tdb' ->
        withCString name $ \c_name ->
            c_tctdbsetindex tdb' c_name (indexTypeToCInt itype)

-- | Generate a unique ID number of a table database object.
genuid :: TDB -> IO (Maybe Int64)
genuid tdb = withForeignPtr (unTCTDB tdb) $ \tdb' -> do
               uid <- c_tctdbgenuid tdb'
               return $ if uid == (-1) then Nothing else Just uid
