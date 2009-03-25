-- | Interface to Hash based DBM. See also,
-- <http://tokyocabinet.sourceforge.net/spex-en.html#tchdbapi> for details
module Database.TokyoCabinet.HDB
    (
    -- * Constructors
      TCHDB
    , TCECODE(..)
    , OpenMode(..)
    , TuningOption(..)
    -- * Basic API (tokyocabinet.idl compliant)
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
    , putasync
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
    )
    where

import Foreign.Storable (peek)
import Foreign.ForeignPtr
import Foreign.Marshal (alloca)
import Foreign.Marshal.Utils (maybePeek)

import Data.Int

import Database.TokyoCabinet.HDB.C
import Database.TokyoCabinet.Error
import Database.TokyoCabinet.Internal
import qualified Database.TokyoCabinet.Storable as S

data TCHDB = TCHDB { unTCHDB :: !(ForeignPtr HDB) }

-- | Create a Hash database object. 
new :: IO TCHDB
new = TCHDB `fmap` (c_tchdbnew >>= newForeignPtr tchdbFinalizer)

-- | Force to free region of HDB. 
-- HDB is kept by ForeignPtr, so Haskell runtime GC cleans up memory for
-- almost situation. Most always, you don't need to call this. 
-- After call this, you must not touch HDB object. Its behavior is undefined.
delete :: TCHDB -> IO ()
delete hdb = finalizeForeignPtr (unTCHDB hdb)

-- | Return the last happened error code.
ecode :: TCHDB -> IO TCECODE
ecode hdb = cintToError `fmap` withForeignPtr (unTCHDB hdb) c_tchdbecode

-- | Set the tuning parameters.
tune :: TCHDB -- ^ TCHDB object
     -> Int64 -- ^ the number of elements of the bucket array.
     -> Int8  -- ^ the size of record alignment by power of 2. 
     -> Int8  -- ^ the maximum number of elements of the free block
              -- pool by power of 2.
     -> [TuningOption] -- ^ tuning options.
     -> IO Bool -- ^ if successful, the return value is True.
tune hdb bnum apow fpow options =
    withForeignPtr (unTCHDB hdb) $ \p ->
        c_tchdbtune p bnum apow fpow (combineTuningOption options)

-- | Set the caching parameters.
setcache :: TCHDB   -- ^ TCHDB object.
         -> Int32   -- ^ the maximum number of records to be cached.
         -> IO Bool -- ^ if successful, the return value is True.
setcache hdb rcnum = withForeignPtr (unTCHDB hdb) (flip c_tchdbsetcache rcnum)

-- | Set the size of extra mapped memory.
setxmsiz :: TCHDB -> Int64 -> IO Bool
setxmsiz hdb xmsiz = withForeignPtr (unTCHDB hdb) (flip c_tchdbsetxmsiz xmsiz)

-- | Open a database file.
open :: TCHDB -> String -> [OpenMode] -> IO Bool
open = openHelper c_tchdbopen unTCHDB combineOpenMode

-- | Close the database file.
close :: TCHDB -> IO Bool
close hdb = withForeignPtr (unTCHDB hdb) c_tchdbclose

-- | Stora a record (key-value pair) on HDB.  Key and value type must
-- be instance of Storable class.  Usually, we can use `String',
-- `ByteString' for key, `String', `ByteString', `Int', `Double' for
-- value.
put :: (S.Storable a, S.Storable b) => TCHDB -> a -> b -> IO Bool
put = putHelper c_tchdbput unTCHDB

-- | Store a new record. If a record with the same key exists in the
-- database, this function has no effect.
putkeep :: (S.Storable a, S.Storable b) => TCHDB -> a -> b -> IO Bool
putkeep = putHelper c_tchdbputkeep unTCHDB

-- | Concatenate a value at the end of the existing record.
putcat :: (S.Storable a, S.Storable b) => TCHDB -> a -> b -> IO Bool
putcat = putHelper c_tchdbputcat unTCHDB

-- | Store a record into a hash database object in asynchronous fashion.
putasync :: (S.Storable a, S.Storable b) => TCHDB -> a -> b -> IO Bool
putasync = putHelper c_tchdbputasync unTCHDB

-- | Delete a record.
out :: (S.Storable a) => TCHDB -> a -> IO Bool
out = outHelper c_tchdbout unTCHDB

-- | Return the value of record. 
get :: (S.Storable a, S.Storable b) => TCHDB -> a -> IO (Maybe b)
get = getHelper c_tchdbget unTCHDB

-- | Return the byte size of value in a record.
vsiz :: (S.Storable a) => TCHDB -> a -> IO (Maybe Int)
vsiz = vsizHelper c_tchdbvsiz unTCHDB

-- | Initialize the iterator of a TCHDB object.
iterinit :: TCHDB -> IO Bool
iterinit hdb = withForeignPtr (unTCHDB hdb) c_tchdbiterinit

-- | Return the next key of the iterator of a TCHDB object.
iternext :: (S.Storable a) => TCHDB -> IO (Maybe a)
iternext hdb =
    withForeignPtr (unTCHDB hdb) $ \p ->
        alloca $ \sizbuf -> do
            vbuf <- c_tchdbiternext p sizbuf
            flip maybePeek vbuf $ \vp ->
                do siz <- peek sizbuf
                   S.peekPtrLen (vp, siz)

-- | Return list of forward matched keys.
fwmkeys :: (S.Storable a, S.Storable b) => TCHDB -> a -> Int -> IO [b]
fwmkeys = fwmHelper c_tchdbfwmkeys unTCHDB

-- | Increment the corresponding value. (The value specified by a key
-- is treated as integer.)
addint :: (S.Storable a) => TCHDB -> a -> Int -> IO (Maybe Int)
addint = addHelper c_tchdbaddint unTCHDB fromIntegral fromIntegral (== cINT_MIN)

-- | Increment the corresponding value. (The value specified by a key
-- is treated as double.)
adddouble :: (S.Storable a) => TCHDB -> a -> Double -> IO (Maybe Double)
adddouble = addHelper c_tchdbadddouble unTCHDB realToFrac realToFrac isNaN

-- | Synchronize updated contents of a database object with the file
-- and the device.
sync :: TCHDB -> IO Bool
sync hdb = withForeignPtr (unTCHDB hdb) c_tchdbsync

-- |  Optimize the file of a Hash database object.
optimize :: TCHDB -- ^ TCHDB object
         -> Int64 -- ^ the number of elements of the bucket array.
         -> Int8  -- ^ the size of record alignment by power of 2. 
         -> Int8  -- ^ the maximum number of elements of the free block
                  -- pool by power of 2.
         -> [TuningOption] -- ^ tuning options.
         -> IO Bool -- ^ if successful, the return value is True.
optimize hdb bnum apow fpow options = 
    withForeignPtr (unTCHDB hdb) $ \p ->
        c_tchdboptimize p bnum apow fpow (combineTuningOption options)

-- | Delete all records.
vanish :: TCHDB -> IO Bool
vanish hdb = withForeignPtr (unTCHDB hdb) c_tchdbvanish

-- | Copy the database file.
copy :: TCHDB -> String -> IO Bool
copy = copyHelper c_tchdbcopy unTCHDB

-- | Begin the transaction.
tranbegin :: TCHDB -> IO Bool
tranbegin hdb = withForeignPtr (unTCHDB hdb) c_tchdbtranbegin

-- | Commit the transaction.
trancommit :: TCHDB -> IO Bool
trancommit hdb = withForeignPtr (unTCHDB hdb) c_tchdbtrancommit

-- | Abort the transaction.
tranabort :: TCHDB -> IO Bool
tranabort hdb = withForeignPtr (unTCHDB hdb) c_tchdbtranabort

-- | Return the file path of currentry opened database.
path :: TCHDB -> IO (Maybe String)
path = pathHelper c_tchdbpath unTCHDB

-- | Return the number of records in the database.
rnum :: TCHDB -> IO Int64
rnum hdb = withForeignPtr (unTCHDB hdb) c_tchdbrnum

-- | Return the size of the database file.
fsiz :: TCHDB -> IO Int64
fsiz hdb = withForeignPtr (unTCHDB hdb) c_tchdbfsiz
