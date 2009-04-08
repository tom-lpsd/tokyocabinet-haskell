-- | Interface to Hash based DBM. See also,
-- <http://tokyocabinet.sourceforge.net/spex-en.html#tchdbapi> for details
module Database.TokyoCabinet.HDB
    (
    -- $doc
    -- * Constructors
      HDB
    , ECODE(..)
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


import Foreign.ForeignPtr

import Data.Int
import Data.Word

import Database.TokyoCabinet.HDB.C
import Database.TokyoCabinet.Error
import Database.TokyoCabinet.Internal
import Database.TokyoCabinet.Sequence
import Database.TokyoCabinet.Storable

-- $doc
-- Example
--
-- @
--    import Control.Monad
--    import Database.TokyoCabinet.HDB
-- @
--
-- @
--    main = do hdb <- new
--              -- open the database
--              open hdb \"casket.tch\" [OWRITER, OCREAT] >>= err hdb
--              -- store records
--              puts hdb [(\"foo\", \"hop\"), (\"bar\", \"step\"), (\"baz\", \"jump\")] >>=
--                       err hdb . (all id)
--              -- retrieve records
--              get_print hdb \"foo\"
--              -- traverse records
--              iterinit hdb
--              iter hdb >>= mapM_ (\k -> putStr (k++\":\") >> get_print hdb k)
--              -- close the database
--              close hdb >>= err hdb
--        where
--          puts :: HDB -> [(String, String)] -> IO [Bool]
--          puts hdb = mapM (uncurry $ put hdb)
-- @
--
-- @
--          get_print :: HDB -> String -> IO ()
--          get_print hdb key = get hdb key >>=
--                              maybe (error \"something goes wrong\") putStrLn
-- @
--
-- @  
--          err :: HDB -> Bool -> IO ()
--          err hdb = flip unless $ ecode hdb >>= error . show
-- @
--
-- @    
--          iter :: HDB -> IO [String]
--          iter hdb = iternext hdb >>=
--                     maybe (return []) (\x -> return . (x:) =<< iter hdb)
-- @
--

data HDB = HDB { unTCHDB :: !(ForeignPtr HDB') }

-- | Create a Hash database object. 
new :: IO HDB
new = HDB `fmap` (c_tchdbnew >>= newForeignPtr tchdbFinalizer)

-- | Free HDB resource forcibly. 
-- HDB is kept by ForeignPtr, so Haskell runtime GC cleans up memory for
-- almost situation. Most always, you don't need to call this. 
-- After call this, you must not touch HDB object. Its behavior is undefined.
delete :: HDB -> IO ()
delete hdb = finalizeForeignPtr (unTCHDB hdb)

-- | Return the last happened error code.
ecode :: HDB -> IO ECODE
ecode hdb = cintToError `fmap` withForeignPtr (unTCHDB hdb) c_tchdbecode

-- | Set the tuning parameters.
tune :: HDB   -- ^ HDB object
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
setcache :: HDB   -- ^ HDB object.
         -> Int32   -- ^ the maximum number of records to be cached.
         -> IO Bool -- ^ if successful, the return value is True.
setcache hdb rcnum = withForeignPtr (unTCHDB hdb) (flip c_tchdbsetcache rcnum)

-- | Set the size of extra mapped memory.
setxmsiz :: HDB -> Int64 -> IO Bool
setxmsiz hdb xmsiz = withForeignPtr (unTCHDB hdb) (flip c_tchdbsetxmsiz xmsiz)

-- | Open a database file.
open :: HDB -> String -> [OpenMode] -> IO Bool
open = openHelper c_tchdbopen unTCHDB combineOpenMode

-- | Close the database file.
close :: HDB -> IO Bool
close hdb = withForeignPtr (unTCHDB hdb) c_tchdbclose

-- | Stora a record (key-value pair) on HDB.  Key and value type must
-- be instance of Storable class.  Usually, we can use `String',
-- `ByteString' for key, `String', `ByteString', `Int', `Double' for
-- value.
put :: (Storable k, Storable v) => HDB -> k -> v -> IO Bool
put = putHelper c_tchdbput unTCHDB

-- | Store a new record. If a record with the same key exists in the
-- database, this function has no effect.
putkeep :: (Storable k, Storable v) => HDB -> k -> v -> IO Bool
putkeep = putHelper c_tchdbputkeep unTCHDB

-- | Concatenate a value at the end of the existing record.
putcat :: (Storable k, Storable v) => HDB -> k -> v -> IO Bool
putcat = putHelper c_tchdbputcat unTCHDB

-- | Store a record into a hash database object in asynchronous fashion.
putasync :: (Storable k, Storable v) => HDB -> k -> v -> IO Bool
putasync = putHelper c_tchdbputasync unTCHDB

-- | Delete a record.
out :: (Storable k) => HDB -> k -> IO Bool
out = outHelper c_tchdbout unTCHDB

-- | Return the value of record. 
get :: (Storable k, Storable v) => HDB -> k -> IO (Maybe v)
get = getHelper c_tchdbget unTCHDB

-- | Return the byte size of value in a record.
vsiz :: (Storable k) => HDB -> k -> IO (Maybe Int)
vsiz = vsizHelper c_tchdbvsiz unTCHDB

-- | Initialize the iterator of a HDB object.
iterinit :: HDB -> IO Bool
iterinit hdb = withForeignPtr (unTCHDB hdb) c_tchdbiterinit

-- | Return the next key of the iterator of a HDB object.
iternext :: (Storable k) => HDB -> IO (Maybe k)
iternext = iternextHelper c_tchdbiternext unTCHDB

-- | Return list of forward matched keys.
fwmkeys :: (Storable k1, Storable k2, Sequence q) =>
           HDB -> k1 -> Int -> IO (q k2)
fwmkeys = fwmHelper c_tchdbfwmkeys unTCHDB

-- | Increment the corresponding value. (The value specified by a key
-- is treated as integer.)
addint :: (Storable k) => HDB -> k -> Int -> IO (Maybe Int)
addint = addHelper c_tchdbaddint unTCHDB fromIntegral fromIntegral (== cINT_MIN)

-- | Increment the corresponding value. (The value specified by a key
-- is treated as double.)
adddouble :: (Storable k) => HDB -> k -> Double -> IO (Maybe Double)
adddouble = addHelper c_tchdbadddouble unTCHDB realToFrac realToFrac isNaN

-- | Synchronize updated contents of a database object with the file
-- and the device.
sync :: HDB -> IO Bool
sync hdb = withForeignPtr (unTCHDB hdb) c_tchdbsync

-- |  Optimize the file of a Hash database object.
optimize :: HDB   -- ^ HDB object
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
vanish :: HDB -> IO Bool
vanish hdb = withForeignPtr (unTCHDB hdb) c_tchdbvanish

-- | Copy the database file.
copy :: HDB -> String -> IO Bool
copy = copyHelper c_tchdbcopy unTCHDB

-- | Begin the transaction.
tranbegin :: HDB -> IO Bool
tranbegin hdb = withForeignPtr (unTCHDB hdb) c_tchdbtranbegin

-- | Commit the transaction.
trancommit :: HDB -> IO Bool
trancommit hdb = withForeignPtr (unTCHDB hdb) c_tchdbtrancommit

-- | Abort the transaction.
tranabort :: HDB -> IO Bool
tranabort hdb = withForeignPtr (unTCHDB hdb) c_tchdbtranabort

-- | Return the file path of currentry opened database.
path :: HDB -> IO (Maybe String)
path = pathHelper c_tchdbpath unTCHDB

-- | Return the number of records in the database.
rnum :: HDB -> IO Word64
rnum hdb = withForeignPtr (unTCHDB hdb) c_tchdbrnum

-- | Return the size of the database file.
fsiz :: HDB -> IO Word64
fsiz hdb = withForeignPtr (unTCHDB hdb) c_tchdbfsiz
