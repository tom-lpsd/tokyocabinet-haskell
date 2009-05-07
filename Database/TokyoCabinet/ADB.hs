module Database.TokyoCabinet.ADB
    (
      ADB
    , ECODE(..)
    , new
    , delete
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
    , size
    , misc
    ) where

import Data.Word

import Foreign.C.String
import Foreign.ForeignPtr

import Database.TokyoCabinet.ADB.C
import Database.TokyoCabinet.Error
import Database.TokyoCabinet.Internal
import Database.TokyoCabinet.Storable
import Database.TokyoCabinet.Sequence

data ADB = ADB { unTCADB :: !(ForeignPtr ADB') }

-- | Create a Abstract database object. 
new :: IO ADB
new = ADB `fmap` (c_tcadbnew >>= newForeignPtr tcadbFinalizer)

-- | Free ADB resource forcibly. 
-- HDB is kept by ForeignPtr, so Haskell runtime GC cleans up memory for
-- almost situation. Most always, you don't need to call this. 
-- After call this, you must not touch ADB object. Its behavior is undefined.
delete :: ADB -> IO ()
delete adb = finalizeForeignPtr (unTCADB adb)

-- | Open an abstract dataabse.
open :: ADB -> String -> IO Bool
open adb name = withForeignPtr (unTCADB adb) $ (withCString name) . c_tcadbopen

-- | Close an abstract database object.
close :: ADB -> IO Bool
close adb = withForeignPtr (unTCADB adb) c_tcadbclose

-- | Stora a record into an abstract database object. 
put :: (Storable k, Storable v) => ADB -> k -> v -> IO Bool
put = putHelper c_tcadbput unTCADB

-- | Store a new record into an abstract database object.
putkeep :: (Storable k, Storable v) => ADB -> k -> v -> IO Bool
putkeep = putHelper c_tcadbputkeep unTCADB

-- | Concatenate a value at the end of the existing record in an
-- abstract database object.
putcat :: (Storable k, Storable v) => ADB -> k -> v -> IO Bool
putcat = putHelper c_tcadbputcat unTCADB

-- | Remove a record of an abstract database object.
out :: (Storable k) => ADB -> k -> IO Bool
out = outHelper c_tcadbout unTCADB

-- | Retrieve a record in an abstract database object.
get :: (Storable k, Storable v) => ADB -> k -> IO (Maybe v)
get = getHelper c_tcadbget unTCADB

-- | Get the size of the value of a record in an abstract database object.
vsiz :: (Storable k) => ADB -> k -> IO (Maybe Int)
vsiz = vsizHelper c_tcadbvsiz unTCADB

-- | Initialize the iterator of an abstract database object.
iterinit :: ADB -> IO Bool
iterinit adb = withForeignPtr (unTCADB adb) c_tcadbiterinit

-- | Get the next key of the iterator of an abstract database object.
iternext :: (Storable k) => ADB -> IO (Maybe k)
iternext = iternextHelper c_tcadbiternext unTCADB

-- | Get forward matching keys in an abstract database object.
fwmkeys :: (Storable k1, Storable k2, Sequence q) =>
           ADB -> k1 -> Int -> IO (q k2)
fwmkeys = fwmHelper c_tcadbfwmkeys unTCADB

-- | Add an integer to a record in an abstract database object.
addint :: (Storable k) => ADB -> k -> Int -> IO (Maybe Int)
addint = addHelper c_tcadbaddint unTCADB fromIntegral fromIntegral (== cINT_MIN)

-- | Add a real number to a record in an abstract database object.
adddouble :: (Storable k) => ADB -> k -> Double -> IO (Maybe Double)
adddouble = addHelper c_tcadbadddouble unTCADB realToFrac realToFrac isNaN

-- | Synchronize updated contents of an abstract database object with
-- the file and the device.
sync :: ADB -> IO Bool
sync adb = withForeignPtr (unTCADB adb) c_tcadbsync

-- | Optimize the storage of an abstract database object.
optimize :: ADB -> String -> IO Bool
optimize adb params =
    withForeignPtr (unTCADB adb) $ (withCString params) . c_tcadboptimize

-- | Remove all records of an abstract database object.
vanish :: ADB -> IO Bool
vanish adb = withForeignPtr (unTCADB adb) c_tcadbvanish

-- | Copy the database file of an abstract database object.
copy :: ADB -> String -> IO Bool
copy = copyHelper c_tcadbcopy unTCADB

-- | Begin the transaction of an abstract database object.
tranbegin :: ADB -> IO Bool
tranbegin adb = withForeignPtr (unTCADB adb) c_tcadbtranbegin

-- | Commit the transaction of an abstract database object.
trancommit :: ADB -> IO Bool
trancommit adb = withForeignPtr (unTCADB adb) c_tcadbtrancommit

-- | Abort the transaction of an abstract database object.
tranabort :: ADB -> IO Bool
tranabort adb = withForeignPtr (unTCADB adb) c_tcadbtranabort

-- | Get the file path of an abstract database object.
path :: ADB -> IO (Maybe String)
path = pathHelper c_tcadbpath unTCADB

-- | Get the number of records of an abstract database object.
rnum :: ADB -> IO Word64
rnum adb = withForeignPtr (unTCADB adb) c_tcadbrnum

-- | Get the size of the database of an abstract database object.
size :: ADB -> IO Word64
size adb = withForeignPtr (unTCADB adb) c_tcadbsize

-- | Call a versatile function for miscellaneous operations of an
-- abstract database object.
misc :: (Storable a, Storable b, Sequence q1, Sequence q2) =>
        ADB -> String -> q1 a -> IO (q2 b)
misc adb name args =
    withForeignPtr (unTCADB adb) $ \adb' ->
        withCString name $ \name' ->
            withList args $ \args' -> do
              ret <- c_tcadbmisc adb' name' args'
              peekList' ret
