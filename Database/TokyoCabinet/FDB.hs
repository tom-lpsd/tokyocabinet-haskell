-- | Interface to Fixed-length DBM. See also,
-- <http://tokyocabinet.sourceforge.net/spex-en.html#tcfdbapi> for details
module Database.TokyoCabinet.FDB
    (
     -- * Constructors
      TCFDB
    , TCECODE(..)
    , OpenMode(..)
    , ID(..)
    -- * Basic API (tokyocabinet.idl compliant)
    , new
    , delete
    , ecode
    , errmsg
    , tune
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
    , range
    , fwmkeys
    , addint
    , adddouble
    , sync
    , optimize
    , vanish
    , copy
    , path
    , rnum
    , fsiz
    ) where

import Database.TokyoCabinet.Error
import Database.TokyoCabinet.FDB.C
import Database.TokyoCabinet.FDB.Key
import Database.TokyoCabinet.Internal
import qualified Database.TokyoCabinet.Storable as S

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Storable (peek)
import Foreign.Marshal (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (maybePeek)

import Data.Int
import Data.Word

import Control.Exception

data TCFDB = TCFDB { unTCFDB :: !(ForeignPtr FDB) }

-- | Create a Fixed-length database object. 
new :: IO TCFDB
new = TCFDB `fmap` (c_tcfdbnew >>= newForeignPtr tcfdbFinalizer)

-- | Force to free region of FDB. 
-- FDB is kept by ForeignPtr, so Haskell runtime GC cleans up memory for
-- almost situation. Most always, you don't need to call this. 
-- After call this, you must not touch FDB object. Its behavior is undefined.
delete :: TCFDB -> IO ()
delete fdb = finalizeForeignPtr $ unTCFDB fdb

-- | Return the last happened error code.
ecode :: TCFDB -> IO TCECODE
ecode fdb =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        cintToError `fmap` c_tcfdbecode fdb'

-- | Set the tuning parameters.
tune :: TCFDB   -- ^ TCFDB object.
     -> Int32   -- ^ the width of the value of each record.
     -> Int64   -- ^ the limit size of the database file.
     -> IO Bool -- ^ if successful, the return value is True.
tune fdb width limsiz =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> c_tcfdbtune fdb' width limsiz

-- | Open FDB database file.
open :: TCFDB -> String -> [OpenMode] -> IO Bool
open = openHelper c_tcfdbopen unTCFDB combineOpenMode

-- | Close the database file.
close :: TCFDB -> IO Bool
close fdb = withForeignPtr (unTCFDB fdb) c_tcfdbclose

type PutFunc = Ptr FDB -> Int64 -> Ptr Word8 -> CInt -> IO Bool
liftPutFunc :: (Key k, S.Storable v) => PutFunc -> TCFDB -> k -> v -> IO Bool
liftPutFunc func fdb key val =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        S.withPtrLen val $ \(vbuf, vsize) -> do
          key' <- keyToInt key
          func fdb' key' vbuf vsize

-- | Stora a record (key-value pair) on FDB.  Key type must be
-- instance of Key class. Value type must be instance of Storable.
put :: (Key k, S.Storable v) => TCFDB -> k -> v -> IO Bool
put = liftPutFunc c_tcfdbput

-- | Store a new record. If a record with the same key exists in the
-- database, this function has no effect.
putkeep :: (Key k, S.Storable v) => TCFDB -> k -> v -> IO Bool
putkeep = liftPutFunc c_tcfdbputkeep

-- | Concatenate a value at the end of the existing record.
putcat :: (Key k, S.Storable v) => TCFDB -> k -> v -> IO Bool
putcat =  liftPutFunc c_tcfdbputcat

-- | Delete a record. 
out :: (Key k) => TCFDB -> k -> IO Bool
out fdb key =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        c_tcfdbout fdb' =<< keyToInt key

-- | Return the value of record. 
get :: (Key k, S.Storable v) => TCFDB -> k -> IO (Maybe v)
get fdb key =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        alloca $ \sizbuf -> do
          key' <- keyToInt key
          vbuf  <- c_tcfdbget fdb' key' sizbuf
          vsize <- peek sizbuf
          flip maybePeek vbuf $ \vbuf' -> S.peekPtrLen (vbuf', vsize)

-- | Return the byte size of value in a record.
vsiz :: (Key k) => TCFDB -> k -> IO (Maybe Int)
vsiz fdb key =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      vsize <- c_tcfdbvsiz fdb' =<< keyToInt key
      return $ if vsize == (-1)
                 then Nothing
                 else Just (fromIntegral vsize)

-- | Initialize the iterator of a TCFDB object.
iterinit :: TCFDB -> IO Bool
iterinit fdb = withForeignPtr (unTCFDB fdb) c_tcfdbiterinit

-- | Return the next key of the iterator of a TCFDB object.
iternext :: (Key k) => TCFDB -> IO (Maybe k)
iternext fdb = 
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      i <-  c_tcfdbiternext fdb'
      return $ if i == 0
                 then Nothing
                 else Just (fromID $ ID i)

-- | Return list of keys in the specified range.
range :: (Key k1, Key k2) =>
         TCFDB   -- ^ TCFDB object
      -> k1      -- ^ the lower limit of the range.
      -> k1      -- ^ the upper limit of the range.
      -> Int     -- ^ the maximum number of keys to be fetched.
      -> IO [k2] -- ^ keys in the specified range.
range fdb lower upper maxn =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        alloca $ \sizbuf -> do
          [l, u] <- mapM keyToInt [lower, upper]
          rp <- c_tcfdbrange fdb' l u (fromIntegral maxn) sizbuf
          size <- fromIntegral `fmap` peek sizbuf
          keys <- peekArray size rp
          free rp
          return $ map (fromID . ID) keys

-- | Return list of forward matched keys.
fwmkeys :: (S.Storable k1, S.Storable k2) => TCFDB -> k1 -> Int -> IO [k2]
fwmkeys = fwmHelper c_tcfdbrange4 unTCFDB

-- | Increment the corresponding value. (The value specified by a key
-- is treated as integer.)
addint :: (Key k) => TCFDB -> k -> Int -> IO (Maybe Int)
addint fdb key num =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      key' <- keyToInt key
      sumval <- c_tcfdbaddint fdb' key' (fromIntegral num)
      return $ if sumval == cINT_MIN
                 then Nothing
                 else Just $ fromIntegral sumval

-- | Increment the corresponding value. (The value specified by a key
-- is treated as double.)
adddouble :: (Key k) => TCFDB -> k -> Double -> IO (Maybe Double)
adddouble fdb key num =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      key' <- keyToInt key
      sumval <- c_tcfdbadddouble fdb' key' (realToFrac num)
      return $ if isNaN sumval
                 then Nothing
                 else Just $ realToFrac sumval

-- | Synchronize updated contents of a database object with the file
-- and the device.
sync :: TCFDB -> IO Bool
sync fdb = withForeignPtr (unTCFDB fdb) c_tcfdbsync

-- |  Optimize the file of a Hash database object.
optimize :: TCFDB -> Int32 -> Int64 -> IO Bool
optimize fdb width limsiz =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> c_tcfdboptimize fdb' width limsiz

-- | Delete all records.
vanish :: TCFDB -> IO Bool
vanish fdb = withForeignPtr (unTCFDB fdb) c_tcfdbvanish

-- | Copy the database file.
copy :: TCFDB -> String -> IO Bool
copy = copyHelper c_tcfdbcopy unTCFDB

-- | Return the file path of currentry opened database.
path :: TCFDB -> IO (Maybe String)
path = pathHelper c_tcfdbpath unTCFDB

-- | Return the number of records in the database.
rnum :: TCFDB -> IO Int64
rnum fdb = withForeignPtr (unTCFDB fdb) c_tcfdbrnum

-- | Return the size of the database file.
fsiz :: TCFDB -> IO Int64
fsiz fdb = withForeignPtr (unTCFDB fdb) c_tcfdbfsiz

keyToInt :: (Key k) => k -> IO Int64
keyToInt i = catchJust selector (evaluate (unID . toID $ i)) handler
    where
      selector :: ErrorCall -> Maybe ()
      selector e = if show e == "Prelude.read: no parse"
                     then Just ()
                     else Nothing
      handler _ = error "Database.TokyoCabinet.FDB: invalid key"
