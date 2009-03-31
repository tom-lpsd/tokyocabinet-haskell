-- | Interface to Fixed-length DBM. See also,
-- <http://tokyocabinet.sourceforge.net/spex-en.html#tcfdbapi> for details
module Database.TokyoCabinet.FDB
    (
     -- $doc
     -- * Constructors
      FDB
    , ECODE(..)
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
import Database.TokyoCabinet.Sequence
import Database.TokyoCabinet.Storable

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

-- $doc
-- Example
--
-- @
--    import Control.Monad
--    import Database.TokyoCabinet.FDB
-- @
--
-- @  
--    main = do fdb <- new
--              -- open the database
--              open fdb \"casket.tcf\" [OWRITER, OCREAT] >>= err fdb
--              -- store records
--              puts fdb [(1, \"one\"), (12, \"twelve\"), (144, \"one forty four\")] >>=
--                        err fdb . (all id)
--              -- retrieve records
--              get fdb (1 :: Int) >>= maybe (error \"something goes wrong\") putStrLn
--              -- close the database
--              close fdb >>= err fdb
--        where
--          puts :: FDB -> [(Int, String)] -> IO [Bool]
--          puts fdb = mapM (uncurry $ put fdb)
-- @
--
-- @  
--          err :: FDB -> Bool -> IO ()
--          err fdb = flip unless $ ecode fdb >>= error . show
-- @
--


data FDB = FDB { unTCFDB :: !(ForeignPtr FDB') }

-- | Create a Fixed-length database object. 
new :: IO FDB
new = FDB `fmap` (c_tcfdbnew >>= newForeignPtr tcfdbFinalizer)

-- | Free FDB resource forcibly.
-- FDB is kept by ForeignPtr, so Haskell runtime GC cleans up memory for
-- almost situation. Most always, you don't need to call this. 
-- After call this, you must not touch FDB object. Its behavior is undefined.
delete :: FDB -> IO ()
delete fdb = finalizeForeignPtr $ unTCFDB fdb

-- | Return the last happened error code.
ecode :: FDB -> IO ECODE
ecode fdb =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        cintToError `fmap` c_tcfdbecode fdb'

-- | Set the tuning parameters.
tune :: FDB     -- ^ FDB object.
     -> Int32   -- ^ the width of the value of each record.
     -> Int64   -- ^ the limit size of the database file.
     -> IO Bool -- ^ if successful, the return value is True.
tune fdb width limsiz =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> c_tcfdbtune fdb' width limsiz

-- | Open FDB database file.
open :: FDB -> String -> [OpenMode] -> IO Bool
open = openHelper c_tcfdbopen unTCFDB combineOpenMode

-- | Close the database file.
close :: FDB -> IO Bool
close fdb = withForeignPtr (unTCFDB fdb) c_tcfdbclose

type FunPut' = Ptr FDB' -> Int64 -> Ptr Word8 -> CInt -> IO Bool
putHelper' :: (Key k, Storable v) => FunPut' -> FDB -> k -> v -> IO Bool
putHelper' func fdb key val =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        withPtrLen val $ \(vbuf, vsize) -> do
          key' <- keyToInt key
          func fdb' key' vbuf vsize

-- | Stora a record (key-value pair) on FDB.  Key type must be
-- instance of Key class. Value type must be instance of Storable.
put :: (Key k, Storable v) => FDB -> k -> v -> IO Bool
put = putHelper' c_tcfdbput

-- | Store a new record. If a record with the same key exists in the
-- database, this function has no effect.
putkeep :: (Key k, Storable v) => FDB -> k -> v -> IO Bool
putkeep = putHelper' c_tcfdbputkeep

-- | Concatenate a value at the end of the existing record.
putcat :: (Key k, Storable v) => FDB -> k -> v -> IO Bool
putcat =  putHelper' c_tcfdbputcat

-- | Delete a record. 
out :: (Key k) => FDB -> k -> IO Bool
out fdb key =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        c_tcfdbout fdb' =<< keyToInt key

-- | Return the value of record. 
get :: (Key k, Storable v) => FDB -> k -> IO (Maybe v)
get fdb key =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        alloca $ \sizbuf -> do
          key' <- keyToInt key
          vbuf  <- c_tcfdbget fdb' key' sizbuf
          vsize <- peek sizbuf
          flip maybePeek vbuf $ \vbuf' -> peekPtrLen (vbuf', vsize)

-- | Return the byte size of value in a record.
vsiz :: (Key k) => FDB -> k -> IO (Maybe Int)
vsiz fdb key =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      vsize <- c_tcfdbvsiz fdb' =<< keyToInt key
      return $ if vsize == (-1)
                 then Nothing
                 else Just (fromIntegral vsize)

-- | Initialize the iterator of a FDB object.
iterinit :: FDB -> IO Bool
iterinit fdb = withForeignPtr (unTCFDB fdb) c_tcfdbiterinit

-- | Return the next key of the iterator of a FDB object.
iternext :: (Key k) => FDB -> IO (Maybe k)
iternext fdb = 
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      i <-  c_tcfdbiternext fdb'
      return $ if i == 0
                 then Nothing
                 else Just (fromID $ ID i)

-- | Return list of keys in the specified range.
range :: (Key k1, Key k2) =>
         FDB     -- ^ FDB object
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
fwmkeys :: (Storable k1, Storable k2, Sequence q) =>
           FDB -> k1 -> Int -> IO (q k2)
fwmkeys fdb k maxn = smap fromString =<< fwmkeys' fdb k maxn
    where fwmkeys' = fwmHelper c_tcfdbrange4 unTCFDB

-- | Increment the corresponding value. (The value specified by a key
-- is treated as integer.)
addint :: (Key k) => FDB -> k -> Int -> IO (Maybe Int)
addint fdb key num =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      key' <- keyToInt key
      sumval <- c_tcfdbaddint fdb' key' (fromIntegral num)
      return $ if sumval == cINT_MIN
                 then Nothing
                 else Just $ fromIntegral sumval

-- | Increment the corresponding value. (The value specified by a key
-- is treated as double.)
adddouble :: (Key k) => FDB -> k -> Double -> IO (Maybe Double)
adddouble fdb key num =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      key' <- keyToInt key
      sumval <- c_tcfdbadddouble fdb' key' (realToFrac num)
      return $ if isNaN sumval
                 then Nothing
                 else Just $ realToFrac sumval

-- | Synchronize updated contents of a database object with the file
-- and the device.
sync :: FDB -> IO Bool
sync fdb = withForeignPtr (unTCFDB fdb) c_tcfdbsync

-- |  Optimize the file of a Hash database object.
optimize :: FDB -> Int32 -> Int64 -> IO Bool
optimize fdb width limsiz =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> c_tcfdboptimize fdb' width limsiz

-- | Delete all records.
vanish :: FDB -> IO Bool
vanish fdb = withForeignPtr (unTCFDB fdb) c_tcfdbvanish

-- | Copy the database file.
copy :: FDB -> String -> IO Bool
copy = copyHelper c_tcfdbcopy unTCFDB

-- | Return the file path of currentry opened database.
path :: FDB -> IO (Maybe String)
path = pathHelper c_tcfdbpath unTCFDB

-- | Return the number of records in the database.
rnum :: FDB -> IO Word64
rnum fdb = withForeignPtr (unTCFDB fdb) c_tcfdbrnum

-- | Return the size of the database file.
fsiz :: FDB -> IO Word64
fsiz fdb = withForeignPtr (unTCFDB fdb) c_tcfdbfsiz

keyToInt :: (Key k) => k -> IO Int64
keyToInt i = catchJust selector (evaluate (unID . toID $ i)) handler
    where
      selector :: ErrorCall -> Maybe ()
      selector e = if show e == "Prelude.read: no parse"
                     then Just ()
                     else Nothing
      handler _ = error "Database.TokyoCabinet.FDB: invalid key"
