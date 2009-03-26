-- | Interface to B+ tree based DBM. See also,
-- <http://tokyocabinet.sourceforge.net/spex-en.html#tcbdbapi> for details
module Database.TokyoCabinet.BDB
    (
    -- $doc
    -- * Constructors
      TCBDB
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
    , putdup
    , putlist
    , out
    , outlist
    , get
    , getlist
    , vnum
    , vsiz
    , range
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
    ) where

import Data.Int

import Foreign.Ptr
import Foreign.ForeignPtr

import Database.TokyoCabinet.Error
import Database.TokyoCabinet.BDB.C
import Database.TokyoCabinet.Internal
import qualified Database.TokyoCabinet.Storable as S

-- $doc
-- Example
--
-- @
--    import Control.Monad (unless)
--    import Database.TokyoCabinet.BDB
--    import qualified Database.TokyoCabinet.BDB.Cursor as C
-- @
-- 
-- @
--    main :: IO ()
--    main =
--        do bdb <- new
--           let err = flip unless (ecode bdb >>= error . show)
--           -- open the database
--           open bdb \"casket.tcb\" [OWRITER, OCREAT] >>= err
--           -- store records
--           puts bdb [ (\"foo\", \"hop\")
--                    , (\"bar\", \"step\")
--                    , (\"baz\", \"jump\") ] >>= err . (all id)
--           -- retrieve records
--           get bdb \"foo\" >>= maybe (error \"something go wrong\") putStrLn
--           -- traverse records
--           cur <- C.new bdb
--           C.first cur >>= err
--           iter cur >>= putStrLn . show
--           -- close the database
--           close bdb >>= err
--        where
--          puts :: TCBDB -> [(String, String)] -> IO [Bool]
--          puts bdb = mapM (uncurry $ put bdb)
-- @
--
-- @
--          iter :: C.TCBDBCUR -> IO [(String, String)]
--          iter cur = do
--            [key, value] <- sequence [C.key cur, C.val cur]
--            case (key, value) of
--              (Just k, Just v) -> C.next cur >> iter cur >>= return . ((k,v):)
--              _ -> return []
-- @
--

-- | Create a B+ tree database object. 
new :: IO TCBDB
new = TCBDB `fmap` (c_tcbdbnew >>= newForeignPtr tcbdbFinalizer)

-- | Force to free region of BDB. 
-- BDB is kept by ForeignPtr, so Haskell runtime GC cleans up memory for
-- almost situation. Most always, you don't need to call this. 
-- After call this, you must not touch BDB object. Its behavior is undefined.
delete :: TCBDB -> IO ()
delete bdb = finalizeForeignPtr (unTCBDB bdb)

-- | Return the last happened error code.
ecode :: TCBDB -> IO TCECODE
ecode bdb = cintToError `fmap` withForeignPtr (unTCBDB bdb) c_tcbdbecode

-- | Set the tuning parameters.
tune :: TCBDB -- ^ TCBDB object
     -> Int32 -- ^ the number of members in each leaf page.
     -> Int32 -- ^ the number of members in each non-leaf page.
     -> Int64 -- ^ the number of elements of the bucket array. 
     -> Int8  -- ^ the size of record alignment by power of 2.
     -> Int8  -- ^ the maximum number of elements of the free block
              -- pool by power of 2.
     -> [TuningOption] -- ^ tuning options
     -> IO Bool -- ^ if successful, the return value is True.
tune bdb lmemb nmemb bnum apow fpow opts =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        c_tcbdbtune bdb' lmemb nmemb bnum apow fpow (combineTuningOption opts)

-- | Set the caching parameters.
setcache :: TCBDB -- ^ TCBDB object
         -> Int32 -- ^ the maximum number of leaf nodes to be cached.
         -> Int32 -- ^ the maximum number of non-leaf nodes to be cached.
         -> IO Bool -- ^ if successful, the return value is True.
setcache bdb lcnum ncnum =
    withForeignPtr (unTCBDB bdb) $ \bdb' -> c_tcbdbsetcache bdb' lcnum ncnum

-- | Set the size of extra mapped memory.
setxmsiz :: TCBDB -> Int64 -> IO Bool
setxmsiz bdb xmsiz =
    withForeignPtr (unTCBDB bdb) $ \bdb' -> c_tcbdbsetxmsiz bdb' xmsiz

-- | Open BDB database file.
open :: TCBDB -> String -> [OpenMode] -> IO Bool
open = openHelper c_tcbdbopen unTCBDB combineOpenMode

-- | Close the database file.
close :: TCBDB -> IO Bool
close bdb = withForeignPtr (unTCBDB bdb) c_tcbdbclose

-- | Stora a record (key-value pair) on BDB.  Key and value type must
-- be instance of Storable class.  Usually, we can use `String',
-- `ByteString' for key, `String', `ByteString', `Int', `Double' for
-- value.
put :: (S.Storable k, S.Storable v) => TCBDB -> k -> v -> IO Bool
put = putHelper c_tcbdbput unTCBDB

-- | Store a new record. If a record with the same key exists in the
-- database, this function has no effect.
putkeep :: (S.Storable a, S.Storable b) => TCBDB -> a -> b -> IO Bool
putkeep = putHelper c_tcbdbputkeep unTCBDB

-- | Concatenate a value at the end of the existing record.
putcat :: (S.Storable a, S.Storable b) => TCBDB -> a -> b -> IO Bool
putcat = putHelper c_tcbdbputcat unTCBDB

-- | Store a record with allowing duplication of keys. 
putdup :: (S.Storable a, S.Storable b) => TCBDB -> a -> b -> IO Bool
putdup = putHelper c_tcbdbputdup unTCBDB

-- | Store records with allowing duplication of keys. 
putlist :: (S.Storable a, S.Storable b) => TCBDB -> a -> [b] -> IO Bool
putlist bdb key vals = do
  and `fmap` mapM (putdup bdb key) vals

-- | Delete a record. If the key of duplicated records is specified,
-- the first one is deleted. 
out :: (S.Storable a) => TCBDB -> a -> IO Bool
out = outHelper c_tcbdbout unTCBDB

-- | Delete records. If the key of duplicated records is specified,
-- all of them are deleted.
outlist :: (S.Storable a) => TCBDB -> a -> IO Bool
outlist = outHelper c_tcbdbout3 unTCBDB

-- | Return the value of record. If the key of duplicated records is
-- specified, the first one is returned.
get :: (S.Storable a, S.Storable b) => TCBDB -> a -> IO (Maybe b)
get = getHelper c_tcbdbget unTCBDB

-- | Retrieve records. 
getlist :: (S.Storable a, S.Storable b) => TCBDB -> a -> IO [b]
getlist bdb key =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        S.withPtrLen key $ \(kbuf, ksize) -> do
          ptr <- c_tcbdbget4 bdb' kbuf ksize
          if ptr == nullPtr
            then return []
            else peekTCListAndFree ptr

-- | Return the number of records corresponding to a key. 
vnum :: (S.Storable a) => TCBDB -> a -> IO (Maybe Int)
vnum bdb key =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        S.withPtrLen key $ \(kbuf, ksize) -> do
            res <- c_tcbdbvnum bdb' kbuf ksize
            return $ if res == 0
                       then Nothing
                       else Just $ fromIntegral res

-- | Return the size of the value of a record. If the key of duplicated
-- records is specified, the first one is selected.
vsiz :: (S.Storable a) => TCBDB -> a -> IO (Maybe Int)
vsiz = vsizHelper c_tcbdbvsiz unTCBDB

-- | Return list of keys in the specified range.
range :: (S.Storable a) =>
         TCBDB   -- ^ TCBDB object
      -> Maybe a -- ^ the key of the beginning border. If it is
                 -- Nothing, the first record in the database is
                 -- specified.
      -> Bool    -- ^ whether the beginning border is inclusive or not. 
      -> Maybe a -- ^ the key of the ending border. If it is Nothing,
                 -- the last record is specified.
      -> Bool    -- ^ whether the ending border is inclusive or not.
      -> Int     -- ^ the maximum number of keys to be fetched. If it
                 -- is negative value, no limit is specified.
      -> IO [a]  -- ^ keys in the specified range.
range bdb bkey binc ekey einc maxn =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        withPtrLen' bkey $ \(bkbuf, bksiz) ->
        withPtrLen' ekey $ \(ekbuf, eksiz) ->
            c_tcbdbrange bdb' bkbuf bksiz binc ekbuf eksiz einc
                             (fromIntegral maxn) >>= peekTCListAndFree
    where
      withPtrLen' (Just key) action = S.withPtrLen key action
      withPtrLen' Nothing action = action (nullPtr, 0)

-- | Return list of forward matched keys.
fwmkeys :: (S.Storable a, S.Storable b) =>
           TCBDB  -- ^ TCBDB object
        -> a      -- ^ search string
        -> Int    -- ^ the maximum number of keys to be fetched. If it
                  -- is negative value, no limit is specified.
        -> IO [b] -- ^ keys matches specified string (in forward matching).
fwmkeys = fwmHelper c_tcbdbfwmkeys unTCBDB

-- | Increment the corresponding value. (The value specified by a key
-- is treated as integer.)
addint :: (S.Storable a) =>
          TCBDB -- ^ TCBDB object.
       -> a     -- ^ Key.
       -> Int   -- ^ Amount of increment.
       -> IO (Maybe Int) -- ^ If successful, a new value is returned.
addint = addHelper c_tcbdbaddint unTCBDB fromIntegral fromIntegral (== cINT_MIN)

-- | Increment the corresponding value. (The value specified by a key
-- is treated as double.)
adddouble :: (S.Storable a) =>
             TCBDB  -- ^ TCBDB object.
          -> a      -- ^ Key.
          -> Double -- ^ Amount of increment.
          -> IO (Maybe Double) -- ^ If successful, a new value is returned.
adddouble = addHelper c_tcbdbadddouble unTCBDB realToFrac realToFrac isNaN

-- | Synchronize updated contents of a database object with the file
-- and the device.
sync :: TCBDB -> IO Bool
sync bdb = withForeignPtr (unTCBDB bdb) c_tcbdbsync

-- |  Optimize the file of a B+ tree database object.
optimize :: TCBDB
         -> Int32 -- ^ the number of members in each leaf page.
         -> Int32 -- ^ the number of members in each non-leaf page.
         -> Int64 -- ^ the number of elements of the bucket array. 
         -> Int8  -- ^ the size of record alignment by power of 2.
         -> Int8  -- ^ the maximum number of elements of the free block
                  -- pool by power of 2.
         -> [TuningOption] -- ^ tuning options
         -> IO Bool -- ^ if successful, the return value is True.
optimize bdb lmemb nmemb bnum apow fpow opts =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        c_tcbdboptimize bdb' lmemb nmemb bnum apow fpow
                        (combineTuningOption opts)

-- | Delete all records.
vanish :: TCBDB -> IO Bool
vanish bdb = withForeignPtr (unTCBDB bdb) c_tcbdbvanish

-- | Copy the database file.
copy :: TCBDB -> String -> IO Bool
copy = copyHelper c_tcbdbcopy unTCBDB

-- | Begin the transaction.
tranbegin :: TCBDB -> IO Bool
tranbegin bdb = withForeignPtr (unTCBDB bdb) c_tcbdbtranbegin

-- | Commit the transaction.
trancommit :: TCBDB -> IO Bool
trancommit bdb = withForeignPtr (unTCBDB bdb) c_tcbdbtrancommit

-- | Abort the transaction.
tranabort :: TCBDB -> IO Bool
tranabort bdb = withForeignPtr (unTCBDB bdb) c_tcbdbtranabort

-- | Return the file path of currentry opened database.
path :: TCBDB -> IO (Maybe String)
path = pathHelper c_tcbdbpath unTCBDB

-- | Return the number of records in the database.
rnum :: TCBDB -> IO Int64
rnum bdb = withForeignPtr (unTCBDB bdb) c_tcbdbrnum

-- | Return the size of the database file.
fsiz :: TCBDB -> IO Int64
fsiz bdb = withForeignPtr (unTCBDB bdb) c_tcbdbfsiz
