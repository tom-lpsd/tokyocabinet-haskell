-- | Interface to B+ tree based DBM. See also,
-- <http://tokyocabinet.sourceforge.net/spex-en.html#tcbdbapi> for details
module Database.TokyoCabinet.BDB
    (
    -- $doc
    -- * Constructors
      BDB
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
import Data.Word

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (peek)
import Foreign.Marshal (alloca)

import Database.TokyoCabinet.Error
import Database.TokyoCabinet.BDB.C
import Database.TokyoCabinet.List.C
import Database.TokyoCabinet.Internal
import Database.TokyoCabinet.Storable
import Database.TokyoCabinet.Sequence

-- $doc
-- Example
--
-- @
--    import Control.Monad
--    import Database.TokyoCabinet.BDB
--    import qualified Database.TokyoCabinet.BDB.Cursor as C
-- @
--
-- @  
--    main :: IO ()
--    main =
--        do bdb <- new
--           -- open the database
--           open bdb \"casket.tcb\" [OWRITER, OCREAT] >>= err bdb
--           -- store records
--           puts bdb [ (\"foo\", \"hop\"), (\"bar\", \"step\"), (\"baz\", \"jump\") ] >>=
--                    err bdb . (all id)
--           -- retrieve records
--           get bdb \"foo\" >>= maybe (error \"something goes wrong\") putStrLn
--           -- traverse records
--           cur <- C.new bdb
--           C.first cur >>= err bdb
--           iter cur >>= putStrLn . show
--           -- close the database
--           close bdb >>= err bdb
--        where
--          puts :: BDB -> [(String, String)] -> IO [Bool]
--          puts bdb = mapM (uncurry $ put bdb)
-- @
--
-- @
--          err :: BDB -> Bool -> IO ()
--          err bdb = flip unless $ ecode bdb >>= error . show
-- @
--
-- @  
--          iter :: C.BDBCUR -> IO [(String, String)]
--          iter cur = do
--            [key, value] <- sequence [C.key cur, C.val cur]
--            case (key, value) of
--              (Just k, Just v) -> C.next cur >> iter cur >>= return . ((k,v):)
--              _ -> return []
-- @
--

-- | Create a B+ tree database object. 
new :: IO BDB
new = BDB `fmap` (c_tcbdbnew >>= newForeignPtr tcbdbFinalizer)

-- | Free BDB resource forcibly. 
-- BDB is kept by ForeignPtr, so Haskell runtime GC cleans up memory for
-- almost situation. Most always, you don't need to call this. 
-- After call this, you must not touch BDB object. Its behavior is undefined.
delete :: BDB -> IO ()
delete bdb = finalizeForeignPtr (unTCBDB bdb)

-- | Return the last happened error code.
ecode :: BDB -> IO ECODE
ecode bdb = cintToError `fmap` withForeignPtr (unTCBDB bdb) c_tcbdbecode

-- | Set the tuning parameters.
tune :: BDB   -- ^ BDB object
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
setcache :: BDB   -- ^ BDB object
         -> Int32 -- ^ the maximum number of leaf nodes to be cached.
         -> Int32 -- ^ the maximum number of non-leaf nodes to be cached.
         -> IO Bool -- ^ if successful, the return value is True.
setcache bdb lcnum ncnum =
    withForeignPtr (unTCBDB bdb) $ \bdb' -> c_tcbdbsetcache bdb' lcnum ncnum

-- | Set the size of extra mapped memory.
setxmsiz :: BDB -> Int64 -> IO Bool
setxmsiz bdb xmsiz =
    withForeignPtr (unTCBDB bdb) $ \bdb' -> c_tcbdbsetxmsiz bdb' xmsiz

-- | Open BDB database file.
open :: BDB -> String -> [OpenMode] -> IO Bool
open = openHelper c_tcbdbopen unTCBDB combineOpenMode

-- | Close the database file.
close :: BDB -> IO Bool
close bdb = withForeignPtr (unTCBDB bdb) c_tcbdbclose

-- | Stora a record (key-value pair) on BDB.  Key and value type must
-- be instance of Storable class.  Usually, we can use `String',
-- `ByteString' for key, `String', `ByteString', `Int', `Double' for
-- value.
put :: (Storable k, Storable v) => BDB -> k -> v -> IO Bool
put = putHelper c_tcbdbput unTCBDB

-- | Store a new record. If a record with the same key exists in the
-- database, this function has no effect.
putkeep :: (Storable k, Storable v) => BDB -> k -> v -> IO Bool
putkeep = putHelper c_tcbdbputkeep unTCBDB

-- | Concatenate a value at the end of the existing record.
putcat :: (Storable k, Storable v) => BDB -> k -> v -> IO Bool
putcat = putHelper c_tcbdbputcat unTCBDB

-- | Store a record with allowing duplication of keys. 
putdup :: (Storable k, Storable v) => BDB -> k -> v -> IO Bool
putdup = putHelper c_tcbdbputdup unTCBDB

-- | Store records with allowing duplication of keys. 
putlist :: (Storable k, Storable v, Sequence q) =>
           BDB -> k -> q v -> IO Bool
putlist bdb key vals =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        withList vals $ \tcls ->
            withPtrLen key $ \(kbuf, ksize) ->
                alloca $ \sizbuf -> do
                    num <- c_tclistnum tcls
                    putlist' bdb' (kbuf, ksize) tcls sizbuf num
    where
      putlist' bdb' (kbuf, ksize) tcls sizbuf = put' 0
          where
            put' n num
                | n < num = do vbuf  <- c_tclistval tcls n sizbuf
                               vsize <- fromIntegral `fmap` peek sizbuf
                               res <- c_tcbdbputdup bdb' kbuf ksize vbuf vsize
                               if res then put' (n+1) num else return False
                | otherwise = return True

-- | Delete a record. If the key of duplicated records is specified,
-- the first one is deleted. 
out :: (Storable k) => BDB -> k -> IO Bool
out = outHelper c_tcbdbout unTCBDB

-- | Delete records. If the key of duplicated records is specified,
-- all of them are deleted.
outlist :: (Storable k) => BDB -> k -> IO Bool
outlist = outHelper c_tcbdbout3 unTCBDB

-- | Return the value of record. If the key of duplicated records is
-- specified, the first one is returned.
get :: (Storable k, Storable v) => BDB -> k -> IO (Maybe v)
get = getHelper c_tcbdbget unTCBDB

-- | Retrieve records. 
getlist :: (Storable k, Storable v, Sequence q) => BDB -> k -> IO (q v)
getlist bdb key =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        withPtrLen key $ \(kbuf, ksize) -> do
          ptr <- c_tcbdbget4 bdb' kbuf ksize
          if ptr == nullPtr
            then empty
            else peekList' ptr

-- | Return the number of records corresponding to a key. 
vnum :: (Storable k) => BDB -> k -> IO (Maybe Int)
vnum bdb key =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        withPtrLen key $ \(kbuf, ksize) -> do
            res <- c_tcbdbvnum bdb' kbuf ksize
            return $ if res == 0
                       then Nothing
                       else Just $ fromIntegral res

-- | Return the size of the value of a record. If the key of duplicated
-- records is specified, the first one is selected.
vsiz :: (Storable k) => BDB -> k -> IO (Maybe Int)
vsiz = vsizHelper c_tcbdbvsiz unTCBDB

-- | Return list of keys in the specified range.
range :: (Storable k, Sequence q) =>
         BDB     -- ^ BDB object
      -> Maybe k -- ^ the key of the beginning border. If it is
                 -- Nothing, the first record in the database is
                 -- specified.
      -> Bool    -- ^ whether the beginning border is inclusive or not. 
      -> Maybe k -- ^ the key of the ending border. If it is Nothing,
                 -- the last record is specified.
      -> Bool    -- ^ whether the ending border is inclusive or not.
      -> Int     -- ^ the maximum number of keys to be fetched. If it
                 -- is negative value, no limit is specified.
      -> IO (q k)  -- ^ keys in the specified range.
range bdb bkey binc ekey einc maxn =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        withPtrLen' bkey $ \(bkbuf, bksiz) ->
        withPtrLen' ekey $ \(ekbuf, eksiz) ->
            c_tcbdbrange bdb' bkbuf bksiz binc ekbuf eksiz einc
                             (fromIntegral maxn) >>= peekList'
    where
      withPtrLen' (Just key) action = withPtrLen key action
      withPtrLen' Nothing action = action (nullPtr, 0)

-- | Return list of forward matched keys.
fwmkeys :: (Storable k1, Storable k2, Sequence q) =>
           BDB    -- ^ BDB object
        -> k1     -- ^ search string
        -> Int    -- ^ the maximum number of keys to be fetched. If it
                  -- is negative value, no limit is specified.
        -> IO (q k2) -- ^ keys matches specified string (in forward matching).
fwmkeys = fwmHelper c_tcbdbfwmkeys unTCBDB

-- | Increment the corresponding value. (The value specified by a key
-- is treated as integer.)
addint :: (Storable k) =>
          BDB   -- ^ BDB object.
       -> k     -- ^ Key.
       -> Int   -- ^ Amount of increment.
       -> IO (Maybe Int) -- ^ If successful, a new value is returned.
addint = addHelper c_tcbdbaddint unTCBDB fromIntegral fromIntegral (== cINT_MIN)

-- | Increment the corresponding value. (The value specified by a key
-- is treated as double.)
adddouble :: (Storable k) =>
             BDB    -- ^ BDB object.
          -> k      -- ^ Key.
          -> Double -- ^ Amount of increment.
          -> IO (Maybe Double) -- ^ If successful, a new value is returned.
adddouble = addHelper c_tcbdbadddouble unTCBDB realToFrac realToFrac isNaN

-- | Synchronize updated contents of a database object with the file
-- and the device.
sync :: BDB -> IO Bool
sync bdb = withForeignPtr (unTCBDB bdb) c_tcbdbsync

-- |  Optimize the file of a B+ tree database object.
optimize :: BDB
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
vanish :: BDB -> IO Bool
vanish bdb = withForeignPtr (unTCBDB bdb) c_tcbdbvanish

-- | Copy the database file.
copy :: BDB -> String -> IO Bool
copy = copyHelper c_tcbdbcopy unTCBDB

-- | Begin the transaction.
tranbegin :: BDB -> IO Bool
tranbegin bdb = withForeignPtr (unTCBDB bdb) c_tcbdbtranbegin

-- | Commit the transaction.
trancommit :: BDB -> IO Bool
trancommit bdb = withForeignPtr (unTCBDB bdb) c_tcbdbtrancommit

-- | Abort the transaction.
tranabort :: BDB -> IO Bool
tranabort bdb = withForeignPtr (unTCBDB bdb) c_tcbdbtranabort

-- | Return the file path of currentry opened database.
path :: BDB -> IO (Maybe String)
path = pathHelper c_tcbdbpath unTCBDB

-- | Return the number of records in the database.
rnum :: BDB -> IO Word64
rnum bdb = withForeignPtr (unTCBDB bdb) c_tcbdbrnum

-- | Return the size of the database file.
fsiz :: BDB -> IO Word64
fsiz bdb = withForeignPtr (unTCBDB bdb) c_tcbdbfsiz
