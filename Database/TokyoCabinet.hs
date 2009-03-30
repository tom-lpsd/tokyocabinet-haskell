{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.TokyoCabinet
    (
    -- $doc
      TCM
    , runTCM
    , OpenMode(..)
    , TCDB(..)
    , H.HDB
    , F.FDB
    , BDB
    -- * Error Code
    , E.ECODE(..)
    , E.errmsg
    ) where

import Control.Monad.Trans (MonadIO)

import Database.TokyoCabinet.Storable
import Database.TokyoCabinet.FDB.Key (ID(ID))
import qualified Database.TokyoCabinet.HDB as H
import qualified Database.TokyoCabinet.FDB as F
import qualified Database.TokyoCabinet.BDB as B
import qualified Database.TokyoCabinet.BDB.Cursor as C
import qualified Database.TokyoCabinet.Error as E

import Data.Int

-- $doc
-- Basic Usage (sample code)
--
-- @
--    import Database.TokyoCabinet
--    import Data.ByteString.Char8
-- @
--
-- @
--    putsample :: String -> [(ByteString, ByteString)] -> TCM Bool
--    putsample file kv =
--        do tc <- new :: TCM HDB -- alternatively you can use BDB or FDB
--           open tc file [OWRITER, OCREAT]
--           mapM (uncurry $ put tc) kv
--           close tc
-- @
--
-- @
--    getsample :: String -> ByteString -> TCM (Maybe ByteString)
--    getsample file key =
--        do tc <- new :: TCM HDB -- alternatively you can use BDB or FDB
--           open tc file [OREADER]
--           val <- get tc key
--           close tc
--           return val
-- @
--
-- @
--    main = runTCM (do putsample \"foo.tch\" [(pack \"foo\", pack \"bar\")]
--                      getsample \"foo.tch\" (pack \"foo\")) >>=
--           maybe (return ()) (putStrLn . show)
-- @
--

-- | Tokyo Cabinet related computation. Wrap of IO.
newtype TCM a =
    TCM { -- | Unwrap TCM.
          runTCM :: IO a
    } deriving (Monad, MonadIO)

-- | Represent open mode for `open' function.
data OpenMode = OREADER |
                OWRITER |
                OCREAT  |
                OTRUNC  |
                ONOLCK  |
                OLCKNB
                deriving (Eq, Ord, Show)

-- | Type class that abstract Tokyo Cabinet database.
class TCDB a where
    -- | Create a database object.
    new       :: TCM a

    -- | Free object resource forcibly.
    delete    :: a -> TCM ()

    -- | Open a database file.
    open      :: a          -- ^ database object
              -> String     -- ^ path to database file
              -> [OpenMode] -- ^ open mode
              -> TCM Bool   -- ^ if successful, the return value is True

    -- | Close the database file. If successful, the return value is True
    close     :: a -> TCM Bool

    -- | Store a record.
    put       :: (Storable k, Storable v) =>
                 a -- ^ database object
              -> k -- ^ key
              -> v -- ^ value
              -> TCM Bool -- ^ if successful, the return value is True

    -- | Store a new recoed. If a record with the same key exists
    -- in the database, this function has no effect.
    putkeep   :: (Storable k, Storable v) =>
                 a -- ^ database object
              -> k -- ^ key
              -> v -- ^ value
              -> TCM Bool -- ^ if successful, the return value is True

    -- | Concatenate a value at the end of the existing record.
    putcat    :: (Storable k, Storable v) =>
                 a -- ^ database object
              -> k -- ^ key
              -> v -- ^ value
              -> TCM Bool -- ^ if successful, the return value is True

    -- | Retrieve a record.
    get       :: (Storable k, Storable v) =>
                 a -- ^ database object
              -> k -- ^ key
              -> TCM (Maybe v) -- ^ If successful, the return value is the
                               -- value of the corresponding record wrapped
                               -- by `Just', else, Nothing is returned.

    -- | Remove a record.
    out       :: (Storable k) =>
                 a -- ^ database object
              -> k -- ^ key
              -> TCM Bool -- ^ if successful, the return value is True

    -- | Get the size of the value of a record.
    vsiz      :: (Storable k) =>
                 a -- ^ database object
              -> k -- ^ key
              -> TCM (Maybe Int) -- ^ If successful, the return value
                                 -- is the size of the value of the
                                 -- corresponding record wrapped by
                                 -- `Just', else, it is Nothing.

    -- | Initialize the iterator. If successful, the return value is True.
    iterinit  :: a -> TCM Bool

    -- | Get the next key of the iterator.  If successful, the return
    -- value is the next key wrapped by `Just', else, it is Nothing.
    iternext  :: (Storable v) => a -> TCM (Maybe v)

    -- | Get forward matching keys.
    fwmkeys   :: (Storable k, Storable v) =>
                 a   -- ^ database object
              -> k   -- ^ search string
              -> Int -- ^ the maximum number of keys to be fetched
              -> TCM [v] -- ^ result keys

    -- | Add an integer to a record.
    addint    :: (Storable k) =>
                 a -- ^ database object
              -> k -- ^ key
              -> Int -- ^ the addtional value
              -> TCM (Maybe Int) -- ^ If the corresponding record
                                 -- exists, the value is treated as an
                                 -- integer and is added to. If no
                                 -- record corresponds, a new record
                                 -- of the additional value is stored.

    -- | Add a real number to a record.
    adddouble :: (Storable k) =>
                 a -- ^ database object
              -> k -- ^ key
              -> Double -- ^ the additional value
              -> TCM (Maybe Double) -- ^ If the corresponding record
                                    -- exists, the value is treated as
                                    -- a real number and is added
                                    -- to. If no record corresponds, a
                                    -- new record of the additional
                                    -- value is stored.

    -- | Synchronize updated contents with the file and the device.
    -- If successful, the return value is True.
    sync      :: a -> TCM Bool

    -- | Remove all records. If successful, the return value is True.
    vanish    :: a -> TCM Bool

    -- | Copy the database file.
    copy      :: a        -- ^ database object
              -> String   -- ^ path of the destination file
              -> TCM Bool -- ^ If successful, the return value is True.

    -- | Get the path of the database file.
    path      :: a -> TCM (Maybe String)

    -- | Get the number of records.
    rnum      :: a -> TCM Int64

    -- | Get the size of the database file.
    size      :: a -> TCM Int64

    -- | Get the last happened error code.
    ecode     :: a -> TCM E.ECODE

    -- | Get the default extension for specified database object.
    defaultExtension :: a -> String

openModeToHOpenMode :: OpenMode -> H.OpenMode
openModeToHOpenMode OREADER = H.OREADER
openModeToHOpenMode OWRITER = H.OWRITER
openModeToHOpenMode OCREAT  = H.OCREAT
openModeToHOpenMode OTRUNC  = H.OTRUNC
openModeToHOpenMode ONOLCK  = H.ONOLCK
openModeToHOpenMode OLCKNB  = H.OLCKNB

lift :: (a -> IO b) -> a -> TCM b
lift = (TCM .)

lift2 :: (a -> b -> IO c) -> a -> b -> TCM c
lift2 f x y = TCM $ f x y

lift3 :: (a -> b -> c -> IO d) -> a -> b -> c -> TCM d
lift3 f x y z = TCM $ f x y z

instance TCDB H.HDB where
    new               = TCM   H.new
    delete            = lift  H.delete
    open tc name mode = TCM $ H.open tc name (map openModeToHOpenMode mode)
    close             = lift  H.close
    put               = lift3 H.put
    putkeep           = lift3 H.putkeep
    putcat            = lift3 H.putcat
    get               = lift2 H.get
    out               = lift2 H.out
    vsiz              = lift2 H.vsiz
    iterinit          = lift  H.iterinit
    iternext          = lift  H.iternext
    fwmkeys           = lift3 H.fwmkeys
    addint            = lift3 H.addint
    adddouble         = lift3 H.adddouble
    sync              = lift  H.sync
    vanish            = lift  H.vanish
    copy              = lift2 H.copy
    path              = lift  H.path
    rnum              = lift  H.rnum
    size              = lift  H.fsiz
    ecode             = lift  H.ecode
    defaultExtension  = const ".tch"

openModeToBOpenMode :: OpenMode -> B.OpenMode
openModeToBOpenMode OREADER = B.OREADER
openModeToBOpenMode OWRITER = B.OWRITER
openModeToBOpenMode OCREAT  = B.OCREAT
openModeToBOpenMode OTRUNC  = B.OTRUNC
openModeToBOpenMode ONOLCK  = B.ONOLCK
openModeToBOpenMode OLCKNB  = B.OLCKNB

data BDB = BDB { unTCBDB    :: B.BDB
               , unTCBDBCUR :: C.BDBCUR }

liftB :: (B.BDB -> IO a) -> BDB -> TCM a
liftB f x = TCM $ f (unTCBDB x)

liftB2 :: (B.BDB -> a -> IO b) -> BDB -> a -> TCM b
liftB2 f x y = TCM $ f (unTCBDB x) y

liftB3 :: (B.BDB -> a -> b -> IO c) -> BDB -> a -> b -> TCM c
liftB3 f x y z = TCM $ f (unTCBDB x) y z

instance TCDB BDB where
    new               = TCM $ do bdb <- B.new
                                 cur <- C.new bdb
                                 return $ BDB bdb cur
    delete            = liftB  B.delete
    open tc name mode = TCM $  B.open (unTCBDB tc) name
                                   (map openModeToBOpenMode mode)
    close             = liftB  B.close
    put               = liftB3 B.put
    putkeep           = liftB3 B.putkeep
    putcat            = liftB3 B.putcat
    get               = liftB2 B.get
    out               = liftB2 B.out
    vsiz              = liftB2 B.vsiz
    iterinit bdb      = TCM $ C.first (unTCBDBCUR bdb)
    iternext bdb      = TCM $ do k <- C.key (unTCBDBCUR bdb)
                                 C.next (unTCBDBCUR bdb)
                                 return k
    fwmkeys           = liftB3 B.fwmkeys
    addint            = liftB3 B.addint
    adddouble         = liftB3 B.adddouble
    sync              = liftB  B.sync
    vanish            = liftB  B.vanish
    copy              = liftB2 B.copy
    path              = liftB  B.path
    rnum              = liftB  B.rnum
    size              = liftB  B.fsiz
    ecode             = liftB  B.ecode
    defaultExtension  = const ".tcb"

instance TCDB B.BDB where
    new               = TCM   B.new
    delete            = lift  B.delete
    open tc name mode = TCM $ B.open tc name (map openModeToBOpenMode mode)
    close             = lift  B.close
    put               = lift3 B.put
    putkeep           = lift3 B.putkeep
    putcat            = lift3 B.putcat
    get               = lift2 B.get
    out               = lift2 B.out
    vsiz              = lift2 B.vsiz
    iterinit          = undefined
    iternext          = undefined
    fwmkeys           = lift3 B.fwmkeys
    addint            = lift3 B.addint
    adddouble         = lift3 B.adddouble
    sync              = lift  B.sync
    vanish            = lift  B.vanish
    copy              = lift2 B.copy
    path              = lift  B.path
    rnum              = lift  B.rnum
    size              = lift  B.fsiz
    ecode             = lift  B.ecode
    defaultExtension  = const ".tcb"

openModeToFOpenMode :: OpenMode -> F.OpenMode
openModeToFOpenMode OREADER = F.OREADER
openModeToFOpenMode OWRITER = F.OWRITER
openModeToFOpenMode OCREAT  = F.OCREAT
openModeToFOpenMode OTRUNC  = F.OTRUNC
openModeToFOpenMode ONOLCK  = F.ONOLCK
openModeToFOpenMode OLCKNB  = F.OLCKNB

liftF2 :: (Storable b) => (a -> ID -> IO c) -> a -> b -> TCM c
liftF2 f x y = TCM $ f x (storableToKey y)

liftF3 :: (Storable b) => (a -> ID -> c -> IO d) -> a -> b -> c -> TCM d
liftF3 f x y z = TCM $ f x (storableToKey y) z

storableToKey :: (Storable a) => a -> ID
storableToKey = ID . toInt64

keyToStorable :: (Storable a) => String -> a
keyToStorable = fromString

instance TCDB F.FDB where
    new               = TCM    F.new
    delete            = lift   F.delete
    open tc name mode = TCM $  F.open tc name (map openModeToFOpenMode mode)
    close             = lift   F.close
    put               = liftF3 F.put
    putkeep           = liftF3 F.putkeep
    putcat            = liftF3 F.putcat
    get               = liftF2 F.get
    out               = liftF2 F.out
    vsiz              = liftF2 F.vsiz
    iterinit          = lift   F.iterinit
    iternext tc       = TCM    $ do key <- F.iternext tc
                                    case key of
                                      Nothing -> return Nothing
                                      Just x  -> return $ Just (keyToStorable x)
    fwmkeys           = lift3  F.fwmkeys
    addint            = liftF3 F.addint
    adddouble         = liftF3 F.adddouble
    sync              = lift   F.sync
    vanish            = lift   F.vanish
    copy              = lift2  F.copy
    path              = lift   F.path
    rnum              = lift   F.rnum
    size              = lift   F.fsiz
    ecode             = lift   F.ecode
    defaultExtension  = const ".tcf"
