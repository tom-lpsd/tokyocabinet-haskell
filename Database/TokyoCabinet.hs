{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.TokyoCabinet
    (
    -- $doc
      TCM
    , runTCM
    , OpenMode(..)
    , TCDB(..)
    , H.TCHDB
    , F.TCFDB
    , BDB
    -- * Error Code
    , E.TCECODE(..)
    , E.errmsg
    ) where

import Control.Monad.Trans (MonadIO)

import Database.TokyoCabinet.Storable
import Database.TokyoCabinet.FDB.Key
import qualified Database.TokyoCabinet.HDB as H
import qualified Database.TokyoCabinet.FDB as F
import qualified Database.TokyoCabinet.BDB as B
import qualified Database.TokyoCabinet.BDB.Cursor as C
import qualified Database.TokyoCabinet.Error as E

import Foreign.Ptr (castPtr)
import Foreign.C.String (newCStringLen)

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
--        do tc <- new :: TCM TCHDB -- alternatively you can use BDB or TCFDB
--           open tc file [OWRITER, OCREAT]
--           mapM (uncurry $ put tc) kv
--           close tc
-- @
--
-- @
--    getsample :: String -> ByteString -> TCM (Maybe ByteString)
--    getsample file key =
--        do tc <- new :: TCM TCHDB -- alternatively you can use BDB or TCFDB
--           open tc file [OREADER]
--           val <- get tc key
--           close tc
--           return val
-- @
--
-- @
--    main = runTCM (do putsample "foo.tch" [(pack "foo", pack "bar")]
--                      getsample "foo.tch" (pack "foo")) >>=
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
    new       :: TCM a
    delete    :: a -> TCM ()
    open      :: a -> String -> [OpenMode] -> TCM Bool
    close     :: a -> TCM Bool
    put       :: (Storable k, Storable v) => a -> k -> v -> TCM Bool
    putkeep   :: (Storable k, Storable v) => a -> k -> v -> TCM Bool
    putcat    :: (Storable k, Storable v) => a -> k -> v -> TCM Bool
    get       :: (Storable k, Storable v) => a -> k -> TCM (Maybe v)
    out       :: (Storable k) => a -> k -> TCM Bool
    vsiz      :: (Storable k) => a -> k -> TCM (Maybe Int)
    iterinit  :: a -> TCM Bool
    iternext  :: (Storable v) => a -> TCM (Maybe v)
    fwmkeys   :: (Storable k, Storable v) => a -> k -> Int -> TCM [v]
    addint    :: (Storable k) => a -> k -> Int -> TCM (Maybe Int)
    adddouble :: (Storable k) => a -> k -> Double -> TCM (Maybe Double)
    sync      :: a -> TCM Bool
    vanish    :: a -> TCM Bool
    copy      :: a -> String -> TCM Bool
    path      :: a -> TCM (Maybe String)
    rnum      :: a -> TCM Int64
    size      :: a -> TCM Int64
    ecode     :: a -> TCM E.TCECODE
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

instance TCDB H.TCHDB where
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
storableToKey s = toID . strip . show $ s
    where
      strip "" = ""
      strip ('"':[]) = ""
      strip ('"':xs) = strip xs
      strip (x:xs) = x:strip xs

keyToStorable :: (Storable a) => ID -> IO a
keyToStorable k = newCStringLen (show k) >>= \(ptr, len) ->
                  peekPtrLen (castPtr ptr, fromIntegral len)

instance TCDB F.TCFDB where
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
                                      Just x  -> Just `fmap` keyToStorable x
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
