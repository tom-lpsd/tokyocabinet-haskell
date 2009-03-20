{-# LANGUAGE MultiParamTypeClasses #-}
module Database.TokyoCabinet where

import Database.TokyoCabinet.Storable
import Database.TokyoCabinet.FDB.Key
import qualified Database.TokyoCabinet.HDB as H
import qualified Database.TokyoCabinet.FDB as F
import qualified Database.TokyoCabinet.BDB as B

import Data.Int

type TCM = IO

data OpenMode =
    OREADER |
    OWRITER |
    OCREAT  |
    OTRUNC  |
    ONOLCK  |
    OLCKNB
    deriving (Eq, Ord, Show)

class TokyoCabinet a where
    new     :: TCM a
    delete  :: a -> TCM ()
    open    :: a -> String -> [OpenMode] -> TCM Bool
    close   :: a -> TCM Bool
    put     :: (Storable k, Storable v) => a -> k -> v -> TCM Bool
    putkeep :: (Storable k, Storable v) => a -> k -> v -> TCM Bool
    putcat  :: (Storable k, Storable v) => a -> k -> v -> TCM Bool
    get     :: (Storable k, Storable v) => a -> k -> TCM (Maybe v)
    out     :: (Storable k) => a -> k -> TCM Bool
    vsiz    :: (Storable k) => a -> k -> TCM (Maybe Int)
    iterinit :: a -> TCM Bool
    iternext :: (Storable v) => a -> TCM (Maybe v)
    fwmkeys :: (Storable k, Storable v) => a -> k -> Int -> TCM [v]
    addint :: (Storable k) => a -> k -> Int -> TCM (Maybe Int)
    adddouble :: (Storable k) => a -> k -> Double -> TCM (Maybe Double)
    sync :: a -> TCM Bool
    vanish :: a -> TCM Bool
    copy :: a -> String -> TCM Bool
    path :: a -> TCM (Maybe String)
    rnum :: a -> TCM Int64
    size :: a -> TCM Int64

hOpenModeToOpenMode :: OpenMode -> H.OpenMode
hOpenModeToOpenMode OREADER = H.OREADER
hOpenModeToOpenMode OWRITER = H.OWRITER
hOpenModeToOpenMode OCREAT  = H.OCREAT
hOpenModeToOpenMode OTRUNC  = H.OTRUNC
hOpenModeToOpenMode ONOLCK  = H.ONOLCK
hOpenModeToOpenMode OLCKNB  = H.OLCKNB

instance TokyoCabinet H.TCHDB where
    new = H.new
    delete = H.delete
    open tc name modes = H.open tc name (map hOpenModeToOpenMode modes)
    close = H.close
    put = H.put
    putkeep = H.putkeep
    putcat = H.putcat
    get = H.get
    out = H.out
    vsiz = H.vsiz
    iterinit = H.iterinit
    iternext = H.iternext
    fwmkeys = H.fwmkeys
    addint = H.addint
    adddouble = H.adddouble
    sync = H.sync
    vanish = H.vanish
    copy = H.copy
    path = H.path
    rnum = H.rnum
    size = H.fsiz

bOpenModeToOpenMode :: OpenMode -> B.OpenMode
bOpenModeToOpenMode OREADER = B.OREADER
bOpenModeToOpenMode OWRITER = B.OWRITER
bOpenModeToOpenMode OCREAT  = B.OCREAT
bOpenModeToOpenMode OTRUNC  = B.OTRUNC
bOpenModeToOpenMode ONOLCK  = B.ONOLCK
bOpenModeToOpenMode OLCKNB  = B.OLCKNB

instance TokyoCabinet B.TCBDB where
    new = B.new
    delete = B.delete
    open tc name modes = B.open tc name (map bOpenModeToOpenMode modes)
    close = B.close
    put = B.put
    putkeep = B.putkeep
    putcat = B.putcat
    get = B.get
    out = B.out
    vsiz = B.vsiz
    iterinit = undefined
    iternext = undefined
    fwmkeys = B.fwmkeys
    addint = B.addint
    adddouble = B.adddouble
    sync = B.sync
    vanish = B.vanish
    copy = B.copy
    path = B.path
    rnum = B.rnum
    size = B.fsiz

fOpenModeToOpenMode :: OpenMode -> F.OpenMode
fOpenModeToOpenMode OREADER = F.OREADER
fOpenModeToOpenMode OWRITER = F.OWRITER
fOpenModeToOpenMode OCREAT  = F.OCREAT
fOpenModeToOpenMode OTRUNC  = F.OTRUNC
fOpenModeToOpenMode ONOLCK  = F.ONOLCK
fOpenModeToOpenMode OLCKNB  = F.OLCKNB

storableToKey :: (Storable a) => a -> ID
storableToKey = undefined

keyToStorable :: (Key a, Storable b) => a -> b
keyToStorable = undefined

instance TokyoCabinet F.TCFDB where
    new = F.new
    delete = F.delete
    open tc name modes = F.open tc name (map fOpenModeToOpenMode modes)
    close = F.close
    put tc key val = F.put tc (storableToKey key) val
    putkeep tc key val = F.putkeep tc (storableToKey key) val
    putcat tc key val = F.putcat tc (storableToKey key) val
    get tc key = F.get tc (storableToKey key)
    out tc key = F.out tc (storableToKey key)
    vsiz tc key = F.vsiz tc (storableToKey key)
    iterinit = F.iterinit
    iternext = undefined
    fwmkeys = undefined
    addint tc key = F.addint tc (storableToKey key)
    adddouble tc key = F.adddouble tc (storableToKey key)
    sync = F.sync
    vanish = F.vanish
    copy = F.copy
    path = F.path
    rnum = F.rnum
    size = F.fsiz
