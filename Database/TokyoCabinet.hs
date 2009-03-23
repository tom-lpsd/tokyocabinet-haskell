{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Database.TokyoCabinet
    (
      TCM
    , runTCM
    , OpenMode(..)
    , TCDB(..)
    , H.TCHDB
    , F.TCFDB
    , B.TCBDB
    , TCECODE(..)
    , errmsg
    ) where

import Control.Monad.Trans (MonadIO)

import Database.TokyoCabinet.Error (TCECODE(..), errmsg)
import Database.TokyoCabinet.Storable
import Database.TokyoCabinet.FDB.Key
import qualified Database.TokyoCabinet.HDB as H
import qualified Database.TokyoCabinet.FDB as F
import qualified Database.TokyoCabinet.BDB as B

import Foreign.Ptr (castPtr)
import Foreign.C.String (newCStringLen)

import Data.Int

newtype TCM a = TCM { runTCM :: IO a } deriving (Monad, MonadIO)

data OpenMode = OREADER |
                OWRITER |
                OCREAT  |
                OTRUNC  |
                ONOLCK  |
                OLCKNB
                deriving (Eq, Ord, Show)

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
    ecode     :: a -> TCM TCECODE

openModeToHOpenMode :: OpenMode -> H.OpenMode
openModeToHOpenMode OREADER = H.OREADER
openModeToHOpenMode OWRITER = H.OWRITER
openModeToHOpenMode OCREAT  = H.OCREAT
openModeToHOpenMode OTRUNC  = H.OTRUNC
openModeToHOpenMode ONOLCK  = H.ONOLCK
openModeToHOpenMode OLCKNB  = H.OLCKNB

instance TCDB H.TCHDB where
    new                    = TCM $ H.new
    delete                 = TCM . H.delete
    open tc name mode      = TCM $ H.open tc name (map openModeToHOpenMode mode)
    close                  = TCM . H.close
    put tc key val         = TCM $ H.put tc key val
    putkeep tc key val     = TCM $ H.putkeep tc key val
    putcat tc key val      = TCM $ H.putcat tc key val
    get tc key             = TCM $ H.get tc key
    out tc key             = TCM $ H.out tc key
    vsiz tc key            = TCM $ H.vsiz tc key
    iterinit               = TCM . H.iterinit
    iternext               = TCM . H.iternext
    fwmkeys tc prefix maxn = TCM $ H.fwmkeys tc prefix maxn
    addint tc key num      = TCM $ H.addint tc key num
    adddouble tc key num   = TCM $ H.adddouble tc key num
    sync                   = TCM . H.sync
    vanish                 = TCM . H.vanish
    copy tc fpath          = TCM $ H.copy tc fpath
    path                   = TCM . H.path
    rnum                   = TCM . H.rnum
    size                   = TCM . H.fsiz
    ecode                  = TCM . H.ecode

openModeToBOpenMode :: OpenMode -> B.OpenMode
openModeToBOpenMode OREADER = B.OREADER
openModeToBOpenMode OWRITER = B.OWRITER
openModeToBOpenMode OCREAT  = B.OCREAT
openModeToBOpenMode OTRUNC  = B.OTRUNC
openModeToBOpenMode ONOLCK  = B.ONOLCK
openModeToBOpenMode OLCKNB  = B.OLCKNB

instance TCDB B.TCBDB where
    new                    = TCM $ B.new
    delete                 = TCM . B.delete
    open tc name mode      = TCM $ B.open tc name (map openModeToBOpenMode mode)
    close                  = TCM . B.close
    put tc key val         = TCM $ B.put tc key val
    putkeep tc key val     = TCM $ B.putkeep tc key val
    putcat tc key val      = TCM $ B.putcat tc key val
    get tc key             = TCM $ B.get tc key
    out tc key             = TCM $ B.out tc key
    vsiz tc key            = TCM $ B.vsiz tc key
    iterinit               = undefined
    iternext               = undefined
    fwmkeys tc prefix maxn = TCM $ B.fwmkeys tc prefix maxn
    addint tc key num      = TCM $ B.addint tc key num
    adddouble tc key num   = TCM $ B.adddouble tc key num
    sync                   = TCM . B.sync
    vanish                 = TCM . B.vanish
    copy tc fpath          = TCM $ B.copy tc fpath
    path                   = TCM . B.path
    rnum                   = TCM . B.rnum
    size                   = TCM . B.fsiz
    ecode                  = TCM . B.ecode

openModeToFOpenMode :: OpenMode -> F.OpenMode
openModeToFOpenMode OREADER = F.OREADER
openModeToFOpenMode OWRITER = F.OWRITER
openModeToFOpenMode OCREAT  = F.OCREAT
openModeToFOpenMode OTRUNC  = F.OTRUNC
openModeToFOpenMode ONOLCK  = F.ONOLCK
openModeToFOpenMode OLCKNB  = F.OLCKNB

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
    new                    = TCM $ F.new
    delete                 = TCM . F.delete
    open tc name mode      = TCM $ F.open tc name (map openModeToFOpenMode mode)
    close                  = TCM . F.close
    put tc key val         = TCM $ F.put tc (storableToKey key) val
    putkeep tc key val     = TCM $ F.putkeep tc (storableToKey key) val
    putcat tc key val      = TCM $ F.putcat tc (storableToKey key) val
    get tc key             = TCM $ F.get tc (storableToKey key)
    out tc key             = TCM $ F.out tc (storableToKey key)
    vsiz tc key            = TCM $ F.vsiz tc (storableToKey key)
    iterinit               = TCM . F.iterinit
    iternext tc            = TCM $ do key <- F.iternext tc
                                      case key of
                                        Nothing -> return Nothing
                                        Just x  -> Just `fmap` keyToStorable x
    fwmkeys tc prefix maxn = undefined
    addint tc key num      = TCM $ F.addint tc (storableToKey key) num
    adddouble tc key num   = TCM $ F.adddouble tc (storableToKey key) num 
    sync                   = TCM . F.sync
    vanish                 = TCM . F.vanish
    copy tc fpath          = TCM $ F.copy tc fpath
    path                   = TCM . F.path
    rnum                   = TCM . F.rnum
    size                   = TCM . F.fsiz
    ecode                  = TCM . F.ecode
