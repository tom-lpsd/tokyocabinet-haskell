{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MyApp2.TokyoCabinet
    (
      TCDB
    , HDB
    , BDB
    , FDB
    , TCM
    , runTCM
    , new
    , open
    , close
    , put
    , get
    , TC.defaultExtension
    , ECODE(..)
    , OpenMode(..)
    ) where

import Control.Monad.Error
import Control.Monad.Reader

import Database.TokyoCabinet
    (
      OpenMode(..)
    , TCDB
    , HDB
    , BDB
    , FDB
    )

import Database.TokyoCabinet.Error
import Database.TokyoCabinet.Storable
import qualified Database.TokyoCabinet as TC

instance Error ECODE where
    noMsg = ESUCCESS
    strMsg "ESUCCESS" = ESUCCESS
    strMsg "ETHREAD" = ETHREAD
    strMsg "EINVALID" = EINVALID
    strMsg "ENOFILE" = ENOFILE
    strMsg "ENOPERM" = ENOPERM
    strMsg "EMETA" = EMETA
    strMsg "ERHEAD" = ERHEAD
    strMsg "EOPEN" = EOPEN
    strMsg "ECLOSE" = ECLOSE
    strMsg "ETRUNC" = ETRUNC
    strMsg "ESYNC" = ESYNC
    strMsg "ESTAT" = ESTAT
    strMsg "ESEEK" = ESEEK
    strMsg "EREAD" = EREAD
    strMsg "EWRITE" = EWRITE
    strMsg "EMMAP" = EMMAP
    strMsg "ELOCK" = ELOCK
    strMsg "EUNLINK" = EUNLINK
    strMsg "ERENAME" = ERENAME
    strMsg "EMKDIR" = EMKDIR
    strMsg "ERMDIR" = ERMDIR
    strMsg "EKEEP" = EKEEP
    strMsg "ENOREC" = ENOREC
    strMsg _ = EMISC

newtype TCM tc a = TCM { unTCM :: ErrorT ECODE (ReaderT tc TC.TCM) a }
    deriving (Monad, MonadIO, MonadReader tc, MonadError ECODE)

runTCM :: (TCDB tc) => TCM tc a -> tc -> IO (Either ECODE a)
runTCM tcm tc = TC.runTCM $ runReaderT (runErrorT $ unTCM tcm) tc

liftT :: (TCDB tc) => TC.TCM a -> TCM tc a
liftT = TCM . lift . lift

throwTCError :: (TCDB tc) => tc -> Bool -> TCM tc ()
throwTCError tc False = (liftT $ TC.ecode tc) >>= throwError
throwTCError _ _ = return ()

new :: (TCDB tc) => IO tc
new = TC.runTCM TC.new

open :: (TCDB tc) => String -> [OpenMode] -> TCM tc ()
open name mode = do tc <- ask
                    (liftT $ TC.open tc name mode) >>= throwTCError tc

close :: (TCDB tc) => TCM tc ()
close = do tc <- ask
           (liftT $ TC.close tc) >>= throwTCError tc

put :: (TCDB tc, Storable k, Storable v) => k -> v -> TCM tc ()
put key val = do tc <- ask
                 (liftT $ TC.put tc key val) >>= throwTCError tc

get :: (TCDB tc, Storable k, Storable v) => k -> TCM tc (Maybe v)
get key = do tc <- ask
             liftT $ TC.get tc key
