{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MyApp1.TokyoCabinet
    (
      TCM
    , runTCM
    , new
    , open
    , close
    , put
    , putcat
    , putkeep
    , get
    , out
    , iterinit
    , iternext
    , fwmkeys
    , rnum
    , size
    , liftT
    , TC.defaultExtension
    , TCDB
    , HDB
    , BDB
    , FDB
    , OpenMode(..)
    ) where

import Data.Int

import Control.Monad
import Control.Monad.Reader

import Database.TokyoCabinet.Storable
import Database.TokyoCabinet (TCDB, FDB, BDB, HDB, OpenMode(..))
import qualified Database.TokyoCabinet as TC

newtype TCM tc a =
    TCM { unTCM :: ReaderT tc TC.TCM a } deriving (Monad, MonadReader tc)

runTCM :: (TCDB tc) => TCM tc a -> tc -> IO a
runTCM tcm tc = TC.runTCM $ runReaderT (unTCM tcm) tc

new :: (TCDB tc) => IO tc
new = TC.runTCM TC.new

liftT :: (TCDB tc) => TC.TCM a -> TCM tc a
liftT = TCM . lift

open :: (TCDB tc) => String -> [OpenMode] -> TCM tc Bool
open name mode = do tc <- ask
                    liftT $ TC.open tc name mode

close :: (TCDB tc) => TCM tc Bool
close = ask >>= liftT . TC.close

put :: (TCDB tc, Storable k, Storable v) => k -> v -> TCM tc Bool
put key val = do tc <- ask
                 liftT $ TC.put tc key val

putcat :: (TCDB tc, Storable k, Storable v) => k -> v -> TCM tc Bool
putcat key val = do tc <- ask
                    liftT $ TC.putcat tc key val

putkeep :: (TCDB tc, Storable k, Storable v) => k -> v -> TCM tc Bool
putkeep key val = do tc <- ask
                     liftT $ TC.putkeep tc key val

get :: (TCDB tc, Storable k, Storable v) => k -> TCM tc (Maybe v)
get key = do tc <- ask
             liftT $ TC.get tc key

out :: (TCDB tc, Storable k) => k -> TCM tc Bool
out key = do tc <- ask
             liftT $ TC.out tc key

iterinit :: (TCDB tc) => TCM tc Bool
iterinit = ask >>= liftT . TC.iterinit

iternext :: (TCDB tc, Storable v) => TCM tc (Maybe v)
iternext = ask >>= liftT . TC.iternext

fwmkeys :: (TCDB tc, Storable k1, Storable k2) => k1 -> Int -> TCM tc [k2]
fwmkeys key maxn = do tc <- ask
                      liftT $ TC.fwmkeys tc key maxn

rnum :: (TCDB tc) => TCM tc Int64
rnum = do tc <- ask
          liftT $ TC.rnum tc

size :: (TCDB tc) => TCM tc Int64
size = do tc <- ask
          liftT $ TC.size tc
