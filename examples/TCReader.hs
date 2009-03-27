{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TCReader where

import Database.TokyoCabinet.Storable
import Control.Monad
import Control.Monad.Reader

import Database.TokyoCabinet
    (
      TCM
    , TCDB
    , HDB
    , BDB
    , FDB
    , new
    , runTCM
    , OpenMode(..)
    )

import qualified Database.TokyoCabinet as TC

newtype TCReader tc a =
    TCReader { runTCR :: ReaderT tc TCM a } deriving (Monad, MonadReader tc)

runTCReader :: TCReader tc a -> tc -> TCM a
runTCReader = runReaderT . runTCR

open :: (TCDB tc) => String -> [OpenMode] -> TCReader tc Bool
open name mode = do tc <- ask
                    TCReader $ lift (TC.open tc name mode)

close :: (TCDB tc) => TCReader tc Bool
close = ask >>= (TCReader . lift . TC.close)

get :: (Storable k, Storable v, TCDB tc) => k -> TCReader tc (Maybe v)
get key = do tc <- ask
             TCReader $ lift (TC.get tc key)

put :: (Storable k, Storable v, TCDB tc) => k  -> v -> TCReader tc Bool
put key val = do tc <- ask
                 TCReader $ lift (TC.put tc key val)

kvstore :: (Storable k, Storable v, TCDB tc) => [(k, v)] -> TCReader tc Bool
kvstore kv = do open "abcd.tch" [OWRITER, OCREAT]
                mapM_ (uncurry put) kv
                close

main :: IO ()
main = runTCM $ do h <- new :: TCM HDB
                   let kv =[ ("foo", 112)
                           , ("bar", 200)
                           , ("baz", 300) ] :: [(String, Int)]
                   runTCReader (kvstore kv) h >> return ()
