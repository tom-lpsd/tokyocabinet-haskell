{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TCRError where

import Control.Monad.Reader
import Control.Monad.Error

import Database.TokyoCabinet
    (
      TCM
    , runTCM
    , new
    , OpenMode(..)
    , TCDB
    , HDB
    , BDB
    , FDB
    )

import Database.TokyoCabinet.Storable
import qualified Database.TokyoCabinet as TC

newtype TCRError tc e a =
    TCRError { runTCRError :: ErrorT e (ReaderT tc TCM) a}
             deriving (Monad, MonadReader tc, MonadError e)

runTCRE :: TCRError tc e a -> tc -> TCM (Either e a)
runTCRE = runReaderT . runErrorT . runTCRError

liftT :: (Error e) => TCM a -> TCRError tc e a
liftT = TCRError . lift . lift

open :: (TCDB tc) => String -> [OpenMode] -> TCRError tc String ()
open name mode = do tc <- ask
                    res <- liftT $ TC.open tc name mode
                    if res then return () else throwError "open failed"


close :: (TCDB tc) => TCRError tc String ()
close = ask >>= liftT . TC.close >>= \res ->
        if res then return () else throwError "close failed"

put :: (TCDB tc) => String -> String -> TCRError tc String ()
put key val = do tc <- ask
                 res <- liftT $ TC.put tc key val
                 if res then return () else throwError "put failed"

get :: (TCDB tc) => String -> TCRError tc String (Maybe String)
get key = do tc <- ask
             liftT $ TC.get tc key
             

kvstore :: (TCDB tc) => [(String, String)] -> TCRError tc String ()
kvstore kv = do open "abcd.tch" [OREADER]
                mapM_ (uncurry put) kv
                close

main :: IO ()
main = runTCM $ do h <- new :: TCM BDB
                   let kv = [("foo", "100"), ("bar", "200")]
                   flip runTCRE h $ catchError (kvstore kv) (\e -> error e)
       >> return ()
