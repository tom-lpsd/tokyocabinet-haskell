module Database.TokyoCabinet.BDB
    (
    -- * constants
      TCECODE(..)
    , OpenMode(..)
    , TuningOption(..)
    -- * basic api
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
    , TCBDB
    ) where

import Data.Int

import Foreign.C.String
import Foreign.Ptr
import Foreign.ForeignPtr

import Database.TokyoCabinet.Error
import Database.TokyoCabinet.BDB.C
import Database.TokyoCabinet.Internal
import qualified Database.TokyoCabinet.Storable as S

new :: IO TCBDB
new = do
  bdb <- c_tcbdbnew
  TCBDB `fmap` newForeignPtr tcbdbFinalizer bdb

delete :: TCBDB -> IO ()
delete bdb = finalizeForeignPtr (unTCBDB bdb)

ecode :: TCBDB -> IO TCECODE
ecode bdb = cintToError `fmap` withForeignPtr (unTCBDB bdb) c_tcbdbecode

tune :: TCBDB -> Int32 -> Int32
     -> Int64 -> Int8 -> Int8 -> [TuningOption] -> IO Bool
tune bdb lmemb nmemb bnum apow fpow opts =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        c_tcbdbtune bdb' lmemb nmemb bnum apow fpow (combineTuningOption opts)

setcache :: TCBDB -> Int32 -> Int32 -> IO Bool
setcache bdb lcnum ncnum =
    withForeignPtr (unTCBDB bdb) $ \bdb' -> c_tcbdbsetcache bdb' lcnum ncnum

setxmsiz :: TCBDB -> Int64 -> IO Bool
setxmsiz bdb xmsiz =
    withForeignPtr (unTCBDB bdb) $ \bdb' -> c_tcbdbsetxmsiz bdb' xmsiz

open :: TCBDB -> String -> [OpenMode] -> IO Bool
open = openHelper c_tcbdbopen unTCBDB combineOpenMode

close :: TCBDB -> IO Bool
close bdb = withForeignPtr (unTCBDB bdb) c_tcbdbclose

put :: (S.Storable a, S.Storable b) => TCBDB -> a -> b -> IO Bool
put = putHelper c_tcbdbput unTCBDB

putkeep :: (S.Storable a, S.Storable b) => TCBDB -> a -> b -> IO Bool
putkeep = putHelper c_tcbdbputkeep unTCBDB

putcat :: (S.Storable a, S.Storable b) => TCBDB -> a -> b -> IO Bool
putcat = putHelper c_tcbdbputcat unTCBDB

putdup :: (S.Storable a, S.Storable b) => TCBDB -> a -> b -> IO Bool
putdup = putHelper c_tcbdbputdup unTCBDB

putlist :: (S.Storable a, S.Storable b) => TCBDB -> a -> [b] -> IO Bool
putlist bdb key vals = do
  and `fmap` mapM (putdup bdb key) vals

out :: (S.Storable a) => TCBDB -> a -> IO Bool
out = outHelper c_tcbdbout unTCBDB

outlist :: (S.Storable a) => TCBDB -> a -> IO Bool
outlist bdb key =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        S.withPtrLen key $ \(kbuf, ksize) -> c_tcbdbout3 bdb' kbuf ksize

get :: (S.Storable a, S.Storable b) => TCBDB -> a -> IO (Maybe b)
get = getHelper c_tcbdbget unTCBDB

getlist :: (S.Storable a, S.Storable b) => TCBDB -> a -> IO [b]
getlist bdb key =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        S.withPtrLen key $ \(kbuf, ksize) -> do
          ptr <- c_tcbdbget4 bdb' kbuf ksize
          if ptr == nullPtr
            then return []
            else peekTCListAndFree ptr

vnum :: (S.Storable a) => TCBDB -> a -> IO (Maybe Int)
vnum bdb key =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        S.withPtrLen key $ \(kbuf, ksize) -> do
            res <- c_tcbdbvnum bdb' kbuf ksize
            return $ if res == 0
                       then Nothing
                       else Just $ fromIntegral res

vsiz :: (S.Storable a) => TCBDB -> a -> IO (Maybe Int)
vsiz bdb key =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        S.withPtrLen key $ \(kbuf, ksize) -> do
            res <- c_tcbdbvsiz bdb' kbuf ksize
            return $ if res == -1
                       then Nothing
                       else Just $ fromIntegral res

range :: (S.Storable a)
         => TCBDB -> Maybe a -> Bool
                  -> Maybe a -> Bool -> Int -> IO [a]
range bdb bkey binc ekey einc maxn =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        withPtrLen' bkey $ \(bkbuf, bksiz) ->
        withPtrLen' ekey $ \(ekbuf, eksiz) ->
            c_tcbdbrange bdb' bkbuf bksiz binc ekbuf eksiz einc
                             (fromIntegral maxn) >>= peekTCListAndFree
    where
      withPtrLen' (Just key) action = S.withPtrLen key action
      withPtrLen' Nothing action = action (nullPtr, 0)

fwmkeys :: (S.Storable a, S.Storable b) => TCBDB -> a -> Int -> IO [b]
fwmkeys bdb prefix maxn =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        S.withPtrLen prefix $ \(pbuf, psiz) ->
            c_tcbdbfwmkeys bdb' pbuf psiz (fromIntegral maxn)
                               >>= peekTCListAndFree

addint :: (S.Storable a) => TCBDB -> a -> Int -> IO (Maybe Int)
addint = addHelper c_tcbdbaddint unTCBDB fromIntegral fromIntegral (== cINT_MIN)

adddouble :: (S.Storable a) => TCBDB -> a -> Double -> IO (Maybe Double)
adddouble = addHelper c_tcbdbadddouble unTCBDB realToFrac realToFrac isNaN

sync :: TCBDB -> IO Bool
sync bdb = withForeignPtr (unTCBDB bdb) c_tcbdbsync

optimize :: TCBDB -> Int32 -> Int32
         -> Int64 -> Int8 -> Int8 -> [TuningOption] -> IO Bool
optimize bdb lmemb nmemb bnum apow fpow opts =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        c_tcbdboptimize bdb' lmemb nmemb bnum apow fpow
                        (combineTuningOption opts)

vanish :: TCBDB -> IO Bool
vanish bdb = withForeignPtr (unTCBDB bdb) c_tcbdbvanish

copy :: TCBDB -> String -> IO Bool
copy bdb fpath =
    withForeignPtr (unTCBDB bdb) $ \bdb' ->
        withCString fpath (c_tcbdbcopy bdb')

tranbegin :: TCBDB -> IO Bool
tranbegin bdb = withForeignPtr (unTCBDB bdb) c_tcbdbtranbegin

trancommit :: TCBDB -> IO Bool
trancommit bdb = withForeignPtr (unTCBDB bdb) c_tcbdbtrancommit

tranabort :: TCBDB -> IO Bool
tranabort bdb = withForeignPtr (unTCBDB bdb) c_tcbdbtranabort

path :: TCBDB -> IO (Maybe String)
path = pathHelper c_tcbdbpath unTCBDB

rnum :: TCBDB -> IO Int64
rnum bdb = withForeignPtr (unTCBDB bdb) c_tcbdbrnum

fsiz :: TCBDB -> IO Int64
fsiz bdb = withForeignPtr (unTCBDB bdb) c_tcbdbfsiz
