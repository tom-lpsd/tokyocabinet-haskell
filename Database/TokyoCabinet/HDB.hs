module Database.TokyoCabinet.HDB
    (
    -- * constants
      TCECODE(..)
    , OpenMode(..)
    , TuningOption(..)
    -- * basic API
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
    , putasync
    , out
    , get
    , vsiz
    , iterinit
    , iternext
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
    , TCHDB
    )
    where

import Foreign.Storable (peek)
import Foreign.ForeignPtr
import Foreign.Marshal (alloca)
import Foreign.Marshal.Utils (maybePeek)

import Data.Int

import Database.TokyoCabinet.HDB.C
import Database.TokyoCabinet.Error
import Database.TokyoCabinet.Internal
import qualified Database.TokyoCabinet.Storable as S

data TCHDB = TCHDB { unTCHDB :: !(ForeignPtr HDB) }

new :: IO TCHDB
new = TCHDB `fmap` (c_tchdbnew >>= newForeignPtr tchdbFinalizer)

delete :: TCHDB -> IO ()
delete hdb = finalizeForeignPtr (unTCHDB hdb)

ecode :: TCHDB -> IO TCECODE
ecode hdb = cintToError `fmap` withForeignPtr (unTCHDB hdb) c_tchdbecode

tune :: TCHDB -> Int64 -> Int8 -> Int8 -> [TuningOption] -> IO Bool
tune hdb bnum apow fpow options =
    withForeignPtr (unTCHDB hdb) $ \p ->
        c_tchdbtune p bnum apow fpow (combineTuningOption options)

setcache :: TCHDB -> Int32 -> IO Bool
setcache hdb rcnum = withForeignPtr (unTCHDB hdb) (flip c_tchdbsetcache rcnum)

setxmsiz :: TCHDB -> Int64 -> IO Bool
setxmsiz hdb xmsiz = withForeignPtr (unTCHDB hdb) (flip c_tchdbsetxmsiz xmsiz)

open :: TCHDB -> String -> [OpenMode] -> IO Bool
open = openHelper c_tchdbopen unTCHDB combineOpenMode

close :: TCHDB -> IO Bool
close hdb = withForeignPtr (unTCHDB hdb) c_tchdbclose

put :: (S.Storable a, S.Storable b) => TCHDB -> a -> b -> IO Bool
put = putHelper c_tchdbput unTCHDB

putkeep :: (S.Storable a, S.Storable b) => TCHDB -> a -> b -> IO Bool
putkeep = putHelper c_tchdbputkeep unTCHDB

putcat :: (S.Storable a, S.Storable b) => TCHDB -> a -> b -> IO Bool
putcat = putHelper c_tchdbputcat unTCHDB

putasync :: (S.Storable a, S.Storable b) => TCHDB -> a -> b -> IO Bool
putasync = putHelper c_tchdbputasync unTCHDB

out :: (S.Storable a) => TCHDB -> a -> IO Bool
out = outHelper c_tchdbout unTCHDB

get :: (S.Storable a, S.Storable b) => TCHDB -> a -> IO (Maybe b)
get = getHelper c_tchdbget unTCHDB

vsiz :: (S.Storable a) => TCHDB -> a -> IO (Maybe Int)
vsiz = vsizHelper c_tchdbvsiz unTCHDB

iterinit :: TCHDB -> IO Bool
iterinit hdb = withForeignPtr (unTCHDB hdb) c_tchdbiterinit

iternext :: (S.Storable a) => TCHDB -> IO (Maybe a)
iternext hdb =
    withForeignPtr (unTCHDB hdb) $ \p ->
        alloca $ \sizbuf -> do
            vbuf <- c_tchdbiternext p sizbuf
            flip maybePeek vbuf $ \vp ->
                do siz <- peek sizbuf
                   S.peekPtrLen (vp, siz)

fwmkeys :: (S.Storable a, S.Storable b) => TCHDB -> a -> Int -> IO [b]
fwmkeys = fwmHelper c_tchdbfwmkeys unTCHDB

addint :: (S.Storable a) => TCHDB -> a -> Int -> IO (Maybe Int)
addint = addHelper c_tchdbaddint unTCHDB fromIntegral fromIntegral (== cINT_MIN)

adddouble :: (S.Storable a) => TCHDB -> a -> Double -> IO (Maybe Double)
adddouble = addHelper c_tchdbadddouble unTCHDB realToFrac realToFrac isNaN

sync :: TCHDB -> IO Bool
sync hdb = withForeignPtr (unTCHDB hdb) c_tchdbsync

optimize :: TCHDB -> Int64 -> Int8 -> Int8 -> [TuningOption] -> IO Bool
optimize hdb bnum apow fpow options = 
    withForeignPtr (unTCHDB hdb) $ \p ->
        c_tchdboptimize p bnum apow fpow (combineTuningOption options)

vanish :: TCHDB -> IO Bool
vanish hdb = withForeignPtr (unTCHDB hdb) c_tchdbvanish

copy :: TCHDB -> String -> IO Bool
copy = copyHelper c_tchdbcopy unTCHDB

tranbegin :: TCHDB -> IO Bool
tranbegin hdb = withForeignPtr (unTCHDB hdb) c_tchdbtranbegin

trancommit :: TCHDB -> IO Bool
trancommit hdb = withForeignPtr (unTCHDB hdb) c_tchdbtrancommit

tranabort :: TCHDB -> IO Bool
tranabort hdb = withForeignPtr (unTCHDB hdb) c_tchdbtranabort

path :: TCHDB -> IO (Maybe String)
path = pathHelper c_tchdbpath unTCHDB

rnum :: TCHDB -> IO Int64
rnum hdb = withForeignPtr (unTCHDB hdb) c_tchdbrnum

fsiz :: TCHDB -> IO Int64
fsiz hdb = withForeignPtr (unTCHDB hdb) c_tchdbfsiz
