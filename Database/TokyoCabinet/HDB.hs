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

import Foreign.C.Types (CInt)
import Foreign.C.String

import Foreign.Ptr
import Foreign.Storable (peek)
import Foreign.ForeignPtr
import Foreign.Marshal (alloca)
import Foreign.Marshal.Utils (maybePeek)

import Data.Int
import Data.Word

import Database.TokyoCabinet.HDB.C
import Database.TokyoCabinet.Error
import Database.TokyoCabinet.Internal
import qualified Database.TokyoCabinet.Storable as S

data TCHDB = TCHDB { unTCHDB :: !(ForeignPtr HDB) }

new :: IO TCHDB
new = do ptr <- c_tchdbnew
         fptr <- newForeignPtr tchdbFinalizer ptr
         return $ TCHDB fptr

delete :: TCHDB -> IO ()
delete hdb = finalizeForeignPtr (unTCHDB hdb)

ecode :: TCHDB -> IO TCECODE
ecode hdb = cintToError `fmap` withForeignPtr (unTCHDB hdb) c_tchdbecode

tune :: TCHDB -> Int64 -> Int8 -> Int8 -> [TuningOption] -> IO Bool
tune hdb bnum apow fpow options =
    withForeignPtr (unTCHDB hdb) $ \p ->
        c_tchdbtune p bnum apow fpow (combineTuningOption options)

setcache :: TCHDB -> Int32 -> IO Bool
setcache hdb rcnum =
    withForeignPtr (unTCHDB hdb) $ \p -> c_tchdbsetcache p rcnum

setxmsiz :: TCHDB -> Int64 -> IO Bool
setxmsiz hdb xmsiz =
    withForeignPtr (unTCHDB hdb) $ \p -> c_tchdbsetxmsiz p xmsiz

open :: TCHDB -> String -> [OpenMode] -> IO Bool
open hdb name modes = 
    withForeignPtr (unTCHDB hdb) $ \p ->
        withCString name $ \c_name ->
            c_tchdbopen p c_name (combineOpenMode modes)

close :: TCHDB -> IO Bool
close hdb = withForeignPtr (unTCHDB hdb) c_tchdbclose

type PutFunc = Ptr HDB -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

liftPutFunc ::
    (S.Storable a, S.Storable b) => PutFunc -> TCHDB -> a -> b -> IO Bool
liftPutFunc func hdb key val =
    withForeignPtr (unTCHDB hdb) $ \p ->
        S.withPtrLen key $ \(kbuf, ksize) ->
        S.withPtrLen val $ \(vbuf, vsize) -> func p kbuf ksize vbuf vsize

put :: (S.Storable a, S.Storable b) => TCHDB -> a -> b -> IO Bool
put = liftPutFunc c_tchdbput

putkeep :: (S.Storable a, S.Storable b) => TCHDB -> a -> b -> IO Bool
putkeep = liftPutFunc c_tchdbputkeep

putcat :: (S.Storable a, S.Storable b) => TCHDB -> a -> b -> IO Bool
putcat = liftPutFunc c_tchdbputcat

putasync :: (S.Storable a, S.Storable b) => TCHDB -> a -> b -> IO Bool
putasync = liftPutFunc c_tchdbputasync

out :: (S.Storable a) => TCHDB -> a -> IO Bool
out hdb key =
    withForeignPtr (unTCHDB hdb) $ \p ->
        S.withPtrLen key $ \(kbuf, ksiz) ->
            c_tchdbout p kbuf ksiz

get :: (S.Storable a, S.Storable b) => TCHDB -> a -> IO (Maybe b)
get hdb key =
    withForeignPtr (unTCHDB hdb) $ \p ->
        S.withPtrLen key $ \(kbuf, ksiz) ->
            alloca $ \sizbuf -> do
                vbuf <- c_tchdbget p kbuf ksiz sizbuf
                flip maybePeek vbuf $ \vp ->
                    do siz <- peek sizbuf
                       S.peekPtrLen (vp, siz)

vsiz :: (S.Storable a) => TCHDB -> a -> IO (Maybe Int)
vsiz hdb key =
    withForeignPtr (unTCHDB hdb) $ \p ->
        S.withPtrLen key $ \(kbuf, ksiz) -> do
          vsize <- c_tchdbvsiz p kbuf ksiz
          return $ if vsize == -1
                     then Nothing
                     else Just (fromIntegral vsize)

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
fwmkeys hdb key maxn = 
    withForeignPtr (unTCHDB hdb) $ \p ->
        S.withPtrLen key $ \(kbuf, ksiz) ->
            c_tchdbfwmkeys p kbuf ksiz (fromIntegral maxn)
                           >>= peekTCListAndFree

addint :: (S.Storable a) => TCHDB -> a -> Int -> IO (Maybe Int)
addint hdb key num =
    withForeignPtr (unTCHDB hdb) $ \p ->
        S.withPtrLen key $ \(kbuf, ksiz) -> do
            sumval <- c_tchdbaddint p kbuf ksiz (fromIntegral num)
            return $ if sumval == cINT_MIN
                       then Nothing
                       else Just $ fromIntegral sumval

adddouble :: (S.Storable a) => TCHDB -> a -> Double -> IO (Maybe Double)
adddouble hdb key num =
    withForeignPtr (unTCHDB hdb) $ \p ->
        S.withPtrLen key $ \(kbuf, ksiz) -> do
            sumval <- c_tchdbadddouble p kbuf ksiz (realToFrac num)
            return $ if isNaN sumval
                       then Nothing
                       else Just $ realToFrac sumval

sync :: TCHDB -> IO Bool
sync hdb = withForeignPtr (unTCHDB hdb) c_tchdbsync

optimize :: TCHDB -> Int64 -> Int8 -> Int8 -> [TuningOption] -> IO Bool
optimize hdb bnum apow fpow options = 
    withForeignPtr (unTCHDB hdb) $ \p ->
        c_tchdboptimize p bnum apow fpow (combineTuningOption options)

vanish :: TCHDB -> IO Bool
vanish hdb = withForeignPtr (unTCHDB hdb) c_tchdbvanish

copy :: TCHDB -> String -> IO Bool
copy hdb pathname =
    withForeignPtr (unTCHDB hdb) $ \p ->
        withCString pathname $ \c_pathname ->
            c_tchdbcopy p c_pathname

tranbegin :: TCHDB -> IO Bool
tranbegin hdb = withForeignPtr (unTCHDB hdb) c_tchdbtranbegin

trancommit :: TCHDB -> IO Bool
trancommit hdb = withForeignPtr (unTCHDB hdb) c_tchdbtrancommit

tranabort :: TCHDB -> IO Bool
tranabort hdb = withForeignPtr (unTCHDB hdb) c_tchdbtranabort

path :: TCHDB -> IO (Maybe String)
path hdb =
    withForeignPtr (unTCHDB hdb) $ \p -> do
      cstr <- c_tchdbpath p
      maybePeek peekCString cstr

rnum :: TCHDB -> IO Int64
rnum hdb = withForeignPtr (unTCHDB hdb) c_tchdbrnum

fsiz :: TCHDB -> IO Int64
fsiz hdb = withForeignPtr (unTCHDB hdb) c_tchdbfsiz
