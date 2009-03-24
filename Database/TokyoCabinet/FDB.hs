module Database.TokyoCabinet.FDB
    (
    -- * constants
      TCECODE(..)
    , OpenMode(..)
    , ID(..)
    -- * basic api
    , new
    , delete
    , ecode
    , errmsg
    , tune
    , open
    , close
    , put
    , putkeep
    , putcat
    , out
    , get
    , vsiz
    , iterinit
    , iternext
    , range
    , fwmkeys
    , addint
    , adddouble
    , sync
    , optimize
    , vanish
    , copy
    , path
    , rnum
    , fsiz
    , TCFDB
    ) where

import Database.TokyoCabinet.Error
import Database.TokyoCabinet.FDB.C
import Database.TokyoCabinet.FDB.Key
import Database.TokyoCabinet.Internal (peekTCListAndFree)
import qualified Database.TokyoCabinet.Storable as S

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable (peek)
import Foreign.Marshal (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (maybePeek)

import Data.Int
import Data.Word

data TCFDB = TCFDB { unTCFDB :: !(ForeignPtr FDB) }

new :: IO TCFDB
new = do fdb <- c_tcfdbnew
         TCFDB `fmap` newForeignPtr tcfdbFinalizer fdb

delete :: TCFDB -> IO ()
delete fdb = finalizeForeignPtr $ unTCFDB fdb

ecode :: TCFDB -> IO TCECODE
ecode fdb =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        cintToError `fmap` c_tcfdbecode fdb'

tune :: TCFDB -> Int32 -> Int64 -> IO Bool
tune fdb width limsiz =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> c_tcfdbtune fdb' width limsiz

open :: TCFDB -> String -> [OpenMode] -> IO Bool
open fdb fpath modes =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        withCString fpath $ \fpath' ->
            c_tcfdbopen fdb' fpath' (combineOpenMode modes)

close :: TCFDB -> IO Bool
close fdb = withForeignPtr (unTCFDB fdb) c_tcfdbclose

type PutFunc = Ptr FDB -> Int64 -> Ptr Word8 -> CInt -> IO Bool
liftPutFunc :: (Key a, S.Storable b) => PutFunc -> TCFDB -> a -> b -> IO Bool
liftPutFunc func fdb key val =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        S.withPtrLen val $ \(vbuf, vsize) -> do
          func fdb' (unID . toID $ key) vbuf vsize
        
put :: (Key a, S.Storable b) => TCFDB -> a -> b -> IO Bool
put = liftPutFunc c_tcfdbput

putkeep :: (Key a, S.Storable b) => TCFDB -> a -> b -> IO Bool
putkeep = liftPutFunc c_tcfdbputkeep

putcat :: (Key a, S.Storable b) => TCFDB -> a -> b -> IO Bool
putcat =  liftPutFunc c_tcfdbputcat

out :: (Key a) => TCFDB -> a -> IO Bool
out fdb key =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        c_tcfdbout fdb' $ unID . toID $ key

get :: (Key a, S.Storable b) => TCFDB -> a -> IO (Maybe b)
get fdb key =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        alloca $ \sizbuf -> do
            vbuf  <- c_tcfdbget fdb' (unID . toID $ key) sizbuf
            vsize <- peek sizbuf
            flip maybePeek vbuf $ \vbuf' -> S.peekPtrLen (vbuf', vsize)

vsiz :: (Key a) => TCFDB -> a -> IO (Maybe Int)
vsiz fdb key =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      vsize <- c_tcfdbvsiz fdb' (unID . toID $ key)
      return $ if vsize == (-1)
                 then Nothing
                 else Just (fromIntegral vsize)

iterinit :: TCFDB -> IO Bool
iterinit fdb = withForeignPtr (unTCFDB fdb) c_tcfdbiterinit

iternext :: (Key a) => TCFDB -> IO (Maybe a)
iternext fdb = 
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      i <-  c_tcfdbiternext fdb'
      return $ if i == 0
                 then Nothing
                 else Just (fromID $ ID i)

range :: (Key a, Key b) => TCFDB -> a -> a -> Int -> IO [b]
range fdb lower upper maxn =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        alloca $ \sizbuf -> do
          rp <- c_tcfdbrange fdb' (unID . toID $ lower) (unID . toID $ upper)
                                  (fromIntegral maxn) sizbuf
          size <- fromIntegral `fmap` peek sizbuf
          keys <- peekArray size rp
          free rp
          return $ map (fromID . ID) keys

fwmkeys :: (S.Storable a, S.Storable b) => TCFDB -> a -> Int -> IO [b]
fwmkeys fdb spec maxn =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        S.withPtrLen spec $ \(ptr, len) -> do
          c_tcfdbrange4 fdb' ptr len (fromIntegral maxn)
                            >>= peekTCListAndFree

addint :: (Key a) => TCFDB -> a -> Int -> IO (Maybe Int)
addint fdb key num =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
        sumval <- c_tcfdbaddint fdb' (unID . toID $ key) (fromIntegral num)
        return $ if sumval == cINT_MIN
                   then Nothing
                   else Just $ fromIntegral sumval

adddouble :: (Key a) => TCFDB -> a -> Double -> IO (Maybe Double)
adddouble fdb key num =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
        sumval <- c_tcfdbadddouble fdb' (unID . toID $ key) (realToFrac num)
        return $ if isNaN sumval
                   then Nothing
                   else Just $ realToFrac sumval

sync :: TCFDB -> IO Bool
sync fdb = withForeignPtr (unTCFDB fdb) c_tcfdbsync

optimize :: TCFDB -> Int32 -> Int64 -> IO Bool
optimize fdb width limsiz =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> c_tcfdboptimize fdb' width limsiz

vanish :: TCFDB -> IO Bool
vanish fdb = withForeignPtr (unTCFDB fdb) c_tcfdbvanish

copy :: TCFDB -> String -> IO Bool
copy fdb fpath =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        withCString fpath (c_tcfdbcopy fdb')

path :: TCFDB -> IO (Maybe String)
path fdb =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        c_tcfdbpath fdb' >>= (maybePeek peekCString)

rnum :: TCFDB -> IO Int64
rnum fdb = withForeignPtr (unTCFDB fdb) c_tcfdbrnum

fsiz :: TCFDB -> IO Int64
fsiz fdb = withForeignPtr (unTCFDB fdb) c_tcfdbfsiz
