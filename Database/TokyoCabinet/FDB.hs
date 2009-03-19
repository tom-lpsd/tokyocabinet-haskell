module Database.TokyoCabinet.FDB
    (
    -- * error type and utility
      TCErrorCode(..)
    -- * open mode
    , oREADER
    , oWRITER
    , oCREAT
    , oTRUNC
    , oNOLCK
    , oLCKNB
    , OpenMode
    -- * id
    , iDMIN
    , iDPREV
    , iDMAX
    , iDNEXT
    , ID
    -- * basic api
    , new
    , delete
    , ecode
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
import Data.Bits
import Data.Word

data TCFDB = TCFDB { unTCFDB :: !(ForeignPtr FDB) }

new :: IO TCFDB
new = do fdb <- c_tcfdbnew
         TCFDB `fmap` newForeignPtr tcfdbFinalizer fdb

delete :: TCFDB -> IO ()
delete fdb = finalizeForeignPtr $ unTCFDB fdb

ecode :: TCFDB -> IO TCErrorCode
ecode fdb =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        cintToError `fmap` c_tcfdbecode fdb'

tune :: TCFDB -> Int32 -> Int64 -> IO Bool
tune fdb width limsiz =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> c_tcfdbtune fdb' width limsiz

open :: TCFDB -> String -> [OpenMode] -> IO Bool
open (TCFDB fdb) fpath modes =
    withForeignPtr fdb $ \fdb' ->
        withCString fpath $ \fpath' ->
            c_tcfdbopen fdb' fpath' (unOpenMode mode)
    where
      combineOpenMode = OpenMode . foldr ((.|.) . unOpenMode) 0
      mode = combineOpenMode modes

close :: TCFDB -> IO Bool
close (TCFDB fdb) = withForeignPtr fdb c_tcfdbclose

type PutFunc = Ptr FDB -> Int64 -> Ptr Word8 -> CInt -> IO Bool
liftPutFunc :: (Key a, S.Storable b) => PutFunc -> TCFDB -> a -> b -> IO Bool
liftPutFunc func (TCFDB fdb) key val =
    withForeignPtr fdb $ \fdb' ->
        S.withPtrLen val $ \(vbuf, vsize) -> do
          func fdb' (unID . toID $ key) vbuf vsize
        
put :: (Key a, S.Storable b) => TCFDB -> a -> b -> IO Bool
put = liftPutFunc c_tcfdbput

putkeep :: (Key a, S.Storable b) => TCFDB -> a -> b -> IO Bool
putkeep = liftPutFunc c_tcfdbputkeep

putcat :: (Key a, S.Storable b) => TCFDB -> a -> b -> IO Bool
putcat =  liftPutFunc c_tcfdbputcat

out :: (Key a) => TCFDB -> a -> IO Bool
out (TCFDB fdb) key =
    withForeignPtr fdb $ \fdb' -> c_tcfdbout fdb' $ unID . toID $ key

get :: (Key a, S.Storable b) => TCFDB -> a -> IO (Maybe b)
get (TCFDB fdb) key =
    withForeignPtr fdb $ \fdb' ->
        alloca $ \sizbuf -> do
            vbuf  <- c_tcfdbget fdb' (unID . toID $ key) sizbuf
            vsize <- peek sizbuf
            flip maybePeek vbuf $ \vbuf' -> S.peekPtrLen (vbuf', vsize)

vsiz :: (Key a) => TCFDB -> a -> IO (Maybe Int)
vsiz (TCFDB fdb) key =
    withForeignPtr fdb $ \fdb' -> do
      vsize <- c_tcfdbvsiz fdb' (unID . toID $ key)
      return $ if vsize == (-1)
                 then Nothing
                 else Just (fromIntegral vsize)

iterinit :: TCFDB -> IO Bool
iterinit (TCFDB fdb) = withForeignPtr fdb c_tcfdbiterinit

iternext :: (Key a) => TCFDB -> IO (Maybe a)
iternext (TCFDB fdb) = 
    withForeignPtr fdb $ \fdb' -> do
      i <- ID `fmap` c_tcfdbiternext fdb'
      return $ if i == ID 0
                 then Nothing
                 else Just (fromID i)

range :: (Key a, Key b) => TCFDB -> a -> a -> Int -> IO [b]
range (TCFDB fdb) lower upper maxn =
    withForeignPtr fdb $ \fdb' ->
        alloca $ \sizbuf -> do
          rp <- c_tcfdbrange fdb' (unID . toID $ lower) (unID . toID $ upper)
                                  (fromIntegral maxn) sizbuf
          size <- fromIntegral `fmap` peek sizbuf
          keys <- peekArray size rp
          free rp
          return $ map (fromID . ID) keys

addint :: (Key a) => TCFDB -> a -> Int -> IO (Maybe Int)
addint (TCFDB fdb) key num =
    withForeignPtr fdb $ \fdb' -> do
        sumval <- c_tcfdbaddint fdb' (unID . toID $ key) (fromIntegral num)
        return $ if sumval == cINT_MIN
                   then Nothing
                   else Just $ fromIntegral sumval

adddouble :: (Key a) => TCFDB -> a -> Double -> IO (Maybe Double)
adddouble (TCFDB fdb) key num =
    withForeignPtr fdb $ \fdb' -> do
        sumval <- c_tcfdbadddouble fdb' (unID . toID $ key) (realToFrac num)
        return $ if isNaN sumval
                   then Nothing
                   else Just $ realToFrac sumval

sync :: TCFDB -> IO Bool
sync (TCFDB fdb) = withForeignPtr fdb c_tcfdbsync

optimize :: TCFDB -> Int32 -> Int64 -> IO Bool
optimize (TCFDB fdb) width limsiz =
    withForeignPtr fdb $ \fdb' -> c_tcfdboptimize fdb' width limsiz

vanish :: TCFDB -> IO Bool
vanish (TCFDB fdb) = withForeignPtr fdb c_tcfdbvanish

copy :: TCFDB -> String -> IO Bool
copy (TCFDB fdb) fpath =
    withForeignPtr fdb $ \fdb' ->
        withCString fpath (c_tcfdbcopy fdb')

path :: TCFDB -> IO (Maybe String)
path (TCFDB fdb) =
    withForeignPtr fdb $ \fdb' -> c_tcfdbpath fdb' >>= (maybePeek peekCString)

rnum :: TCFDB -> IO Int64
rnum (TCFDB fptr) = withForeignPtr fptr c_tcfdbrnum

fsiz :: TCFDB -> IO Int64
fsiz (TCFDB fptr) = withForeignPtr fptr c_tcfdbfsiz
