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
import Database.TokyoCabinet.Internal
import qualified Database.TokyoCabinet.Storable as S

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.Storable (peek)
import Foreign.Marshal (alloca, free)
import Foreign.Marshal.Array (peekArray)
import Foreign.Marshal.Utils (maybePeek)

import Data.Int
import Data.Word

import Control.Exception

data TCFDB = TCFDB { unTCFDB :: !(ForeignPtr FDB) }

new :: IO TCFDB
new = TCFDB `fmap` (c_tcfdbnew >>= newForeignPtr tcfdbFinalizer)

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
open = openHelper c_tcfdbopen unTCFDB combineOpenMode

close :: TCFDB -> IO Bool
close fdb = withForeignPtr (unTCFDB fdb) c_tcfdbclose

type PutFunc = Ptr FDB -> Int64 -> Ptr Word8 -> CInt -> IO Bool
liftPutFunc :: (Key a, S.Storable b) => PutFunc -> TCFDB -> a -> b -> IO Bool
liftPutFunc func fdb key val =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        S.withPtrLen val $ \(vbuf, vsize) -> do
          key' <- keyToInt key
          func fdb' key' vbuf vsize
        
put :: (Key a, S.Storable b) => TCFDB -> a -> b -> IO Bool
put = liftPutFunc c_tcfdbput

putkeep :: (Key a, S.Storable b) => TCFDB -> a -> b -> IO Bool
putkeep = liftPutFunc c_tcfdbputkeep

putcat :: (Key a, S.Storable b) => TCFDB -> a -> b -> IO Bool
putcat =  liftPutFunc c_tcfdbputcat

out :: (Key a) => TCFDB -> a -> IO Bool
out fdb key =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        c_tcfdbout fdb' =<< keyToInt key

get :: (Key a, S.Storable b) => TCFDB -> a -> IO (Maybe b)
get fdb key =
    withForeignPtr (unTCFDB fdb) $ \fdb' ->
        alloca $ \sizbuf -> do
          key' <- keyToInt key
          vbuf  <- c_tcfdbget fdb' key' sizbuf
          vsize <- peek sizbuf
          flip maybePeek vbuf $ \vbuf' -> S.peekPtrLen (vbuf', vsize)

vsiz :: (Key a) => TCFDB -> a -> IO (Maybe Int)
vsiz fdb key =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      vsize <- c_tcfdbvsiz fdb' =<< keyToInt key
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
          [l, u] <- mapM keyToInt [lower, upper]
          rp <- c_tcfdbrange fdb' l u (fromIntegral maxn) sizbuf
          size <- fromIntegral `fmap` peek sizbuf
          keys <- peekArray size rp
          free rp
          return $ map (fromID . ID) keys

fwmkeys :: (S.Storable a, S.Storable b) => TCFDB -> a -> Int -> IO [b]
fwmkeys = fwmHelper c_tcfdbrange4 unTCFDB

addint :: (Key a) => TCFDB -> a -> Int -> IO (Maybe Int)
addint fdb key num =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      key' <- keyToInt key
      sumval <- c_tcfdbaddint fdb' key' (fromIntegral num)
      return $ if sumval == cINT_MIN
                 then Nothing
                 else Just $ fromIntegral sumval

adddouble :: (Key a) => TCFDB -> a -> Double -> IO (Maybe Double)
adddouble fdb key num =
    withForeignPtr (unTCFDB fdb) $ \fdb' -> do
      key' <- keyToInt key
      sumval <- c_tcfdbadddouble fdb' key' (realToFrac num)
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
copy = copyHelper c_tcfdbcopy unTCFDB

path :: TCFDB -> IO (Maybe String)
path = pathHelper c_tcfdbpath unTCFDB

rnum :: TCFDB -> IO Int64
rnum fdb = withForeignPtr (unTCFDB fdb) c_tcfdbrnum

fsiz :: TCFDB -> IO Int64
fsiz fdb = withForeignPtr (unTCFDB fdb) c_tcfdbfsiz

keyToInt :: (Key a) => a -> IO Int64
keyToInt i = catchJust selector (evaluate (unID . toID $ i)) handler
    where
      selector :: ErrorCall -> Maybe ()
      selector e = if show e == "Prelude.read: no parse"
                     then Just ()
                     else Nothing
      handler _ = error "Database.TokyoCabinet.FDB: invalid key"
