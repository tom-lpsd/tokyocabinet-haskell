module Database.TokyoCabinet.TDB.Query
    (
      Condition(..)
    , OrderType(..)
    , PostTreatment(..)
    , new
    , delete
    , addcond
    , setorder
    , setlimit
    , search
    , searchout
    , hint
    ) where

import Data.Word

import Foreign.C.String

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Storable (pokeByteOff)

import Database.TokyoCabinet.Storable
import Database.TokyoCabinet.Sequence
import Database.TokyoCabinet.TDB.C
import Database.TokyoCabinet.TDB.Query.C

new :: TDB -> IO TDBQRY
new tdb = withForeignPtr (unTCTDB tdb) $ \tdb' ->
          flip TDBQRY tdb `fmap` (c_tctdbqrynew tdb' >>= newForeignPtr tctdbqryFinalizer)

delete :: TDBQRY -> IO ()
delete qry = finalizeForeignPtr (unTDBQRY qry)

addcond :: (Storable k, Storable v) => TDBQRY -> k -> Condition -> v -> IO ()
addcond qry name op expr =
    withForeignPtr (unTDBQRY qry) $ \qry' ->
        withPtrLen name $ \(name', nlen) ->
        withPtrLen expr $ \(expr', elen) ->
            do pokeByteOff name' (fromIntegral nlen) (0 :: Word8)
               pokeByteOff expr' (fromIntegral elen) (0 :: Word8)
               c_tctdbqryaddcond qry' (castPtr name') (condToCInt op) (castPtr expr')


setorder :: (Storable k) => TDBQRY -> k -> OrderType -> IO ()
setorder qry name otype =
    withForeignPtr (unTDBQRY qry) $ \qry' ->
        withPtrLen name $ \(name', nlen) ->
            do pokeByteOff name' (fromIntegral nlen) (0 :: Word8)
               c_tctdbqrysetorder qry' (castPtr name') (orderToCInt otype)

setlimit :: TDBQRY -> Int -> Int -> IO ()
setlimit qry maxn skip =
    withForeignPtr (unTDBQRY qry) $ \qry' ->
        c_tctdbqrysetlimit qry' (fromIntegral maxn) (fromIntegral skip)

search :: (Storable k, Sequence q) => TDBQRY -> IO (q k)
search qry = withForeignPtr (unTDBQRY qry) $ (>>= peekList') . c_tctdbqrysearch

searchout :: TDBQRY -> IO Bool
searchout qry = withForeignPtr (unTDBQRY qry) c_tctdbqrysearchout

hint :: TDBQRY -> IO String
hint qry = withForeignPtr (unTDBQRY qry) $ \qry' -> c_tctdbqryhint qry' >>= peekCString
