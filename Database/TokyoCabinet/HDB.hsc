{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}
module Database.TokyoCabinet.HDB
    (
    -- * error type and utility
      TCErrorCode
    , errmsg
    , eSUCCESS
    , eTHREAD
    , eINVALID
    , eNOFILE
    , eNOPERM
    , eMETA
    , eRHEAD
    , eOPEN
    , eCLOSE
    , eTRUNC
    , eSYNC
    , eSTAT
    , eSEEK
    , eREAD
    , eWRITE
    , eMMAP
    , eLOCK
    , eUNLINK
    , eRENAME
    , eMKDIR
    , eRMDIR
    , eKEEP
    , eNOREC
    , eMISC
     -- * open mode
    , oREADER
    , oWRITER
    , oCREAT
    , oTRUNC
    , oNOLOCK
    , oLCKNB
    , oTSYNC
    -- * tuning options
    , tLARGE
    , tDEFLATE
    , tBZIP
    , tTCBS
    , tEXCODEC
    -- * basic API
    , new
    , ecode
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
    )
    where

import Foreign hiding (new)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal (free)

import Data.Int
import Data.Word
import Data.ByteString (ByteString, useAsCString, packCString)

import Database.TokyoCabinet.Error

#include <tchdb.h>

newtype OpenMode = OpenMode { unOpenMode :: CInt }
    deriving (Eq, Show)

#{enum OpenMode, OpenMode
 , oREADER = HDBOREADER
 , oWRITER = HDBOWRITER
 , oCREAT  = HDBOCREAT
 , oTRUNC  = HDBOTRUNC
 , oNOLOCK = HDBONOLCK
 , oLCKNB  = HDBOLCKNB
 , oTSYNC  = HDBOTSYNC
}

combineOpenMode :: [OpenMode] -> OpenMode
combineOpenMode = OpenMode . foldr ((.|.) . unOpenMode) 0

newtype TuningOption = TuningOption { unTuningOption :: Word8 }
    deriving (Eq, Show)

#{enum TuningOption, TuningOption
 , tLARGE   = HDBTLARGE
 , tDEFLATE = HDBTDEFLATE
 , tBZIP    = HDBTBZIP
 , tTCBS    = HDBTTCBS
 , tEXCODEC = HDBTEXCODEC
}

combineTuningOption :: [TuningOption] -> TuningOption
combineTuningOption = TuningOption . foldr ((.|.) . unTuningOption) 0

data TCHDB = TCHDB !(ForeignPtr HDB)

new :: IO TCHDB
new = do ptr <- c_tchdbnew
         fptr <- newForeignPtr tchdbFinalizer ptr
         return $ TCHDB fptr

ecode :: TCHDB -> IO TCErrorCode
ecode (TCHDB fptr) = withForeignPtr fptr $ \p -> do
                       e <- c_tchdbecode p
                       return $ TCErrorCode e

tune :: TCHDB -> Int64 -> Int8 -> Int8 -> [TuningOption] -> IO Bool
tune (TCHDB fptr) bnum apow fpow options =
    let option = unTuningOption $ combineTuningOption options
    in withForeignPtr fptr $ \p -> c_tchdbtune p bnum apow fpow option

setcache :: TCHDB -> Int32 -> IO Bool
setcache (TCHDB fptr) rnum = withForeignPtr fptr $ \p -> c_tchdbsetcache p rnum

setxmsiz :: TCHDB -> Int64 -> IO Bool
setxmsiz (TCHDB fptr) xmsiz = withForeignPtr fptr $ \p -> c_tchdbsetxmsiz p xmsiz

open :: TCHDB -> String -> [OpenMode] -> IO Bool
open (TCHDB fptr) name modes = 
    withForeignPtr fptr $ \p ->
        withCString name $ \name ->
            c_tchdbopen p name option
    where option = unOpenMode $ combineOpenMode modes

close :: TCHDB -> IO Bool
close (TCHDB fptr) = withForeignPtr fptr c_tchdbclose

put :: TCHDB -> ByteString -> ByteString -> IO Bool
put (TCHDB fptr) key val =
    withForeignPtr fptr $ \p ->
        useAsCString key $ \key ->
        useAsCString val $ \val ->
            c_tchdbput2 p key val

putkeep :: TCHDB -> ByteString -> ByteString -> IO Bool
putkeep (TCHDB fptr) key val =
    withForeignPtr fptr $ \p ->
        useAsCString key $ \key ->
        useAsCString val $ \val ->
            c_tchdbputkeep2 p key val

putcat :: TCHDB -> ByteString -> ByteString -> IO Bool
putcat (TCHDB fptr) key val =
    withForeignPtr fptr $ \p ->
        useAsCString key $ \key ->
        useAsCString val $ \val ->
            c_tchdbputcat2 p key val

putasync :: TCHDB -> ByteString -> ByteString -> IO Bool
putasync = undefined

out :: TCHDB -> ByteString -> IO Bool
out (TCHDB fptr) key =
    withForeignPtr fptr $ \p ->
        useAsCString key $ \key ->
            c_tchdbout2 p key

get :: TCHDB -> ByteString -> IO (Maybe ByteString)
get (TCHDB fptr) key =
    withForeignPtr fptr $ \p ->
        useAsCString key $ \key -> do
          cstr <- c_tchdbget2 p key
          if cstr == nullPtr
            then return Nothing 
            else do val <- packCString cstr
                    free cstr
                    return (Just val)

vsiz :: TCHDB -> ByteString -> IO (Maybe Int)
vsiz = undefined

iterinit :: TCHDB -> IO Bool
iterinit (TCHDB fptr) = withForeignPtr fptr $ \p -> c_tchdbiterinit p

iternext :: TCHDB -> IO (Maybe ByteString)
iternext (TCHDB fptr) =
    withForeignPtr fptr $ \p -> do
      cstr <- c_tchdbiternext2 p
      if cstr == nullPtr
        then return Nothing 
        else do val <- packCString cstr
                free cstr
                return (Just val)

fwmkeys :: TCHDB -> ByteString -> Int -> IO [ByteString]
fwmkeys = undefined

addint :: TCHDB -> ByteString -> Int -> IO Int
addint = undefined

adddouble :: TCHDB -> ByteString -> Double -> IO Double
adddouble = undefined

sync :: TCHDB -> IO ()
sync = undefined

optimize :: TCHDB -> Int64 -> Int8 -> Int8 -> Word8 -> IO Bool
optimize = undefined

vanish :: TCHDB -> IO Bool
vanish = undefined

copy :: TCHDB -> String -> IO Bool
copy = undefined

tranbegin :: TCHDB -> IO Bool
tranbegin = undefined

trancommit :: TCHDB -> IO Bool
trancommit = undefined

tranabort :: TCHDB -> IO Bool
tranabort = undefined

path :: TCHDB -> IO (Maybe String)
path = undefined

rnum :: TCHDB -> IO Int64
rnum = undefined

fsiz :: TCHDB -> IO Int64
fsiz = undefined

data HDB

foreign import ccall "&tchdbdel"
  tchdbFinalizer :: FunPtr (Ptr HDB -> IO ())

foreign import ccall unsafe "tchdbnew"
  c_tchdbnew :: IO (Ptr HDB)

foreign import ccall unsafe "tchdbdel"
  c_tchdbdel :: Ptr HDB -> IO ()

foreign import ccall unsafe "tchdbopen"
  c_tchdbopen :: Ptr HDB -> CString -> CInt -> IO Bool

foreign import ccall unsafe "tchdbclose"
  c_tchdbclose :: Ptr HDB -> IO Bool

foreign import ccall unsafe "tchdbput2"
  c_tchdbput2 :: Ptr HDB -> CString -> CString -> IO Bool

foreign import ccall unsafe "tchdbget2"
  c_tchdbget2 :: Ptr HDB -> CString -> IO (Ptr CChar)

foreign import ccall unsafe "tchdbecode"
  c_tchdbecode :: Ptr HDB -> IO CInt

foreign import ccall unsafe "tchdbtune"
  c_tchdbtune :: Ptr HDB -> Int64 -> Int8 -> Int8 -> Word8 -> IO Bool

foreign import ccall unsafe "tchdbsetcache"
  c_tchdbsetcache :: Ptr HDB -> Int32 -> IO Bool

foreign import ccall unsafe "tchdbsetxmsiz"
  c_tchdbsetxmsiz :: Ptr HDB -> Int64 -> IO Bool

foreign import ccall unsafe "tchdbout2"
  c_tchdbout2 :: Ptr HDB -> CString -> IO Bool

foreign import ccall unsafe "tchdbiterinit"
  c_tchdbiterinit :: Ptr HDB -> IO Bool

foreign import ccall unsafe "tchdbiternext2"
  c_tchdbiternext2 :: Ptr HDB -> IO CString

foreign import ccall unsafe "tchdbputkeep2"
  c_tchdbputkeep2 :: Ptr HDB -> CString -> CString -> IO Bool

foreign import ccall unsafe "tchdbputcat2"
  c_tchdbputcat2 :: Ptr HDB -> CString -> CString -> IO Bool
