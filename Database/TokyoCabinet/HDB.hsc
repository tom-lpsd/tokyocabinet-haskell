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
import Data.ByteString (ByteString, useAsCString, useAsCStringLen, packCString)

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
        useAsCStringLen key $ \(kbuf, ksiz) ->
        useAsCStringLen val $ \(vbuf, vsiz) ->
            c_tchdbput p kbuf (fromIntegral ksiz) vbuf (fromIntegral vsiz)

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
addint (TCHDB fptr) key num =
    withForeignPtr fptr $ \p ->
        useAsCStringLen key $ \(kbuf, ksiz) -> do
            n <- c_tchdbaddint p kbuf (fromIntegral ksiz) (fromIntegral num)
            return $ fromIntegral n

adddouble :: TCHDB -> ByteString -> Double -> IO Double
adddouble (TCHDB fptr) key num =
    withForeignPtr fptr $ \p ->
        useAsCStringLen key $ \(kbuf, ksiz) -> do
            n <- c_tchdbadddouble p kbuf (fromIntegral ksiz) (realToFrac num)
            return $ realToFrac n

sync :: TCHDB -> IO Bool
sync (TCHDB fptr) = withForeignPtr fptr c_tchdbsync

optimize :: TCHDB -> Int64 -> Int8 -> Int8 -> [TuningOption] -> IO Bool
optimize (TCHDB fptr) bnum apow fpow options = 
    let option = unTuningOption $ combineTuningOption options
    in withForeignPtr fptr $ \p -> c_tchdboptimize p bnum apow fpow option

vanish :: TCHDB -> IO Bool
vanish (TCHDB fptr) = withForeignPtr fptr c_tchdbvanish

copy :: TCHDB -> String -> IO Bool
copy (TCHDB fptr) path =
    withForeignPtr fptr $ \p ->
        withCString path $ \path ->
            c_tchdbcopy p path

tranbegin :: TCHDB -> IO Bool
tranbegin (TCHDB fptr) = withForeignPtr fptr c_tchdbtranbegin

trancommit :: TCHDB -> IO Bool
trancommit (TCHDB fptr) = withForeignPtr fptr c_tchdbtrancommit

tranabort :: TCHDB -> IO Bool
tranabort (TCHDB fptr) = withForeignPtr fptr c_tchdbtranabort

path :: TCHDB -> IO (Maybe String)
path (TCHDB fptr) =
    withForeignPtr fptr $ \p -> do
      cstr <- c_tchdbpath p
      if cstr == nullPtr
        then return Nothing
        else peekCString cstr >>= return . Just

rnum :: TCHDB -> IO Int64
rnum (TCHDB fptr) = withForeignPtr fptr c_tchdbrnum

fsiz :: TCHDB -> IO Int64
fsiz (TCHDB fptr) = withForeignPtr fptr c_tchdbfsiz

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

foreign import ccall unsafe "tchdbput"
  c_tchdbput :: Ptr HDB -> CString -> CInt -> CString -> CInt -> IO Bool

foreign import ccall unsafe "tchdbput2"
  c_tchdbput2 :: Ptr HDB -> CString -> CString -> IO Bool

foreign import ccall unsafe "tchdbget"
  c_tchdbget :: Ptr HDB -> CString -> CInt -> Ptr CInt -> IO (Ptr CChar)

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

foreign import ccall unsafe "tchdbaddint"
  c_tchdbaddint :: Ptr HDB -> CString -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "tchdbadddouble"
  c_tchdbadddouble :: Ptr HDB -> CString -> CInt -> CDouble -> IO CDouble

foreign import ccall unsafe "tchdbsync"
  c_tchdbsync :: Ptr HDB -> IO Bool

foreign import ccall unsafe "tchdboptimize"
  c_tchdboptimize :: Ptr HDB -> Int64 -> Int8 -> Int8 -> Word8 -> IO Bool

foreign import ccall unsafe "tchdbvanish"
  c_tchdbvanish :: Ptr HDB -> IO Bool

foreign import ccall unsafe "tchdbcopy"
  c_tchdbcopy :: Ptr HDB -> CString -> IO Bool

foreign import ccall unsafe "tchdbtranbegin"
  c_tchdbtranbegin :: Ptr HDB -> IO Bool

foreign import ccall unsafe "tchdbtrancommit"
  c_tchdbtrancommit :: Ptr HDB -> IO Bool

foreign import ccall unsafe "tchdbtranabort"
  c_tchdbtranabort :: Ptr HDB -> IO Bool

foreign import ccall unsafe "tchdbpath"
  c_tchdbpath :: Ptr HDB -> IO (Ptr CChar)

foreign import ccall unsafe "tchdbrnum"
  c_tchdbrnum :: Ptr HDB -> IO Int64

foreign import ccall unsafe "tchdbfsiz"
  c_tchdbfsiz :: Ptr HDB -> IO Int64
