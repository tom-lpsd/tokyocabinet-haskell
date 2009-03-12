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
    , open
    , close
    , put
    , get
    , ecode
    )
    where

import Foreign hiding (new)
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal (free)

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

newtype TuningOption = TuningOption { unTuningOption :: CInt }
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

open :: TCHDB -> String -> [OpenMode] -> IO Bool
open (TCHDB fptr) name modes = 
    withForeignPtr fptr $ \p ->
        withCString name $ \name ->
            c_tchdbopen p name option
    where option = unOpenMode $ combineOpenMode modes

ecode :: TCHDB -> IO TCErrorCode
ecode (TCHDB fptr) = withForeignPtr fptr $ \p -> do
                       e <- c_tchdbecode p
                       return $ TCErrorCode e

close :: TCHDB -> IO Bool
close (TCHDB fptr) = withForeignPtr fptr c_tchdbclose

put :: TCHDB -> ByteString -> ByteString -> IO Bool
put (TCHDB fptr) key val =
    withForeignPtr fptr $ \p ->
        useAsCString key $ \key ->
        useAsCString val $ \val ->
            c_tchdbput2 p key val

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
