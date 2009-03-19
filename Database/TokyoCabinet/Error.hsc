{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Database.TokyoCabinet.Error
    (
    -- * error code type
      TCErrorCode(..)
    -- * actual error code
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
    -- * utility function
    , errmsg
    -- * other constants
    , cINT_MIN
    ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <tcutil.h>

newtype TCErrorCode = TCErrorCode { unTCErrorCode :: CInt } deriving Eq

instance Show TCErrorCode where
    show e =  errmsg e ++ " (code:" ++ show (unTCErrorCode e) ++ ")"

#{enum TCErrorCode, TCErrorCode
 , eSUCCESS = TCESUCCESS
 , eTHREAD  = TCETHREAD
 , eINVALID = TCEINVALID
 , eNOFILE  = TCENOFILE
 , eNOPERM  = TCENOPERM
 , eMETA    = TCEMETA
 , eRHEAD   = TCERHEAD
 , eOPEN    = TCEOPEN
 , eCLOSE   = TCECLOSE
 , eTRUNC   = TCETRUNC
 , eSYNC    = TCESYNC
 , eSTAT    = TCESTAT
 , eSEEK    = TCESEEK
 , eREAD    = TCEREAD
 , eWRITE   = TCEWRITE
 , eMMAP    = TCEMMAP
 , eLOCK    = TCELOCK
 , eUNLINK  = TCEUNLINK
 , eRENAME  = TCERENAME
 , eMKDIR   = TCEMKDIR
 , eRMDIR   = TCERMDIR
 , eKEEP    = TCEKEEP
 , eNOREC   = TCENOREC
 , eMISC    = TCEMISC
}

cINT_MIN :: CInt
cINT_MIN = #const INT_MIN

errmsg :: TCErrorCode -> String
errmsg = unsafePerformIO . peekCString . c_tcerrmsg . unTCErrorCode

foreign import ccall "tcerrmsg"
  c_tcerrmsg :: CInt -> CString
