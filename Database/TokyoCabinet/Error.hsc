{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Database.TokyoCabinet.Error
    (
    -- * Error code
      ECODE(..)
    -- * Utility function
    , errmsg
    , cintToError
    , errorToCInt
    -- * Other constants
    , cINT_MIN
    ) where

import System.IO.Unsafe (unsafePerformIO)
import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <tcutil.h>

-- | Represents error
data ECODE =
    ESUCCESS | -- ^ success            
    ETHREAD  | -- ^ threading error    
    EINVALID | -- ^ invalid operation  
    ENOFILE  | -- ^ file not found     
    ENOPERM  | -- ^ no permission      
    EMETA    | -- ^ invalid meta data  
    ERHEAD   | -- ^ invalid record header 
    EOPEN    | -- ^ open error         
    ECLOSE   | -- ^ close error        
    ETRUNC   | -- ^ trunc error        
    ESYNC    | -- ^ sync error         
    ESTAT    | -- ^ stat error         
    ESEEK    | -- ^ seek error         
    EREAD    | -- ^ read error         
    EWRITE   | -- ^ write error        
    EMMAP    | -- ^ mmap error         
    ELOCK    | -- ^ lock error         
    EUNLINK  | -- ^ unlink error       
    ERENAME  | -- ^ rename error       
    EMKDIR   | -- ^ mkdir error        
    ERMDIR   | -- ^ rmdir error        
    EKEEP    | -- ^ existing record    
    ENOREC   | -- ^ no record found    
    EMISC      -- ^ miscellaneous error
    deriving (Eq, Ord)

instance Show ECODE where
    show e =  errmsg e ++ " (code:" ++ show (errorToCInt e) ++ ")"

errorToCInt :: ECODE -> CInt
errorToCInt ESUCCESS = #const TCESUCCESS
errorToCInt ETHREAD  = #const TCETHREAD
errorToCInt EINVALID = #const TCEINVALID
errorToCInt ENOFILE  = #const TCENOFILE
errorToCInt ENOPERM  = #const TCENOPERM
errorToCInt EMETA    = #const TCEMETA
errorToCInt ERHEAD   = #const TCERHEAD
errorToCInt EOPEN    = #const TCEOPEN
errorToCInt ECLOSE   = #const TCECLOSE
errorToCInt ETRUNC   = #const TCETRUNC
errorToCInt ESYNC    = #const TCESYNC
errorToCInt ESTAT    = #const TCESTAT
errorToCInt ESEEK    = #const TCESEEK
errorToCInt EREAD    = #const TCEREAD
errorToCInt EWRITE   = #const TCEWRITE
errorToCInt EMMAP    = #const TCEMMAP
errorToCInt ELOCK    = #const TCELOCK
errorToCInt EUNLINK  = #const TCEUNLINK
errorToCInt ERENAME  = #const TCERENAME
errorToCInt EMKDIR   = #const TCEMKDIR
errorToCInt ERMDIR   = #const TCERMDIR
errorToCInt EKEEP    = #const TCEKEEP
errorToCInt ENOREC   = #const TCENOREC
errorToCInt EMISC    = #const TCEMISC

cintToError :: CInt -> ECODE
cintToError #{const TCESUCCESS} = ESUCCESS
cintToError #{const TCETHREAD} = ETHREAD
cintToError #{const TCEINVALID} = EINVALID
cintToError #{const TCENOFILE} = ENOFILE
cintToError #{const TCENOPERM} = ENOPERM
cintToError #{const TCEMETA} = EMETA
cintToError #{const TCERHEAD} = ERHEAD
cintToError #{const TCEOPEN} = EOPEN
cintToError #{const TCECLOSE} = ECLOSE
cintToError #{const TCETRUNC} = ETRUNC
cintToError #{const TCESYNC} = ESYNC
cintToError #{const TCESTAT} = ESTAT
cintToError #{const TCESEEK} = ESEEK
cintToError #{const TCEREAD} = EREAD
cintToError #{const TCEWRITE} = EWRITE
cintToError #{const TCEMMAP} = EMMAP
cintToError #{const TCELOCK} = ELOCK
cintToError #{const TCEUNLINK} = EUNLINK
cintToError #{const TCERENAME} = ERENAME
cintToError #{const TCEMKDIR} = EMKDIR
cintToError #{const TCERMDIR} = ERMDIR
cintToError #{const TCEKEEP} = EKEEP
cintToError #{const TCENOREC} = ENOREC
cintToError #{const TCEMISC} = EMISC
cintToError _ = error "unknown error code"

cINT_MIN :: CInt
cINT_MIN = #const INT_MIN

-- | Convert error code to message string.
errmsg :: ECODE -> String
errmsg = unsafePerformIO . peekCString . c_tcerrmsg . errorToCInt

foreign import ccall "tcerrmsg"
  c_tcerrmsg :: CInt -> CString
