{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Database.TokyoCabinet.TDB.Query.C where

import Data.Word
import Data.Bits

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String

import Database.TokyoCabinet.TDB.C
import Database.TokyoCabinet.Map.C
import Database.TokyoCabinet.List.C

#include <tctdb.h>

data Condition =
    QCSTREQ   |
    QCSTRINC  |
    QCSTRBW   |
    QCSTREW   |
    QCSTRAND  |
    QCSTROR   |
    QCSTROREQ |
    QCSTRRX   |
    QCNUMEQ   |
    QCNUMGT   |
    QCNUMGE   |
    QCNUMLT   |
    QCNUMLE   |
    QCNUMBT   |
    QCNUMOREQ |
    QCNEGATE Condition |
    QCNOIDX  Condition
    deriving (Eq, Ord, Show)

data OrderType =
    QOSTRASC  |
    QOSTRDESC |
    QONUMASC  |
    QONUMDESC
    deriving (Eq, Ord, Show)

data PostTreatment m k v =
    QPPUT (m k v) |
    QPOUT  |
    QPNOP  |
    QPSTOP
    deriving (Eq, Ord, Show)

condToCInt :: Condition -> CInt
condToCInt QCSTREQ   = #const TDBQCSTREQ
condToCInt QCSTRINC  = #const TDBQCSTRINC
condToCInt QCSTRBW   = #const TDBQCSTRBW
condToCInt QCSTREW   = #const TDBQCSTREW
condToCInt QCSTRAND  = #const TDBQCSTRAND
condToCInt QCSTROR   = #const TDBQCSTROR
condToCInt QCSTROREQ = #const TDBQCSTROREQ
condToCInt QCSTRRX   = #const TDBQCSTRRX
condToCInt QCNUMEQ   = #const TDBQCNUMEQ
condToCInt QCNUMGT   = #const TDBQCNUMGT
condToCInt QCNUMGE   = #const TDBQCNUMGE
condToCInt QCNUMLT   = #const TDBQCNUMLT
condToCInt QCNUMLE   = #const TDBQCNUMLE
condToCInt QCNUMBT   = #const TDBQCNUMBT
condToCInt QCNUMOREQ = #const TDBQCNUMOREQ
condToCInt (QCNEGATE c) = (#const TDBQCNEGATE) .|. (condToCInt c)
condToCInt (QCNOIDX  c) = (#const TDBQCNOIDX) .|. (condToCInt c)

orderToCInt :: OrderType -> CInt
orderToCInt QOSTRASC  = #const TDBQOSTRASC
orderToCInt QOSTRDESC = #const TDBQOSTRDESC
orderToCInt QONUMASC  = #const TDBQONUMASC
orderToCInt QONUMDESC = #const TDBQONUMDESC

ptToCInt :: PostTreatment m k v -> CInt
ptToCInt QPNOP  = 0
ptToCInt QPOUT  = #const TDBQPOUT
ptToCInt QPSTOP = #const TDBQPSTOP
ptToCInt (QPPUT _) = #const TDBQPPUT

data TDBQRY = TDBQRY { unTDBQRY :: !(ForeignPtr QRY)
                     , unTDBOBJ :: TDB }

data QRY

foreign import ccall safe "tctdbqrynew"
  c_tctdbqrynew :: Ptr TDB' -> IO (Ptr QRY)

foreign import ccall safe "tctdbqrydel"
  c_tctdbqrydel :: Ptr QRY -> IO ()

foreign import ccall safe "&tctdbqrydel"
  tctdbqryFinalizer :: FunPtr (Ptr QRY -> IO ())

foreign import ccall safe "tctdbqryaddcond"
  c_tctdbqryaddcond :: Ptr QRY -> CString -> CInt -> CString -> IO ()

foreign import ccall safe "tctdbqrysetorder"
  c_tctdbqrysetorder :: Ptr QRY -> CString -> CInt -> IO ()

foreign import ccall safe "tctdbqrysetlimit"
  c_tctdbqrysetlimit :: Ptr QRY -> CInt -> CInt -> IO ()

foreign import ccall safe "tctdbqrysearch"
  c_tctdbqrysearch :: Ptr QRY -> IO (Ptr LIST)

foreign import ccall safe "tctdbqrysearchout"
  c_tctdbqrysearchout :: Ptr QRY -> IO Bool

foreign import ccall safe "tctdbqryhint"
  c_tctdbqryhint :: Ptr QRY -> IO CString

type TDBQRYPROC' = Ptr Word8 -> CInt -> Ptr MAP -> Ptr Word8 -> IO CInt

foreign import ccall safe "tctdbqryproc"
  c_tctdbqryproc :: Ptr QRY
                 -> FunPtr TDBQRYPROC'
                 -> Ptr Word8
                 -> IO Bool

foreign import ccall "wrapper"
  mkProc :: TDBQRYPROC' -> IO (FunPtr TDBQRYPROC')
