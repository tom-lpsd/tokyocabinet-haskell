{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Database.TokyoCabinet.TDB.C where

import Data.Int
import Data.Word
import Data.Bits

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Foreign.C.String

import Database.TokyoCabinet.Map.C
import Database.TokyoCabinet.List.C

#include <tctdb.h>

data OpenMode =
    OREADER |
    OWRITER |
    OCREAT  |
    OTRUNC  |
    ONOLCK  |
    OLCKNB  |
    OTSYNC
    deriving (Eq, Ord, Show)

data TuningOption =
    TLARGE   |
    TDEFLATE |
    TBZIP    |
    TTCBS    |
    TEXCODEC
    deriving (Eq, Ord, Show)

data IndexType =
    ITLEXICAL |
    ITDECIMAL |
    ITOPT     |
    ITVOID    |
    ITKEEP
    deriving (Eq, Ord, Show)

openModeToCInt :: OpenMode -> CInt
openModeToCInt OREADER = #const TDBOREADER
openModeToCInt OWRITER = #const TDBOWRITER
openModeToCInt OCREAT  = #const TDBOCREAT
openModeToCInt OTRUNC  = #const TDBOTRUNC
openModeToCInt ONOLCK  = #const TDBONOLCK
openModeToCInt OLCKNB  = #const TDBOLCKNB
openModeToCInt OTSYNC  = #const TDBOTSYNC

tuningOptionToWord8 :: TuningOption -> Word8
tuningOptionToWord8 TLARGE   = #const TDBTLARGE
tuningOptionToWord8 TDEFLATE = #const TDBTDEFLATE
tuningOptionToWord8 TBZIP    = #const TDBTBZIP
tuningOptionToWord8 TTCBS    = #const TDBTTCBS
tuningOptionToWord8 TEXCODEC = #const TDBTEXCODEC

indexTypeToCInt :: IndexType -> CInt
indexTypeToCInt ITLEXICAL = #const TDBITLEXICAL
indexTypeToCInt ITDECIMAL = #const TDBITDECIMAL
indexTypeToCInt ITOPT     = #const TDBITOPT
indexTypeToCInt ITVOID    = #const TDBITVOID
indexTypeToCInt ITKEEP    = #const TDBITKEEP

combineOpenMode :: [OpenMode] -> CInt
combineOpenMode = foldr ((.|.) . openModeToCInt) 0

combineTuningOption :: [TuningOption] -> Word8
combineTuningOption = foldr ((.|.) . tuningOptionToWord8) 0

data TDB = TDB { unTCTDB :: !(ForeignPtr TDB') }

data TDB'

foreign import ccall safe "tctdbnew"
  c_tctdbnew :: IO (Ptr TDB')

foreign import ccall safe "tctdbdel"
  c_tctdbdel :: Ptr TDB' -> IO ()

foreign import ccall safe "&tctdbdel"
  tctdbFinalizer :: FunPtr (Ptr TDB' -> IO ())

foreign import ccall safe "tctdbecode"
  c_tctdbecode :: Ptr TDB' -> IO CInt

foreign import ccall safe "tctdbsetmutex"
  c_tctdbsetmutex :: Ptr TDB' -> IO Bool

foreign import ccall safe "tctdbtune"
  c_tctdbtune :: Ptr TDB' -> Int64 -> Int8 -> Int8 -> Word8 -> IO Bool

foreign import ccall safe "tctdbsetcache"
  c_tctdbsetcache :: Ptr TDB' -> Int32 -> Int32 -> Int32 -> IO Bool

foreign import ccall safe "tctdbsetxmsiz"
  c_tctdbsetxmsiz :: Ptr TDB' -> Int64 -> IO Bool

foreign import ccall safe "tctdbopen"
  c_tctdbopen :: Ptr TDB' -> CString -> CInt -> IO Bool

foreign import ccall safe "tctdbclose"
  c_tctdbclose :: Ptr TDB' -> IO Bool

foreign import ccall safe "tctdbput"
  c_tctdbput :: Ptr TDB' -> Ptr Word8 -> CInt -> Ptr MAP -> IO Bool

foreign import ccall safe "tctdbput2"
  c_tctdbput2 :: Ptr TDB' -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tctdbput3"
  c_tctdbput3 :: Ptr TDB' -> CString -> CString -> IO Bool

foreign import ccall safe "tctdbputkeep"
  c_tctdbputkeep :: Ptr TDB' -> Ptr Word8 -> CInt -> Ptr MAP -> IO Bool

foreign import ccall safe "tctdbputkeep2"
  c_tctdbputkeep2 :: Ptr TDB' -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tctdbputkeep3"
  c_tctdbputkeep3 :: Ptr TDB' -> CString -> CString -> IO Bool

foreign import ccall safe "tctdbputcat"
  c_tctdbputcat :: Ptr TDB' -> Ptr Word8 -> CInt -> Ptr MAP -> IO Bool

foreign import ccall safe "tctdbputcat2"
  c_tctdbputcat2 :: Ptr TDB' -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tctdbputcat3"
  c_tctdbputcat3 :: Ptr TDB' -> CString -> CString -> IO Bool

foreign import ccall safe "tctdbout"
  c_tctdbout :: Ptr TDB' -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tctdbout2"
  c_tctdbout2 :: Ptr TDB' -> CString -> IO Bool

foreign import ccall safe "tctdbget"
  c_tctdbget :: Ptr TDB' -> Ptr Word8 -> CInt -> IO (Ptr MAP)

foreign import ccall safe "tctdbget2"
  c_tctdbget2 :: Ptr TDB' -> Ptr Word8 -> CInt -> Ptr CInt -> IO CString

foreign import ccall safe "tctdbget3"
  c_tctdbget3 :: Ptr TDB' -> CString -> IO CString

foreign import ccall safe "tctdbvsiz"
  c_tctdbvsiz :: Ptr TDB' -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall safe "tctdbvsiz2"
  c_tctdbvsiz2 :: Ptr TDB' -> CString -> IO CInt

foreign import ccall safe "tctdbiterinit"
  c_tctdbiterinit :: Ptr TDB' -> IO Bool

foreign import ccall safe "tctdbiternext"
  c_tctdbiternext :: Ptr TDB' -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tctdbiternext2"
  c_tctdbiternext2 :: Ptr TDB' -> IO CString

foreign import ccall safe "tctdbfwmkeys"
  c_tctdbfwmkeys :: Ptr TDB' -> Ptr Word8 -> CInt -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tctdbfwmkeys2"
  c_tctdbfwmkeys2 :: Ptr TDB' -> CString -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tctdbaddint"
  c_tctdbaddint :: Ptr TDB' -> Ptr Word8 -> CInt -> CInt -> IO CInt

foreign import ccall safe "tctdbadddouble"
  c_tctdbadddouble :: Ptr TDB' -> Ptr Word8 -> CInt -> CDouble -> IO CDouble

foreign import ccall safe "tctdbsync"
  c_tctdbsync :: Ptr TDB' -> IO Bool

foreign import ccall safe "tctdboptimize"
  c_tctdboptimize :: Ptr TDB' -> Int64 -> Int8 -> Int8 -> Word8 -> IO Bool

foreign import ccall safe "tctdbvanish"
  c_tctdbvanish :: Ptr TDB' -> IO Bool

foreign import ccall safe "tctdbcopy"
  c_tctdbcopy :: Ptr TDB' -> CString -> IO Bool

foreign import ccall safe "tctdbtranbegin"
  c_tctdbtranbegin :: Ptr TDB' -> IO Bool

foreign import ccall safe "tctdbtrancommit"
  c_tctdbtrancommit :: Ptr TDB' -> IO Bool

foreign import ccall safe "tctdbtranabort"
  c_tctdbtranabort :: Ptr TDB' -> IO Bool

foreign import ccall safe "tctdbpath"
  c_tctdbpath :: Ptr TDB' -> IO CString

foreign import ccall safe "tctdbrnum"
  c_tctdbrnum :: Ptr TDB' -> IO Word64

foreign import ccall safe "tctdbfsiz"
  c_tctdbfsiz :: Ptr TDB' -> IO Word64

foreign import ccall safe "tctdbsetindex"
  c_tctdbsetindex :: Ptr TDB' -> CString -> CInt -> IO Bool

foreign import ccall safe "tctdbgenuid"
  c_tctdbgenuid :: Ptr TDB' -> IO Int64
