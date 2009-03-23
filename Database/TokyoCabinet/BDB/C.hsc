{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Database.TokyoCabinet.BDB.C where

#include <tcbdb.h>

import Foreign.Ptr
import Foreign.ForeignPtr

import Foreign.C.Types
import Foreign.C.String

import Data.Int
import Data.Word
import Data.Bits

import Database.TokyoCabinet.List.C (LIST)

data TCBDB = TCBDB { unTCBDB :: !(ForeignPtr BDB) }

data OpenMode =
    OREADER |
    OWRITER |
    OCREAT  |
    OTRUNC  |
    ONOLCK  |
    OLCKNB  |
    OTSYNC
    deriving (Eq, Ord, Show)

openModeToCInt :: OpenMode -> CInt
openModeToCInt OREADER = #const BDBOREADER
openModeToCInt OWRITER = #const BDBOWRITER
openModeToCInt OCREAT  = #const BDBOCREAT
openModeToCInt OTRUNC  = #const BDBOTRUNC
openModeToCInt ONOLCK  = #const BDBONOLCK
openModeToCInt OLCKNB  = #const BDBOLCKNB
openModeToCInt OTSYNC  = #const BDBOTSYNC

combineOpenMode :: [OpenMode] -> CInt
combineOpenMode = foldr ((.|.) . openModeToCInt) 0

data TuningOption =
    TLARGE   |
    TDEFLATE |
    TBZIP    |
    TTCBS    |
    TEXCODEC
    deriving (Eq, Ord, Show)

tuningOptionToWord8 :: TuningOption -> Word8
tuningOptionToWord8 TLARGE   = #const BDBTLARGE
tuningOptionToWord8 TDEFLATE = #const BDBTDEFLATE
tuningOptionToWord8 TBZIP    = #const BDBTBZIP
tuningOptionToWord8 TTCBS    = #const BDBTTCBS
tuningOptionToWord8 TEXCODEC = #const BDBTEXCODEC

combineTuningOption :: [TuningOption] -> Word8
combineTuningOption = foldr ((.|.) . tuningOptionToWord8) 0

type TCCMP = Ptr CChar -> CInt -> Ptr CChar -> CInt -> Ptr Word8 -> CInt

data BDB

foreign import ccall safe "tcbdbnew"
  c_tcbdbnew :: IO (Ptr BDB)

foreign import ccall safe "tcbdbdel"
  c_tcbdbdel :: Ptr BDB -> IO ()

foreign import ccall safe "&tcbdbdel"
  tcbdbFinalizer :: FunPtr (Ptr BDB -> IO ())

foreign import ccall safe "tcbdbecode"
  c_tcbdbecode :: Ptr BDB -> IO CInt

foreign import ccall safe "tcbdbsetmutex"
  c_tcbdbsetmutex :: Ptr BDB -> IO Bool

foreign import ccall safe "tcbdbsetcmpfunc"
  c_tcbdbsetcmpfunc :: Ptr BDB -> FunPtr TCCMP -> IO Bool

foreign import ccall safe "tcbdbtune"
  c_tcbdbtune ::
      Ptr BDB -> Int32 -> Int32 -> Int64 -> Int8 -> Int8 -> Word8 -> IO Bool

foreign import ccall safe "tcbdbsetcache"
  c_tcbdbsetcache :: Ptr BDB -> Int32 -> Int32 -> IO Bool

foreign import ccall safe "tcbdbsetxmsiz"
  c_tcbdbsetxmsiz :: Ptr BDB -> Int64 -> IO Bool

foreign import ccall safe "tcbdbopen"
  c_tcbdbopen :: Ptr BDB -> CString -> CInt -> IO Bool

foreign import ccall safe "tcbdbclose"
  c_tcbdbclose :: Ptr BDB -> IO Bool

foreign import ccall safe "tcbdbput"
  c_tcbdbput :: Ptr BDB -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcbdbput2"
  c_tcbdbput2 :: Ptr BDB -> CString -> CString -> IO Bool

foreign import ccall safe "tcbdbputkeep"
  c_tcbdbputkeep ::
      Ptr BDB -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcbdbputkeep2"
  c_tcbdbputkeep2 :: Ptr BDB -> CString -> CString -> IO Bool

foreign import ccall safe "tcbdbputcat"
  c_tcbdbputcat ::
      Ptr BDB -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcbdbputcat2"
  c_tcbdbputcat2 :: Ptr BDB -> CString -> CString -> IO Bool

foreign import ccall safe "tcbdbputdup"
  c_tcbdbputdup ::
      Ptr BDB -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcbdbputdup2"
  c_tcbdbputdup2 :: Ptr BDB -> CString -> CString -> IO Bool

foreign import ccall safe "tcbdbputdup3"
  c_tcbdbputdup3 :: Ptr BDB -> Ptr Word8 -> CInt -> Ptr LIST -> IO Bool

foreign import ccall safe "tcbdbout"
  c_tcbdbout :: Ptr BDB -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcbdbout2"
  c_tcbdbout2 :: Ptr BDB -> CString -> IO Bool

foreign import ccall safe "tcbdbout3"
  c_tcbdbout3 :: Ptr BDB -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcbdbget"
  c_tcbdbget :: Ptr BDB -> Ptr Word8 -> CInt -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcbdbget2"
  c_tcbdbget2 :: Ptr BDB -> CString -> IO CString

foreign import ccall safe "tcbdbget3"
  c_tcbdbget3 :: Ptr BDB -> Ptr Word8 -> CInt -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcbdbget4"
  c_tcbdbget4 :: Ptr BDB -> Ptr Word8 -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tcbdbvnum"
  c_tcbdbvnum :: Ptr BDB -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall safe "tcbdbvnum2"
  c_tcbdbvnum2 :: Ptr BDB -> CString -> IO CInt

foreign import ccall safe "tcbdbvsiz"
  c_tcbdbvsiz :: Ptr BDB -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall safe "tcbdbvsiz2"
  c_tcbdbvsiz2 :: Ptr BDB -> CString -> IO CInt

foreign import ccall safe "tcbdbrange"
  c_tcbdbrange ::
      Ptr BDB -> Ptr Word8 -> CInt -> Bool
              -> Ptr Word8 -> CInt -> Bool -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tcbdbrange2"
  c_tcbdbrange2 ::
      Ptr BDB -> CString -> Bool -> CString -> Bool -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tcbdbfwmkeys"
  c_tcbdbfwmkeys :: Ptr BDB -> Ptr Word8 -> CInt -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tcbdbfwmkeys2"
  c_tcbdbfwmkeys2 :: Ptr BDB -> CString -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tcbdbaddint"
  c_tcbdbaddint :: Ptr BDB -> Ptr Word8 -> CInt -> CInt -> IO CInt

foreign import ccall safe "tcbdbadddouble"
  c_tcbdbadddouble :: Ptr BDB -> Ptr Word8 -> CInt -> CDouble -> IO CDouble

foreign import ccall safe "tcbdbsync"
  c_tcbdbsync :: Ptr BDB -> IO Bool

foreign import ccall safe "tcbdboptimize"
  c_tcbdboptimize ::
      Ptr BDB -> Int32 -> Int32 -> Int64 -> Int8 -> Int8 -> Word8 -> IO Bool

foreign import ccall safe "tcbdbvanish"
  c_tcbdbvanish :: Ptr BDB -> IO Bool

foreign import ccall safe "tcbdbcopy"
  c_tcbdbcopy :: Ptr BDB -> CString -> IO Bool

foreign import ccall safe "tcbdbtranbegin"
  c_tcbdbtranbegin :: Ptr BDB -> IO Bool

foreign import ccall safe "tcbdbtrancommit"
  c_tcbdbtrancommit :: Ptr BDB -> IO Bool

foreign import ccall safe "tcbdbtranabort"
  c_tcbdbtranabort :: Ptr BDB -> IO Bool

foreign import ccall safe "tcbdbpath"
  c_tcbdbpath :: Ptr BDB -> IO CString

foreign import ccall safe "tcbdbrnum"
  c_tcbdbrnum :: Ptr BDB -> IO Int64

foreign import ccall safe "tcbdbfsiz"
  c_tcbdbfsiz :: Ptr BDB -> IO Int64
