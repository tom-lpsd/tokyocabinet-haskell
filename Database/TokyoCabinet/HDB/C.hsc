{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Database.TokyoCabinet.HDB.C where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Int
import Data.Word

import Database.TokyoCabinet.List.C (LIST)

#include <tchdb.h>

newtype OpenMode = OpenMode { unOpenMode :: CInt }
    deriving (Eq, Show)

#{enum OpenMode, OpenMode
 , oREADER = HDBOREADER
 , oWRITER = HDBOWRITER
 , oCREAT  = HDBOCREAT
 , oTRUNC  = HDBOTRUNC
 , oNOLCK  = HDBONOLCK
 , oLCKNB  = HDBOLCKNB
 , oTSYNC  = HDBOTSYNC
}

newtype TuningOption = TuningOption { unTuningOption :: Word8 }
    deriving (Eq, Show)

#{enum TuningOption, TuningOption
 , tLARGE   = HDBTLARGE
 , tDEFLATE = HDBTDEFLATE
 , tBZIP    = HDBTBZIP
 , tTCBS    = HDBTTCBS
 , tEXCODEC = HDBTEXCODEC
}

data HDB

foreign import ccall "&tchdbdel"
  tchdbFinalizer :: FunPtr (Ptr HDB -> IO ())

foreign import ccall safe "tchdbnew"
  c_tchdbnew :: IO (Ptr HDB)

foreign import ccall safe "tchdbdel"
  c_tchdbdel :: Ptr HDB -> IO ()

foreign import ccall safe "tchdbecode"
  c_tchdbecode :: Ptr HDB -> IO CInt

foreign import ccall safe "tchdbtune"
  c_tchdbtune :: Ptr HDB -> Int64 -> Int8 -> Int8 -> Word8 -> IO Bool

foreign import ccall safe "tchdbsetcache"
  c_tchdbsetcache :: Ptr HDB -> Int32 -> IO Bool

foreign import ccall safe "tchdbsetxmsiz"
  c_tchdbsetxmsiz :: Ptr HDB -> Int64 -> IO Bool

foreign import ccall safe "tchdbopen"
  c_tchdbopen :: Ptr HDB -> CString -> CInt -> IO Bool

foreign import ccall safe "tchdbclose"
  c_tchdbclose :: Ptr HDB -> IO Bool

foreign import ccall safe "tchdbput"
  c_tchdbput :: Ptr HDB -> CString -> CInt -> CString -> CInt -> IO Bool

foreign import ccall safe "tchdbput2"
  c_tchdbput2 :: Ptr HDB -> CString -> CString -> IO Bool

foreign import ccall safe "tchdbputkeep"
  c_tchdbputkeep :: Ptr HDB -> CString -> CInt -> CString -> CInt -> IO Bool

foreign import ccall safe "tchdbputkeep2"
  c_tchdbputkeep2 :: Ptr HDB -> CString -> CString -> IO Bool

foreign import ccall safe "tchdbputcat"
  c_tchdbputcat :: Ptr HDB -> CString -> CInt -> CString -> CInt -> IO Bool

foreign import ccall safe "tchdbputcat2"
  c_tchdbputcat2 :: Ptr HDB -> CString -> CString -> IO Bool

foreign import ccall safe "tchdbputasync"
  c_tchdbputasync :: Ptr HDB -> CString -> CInt -> CString -> CInt -> IO Bool

foreign import ccall safe "tchdbout"
  c_tchdbout :: Ptr HDB -> CString -> CInt -> IO Bool

foreign import ccall safe "tchdbout2"
  c_tchdbout2 :: Ptr HDB -> CString -> IO Bool

foreign import ccall safe "tchdbget"
  c_tchdbget :: Ptr HDB -> CString -> CInt -> Ptr CInt -> IO (Ptr CChar)

foreign import ccall safe "tchdbget2"
  c_tchdbget2 :: Ptr HDB -> CString -> IO (Ptr CChar)

foreign import ccall safe "tchdbvsiz"
  c_tchdbvsiz :: Ptr HDB -> CString -> CInt -> IO CInt

foreign import ccall safe "tchdbiterinit"
  c_tchdbiterinit :: Ptr HDB -> IO Bool

foreign import ccall safe "tchdbiternext"
  c_tchdbiternext :: Ptr HDB -> Ptr Int -> IO (Ptr CChar)

foreign import ccall safe "tchdbiternext2"
  c_tchdbiternext2 :: Ptr HDB -> IO CString

foreign import ccall safe "tchdbfwmkeys"
  c_tchdbfwmkeys :: Ptr HDB -> CString -> CInt -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tchdbaddint"
  c_tchdbaddint :: Ptr HDB -> CString -> CInt -> CInt -> IO CInt

foreign import ccall safe "tchdbadddouble"
  c_tchdbadddouble :: Ptr HDB -> CString -> CInt -> CDouble -> IO CDouble

foreign import ccall safe "tchdbsync"
  c_tchdbsync :: Ptr HDB -> IO Bool

foreign import ccall safe "tchdboptimize"
  c_tchdboptimize :: Ptr HDB -> Int64 -> Int8 -> Int8 -> Word8 -> IO Bool

foreign import ccall safe "tchdbvanish"
  c_tchdbvanish :: Ptr HDB -> IO Bool

foreign import ccall safe "tchdbcopy"
  c_tchdbcopy :: Ptr HDB -> CString -> IO Bool

foreign import ccall safe "tchdbtranbegin"
  c_tchdbtranbegin :: Ptr HDB -> IO Bool

foreign import ccall safe "tchdbtrancommit"
  c_tchdbtrancommit :: Ptr HDB -> IO Bool

foreign import ccall safe "tchdbtranabort"
  c_tchdbtranabort :: Ptr HDB -> IO Bool

foreign import ccall safe "tchdbpath"
  c_tchdbpath :: Ptr HDB -> IO (Ptr CChar)

foreign import ccall safe "tchdbrnum"
  c_tchdbrnum :: Ptr HDB -> IO Int64

foreign import ccall safe "tchdbfsiz"
  c_tchdbfsiz :: Ptr HDB -> IO Int64
