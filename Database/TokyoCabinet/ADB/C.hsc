{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Database.TokyoCabinet.ADB.C where

import Data.Word

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import Database.TokyoCabinet.List.C (LIST)

#include <tcadb.h>

data ADB'

foreign import ccall safe "tcadbnew"
  c_tcadbnew :: IO (Ptr ADB')

foreign import ccall safe "tcadbdel"
  c_tcadbdel :: Ptr ADB' -> IO ()

foreign import ccall safe "&tcadbdel"
  tcadbFinalizer :: FunPtr (Ptr ADB' -> IO ())

foreign import ccall safe "tcadbopen"
  c_tcadbopen :: Ptr ADB' -> CString -> IO Bool

foreign import ccall safe "tcadbclose"
  c_tcadbclose :: Ptr ADB' -> IO Bool

foreign import ccall safe "tcadbput"
  c_tcadbput :: Ptr ADB' -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcadbput2"
  c_tcadbput2 :: Ptr ADB' -> CString -> CString -> IO Bool

foreign import ccall safe "tcadbputkeep"
  c_tcadbputkeep :: Ptr ADB' -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcadbputkeep2"
  c_tcadbputkeep2 :: Ptr ADB' -> CString -> CString -> IO Bool

foreign import ccall safe "tcadbputcat"
  c_tcadbputcat :: Ptr ADB' -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcadbputcat2"
  c_tcadbputcat2 :: Ptr ADB' -> CString -> CString -> IO Bool

foreign import ccall safe "tcadbout"
  c_tcadbout :: Ptr ADB' -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcadbout2"
  c_tcadbout2 :: Ptr ADB' -> CString -> IO Bool

foreign import ccall safe "tcadbget"
  c_tcadbget :: Ptr ADB' -> Ptr Word8 -> CInt -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcadbget2"
  c_tcadbget2 :: Ptr ADB' -> CString -> IO CString

foreign import ccall safe "tcadbvsiz"
  c_tcadbvsiz :: Ptr ADB' -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall safe "tcadbvsiz2"
  c_tcadbvsiz2 :: Ptr ADB' -> CString -> IO CInt

foreign import ccall safe "tcadbiterinit"
  c_tcadbiterinit :: Ptr ADB' -> IO Bool

foreign import ccall safe "tcadbiternext"
  c_tcadbiternext :: Ptr ADB' -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcadbiternext2"
  c_tcadbiternext2 :: Ptr ADB' ->  IO CString

foreign import ccall safe "tcadbfwmkeys"
  c_tcadbfwmkeys :: Ptr ADB' -> Ptr Word8 -> CInt -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tcadbfwmkeys2"
  c_tcadbfwmkeys2 :: Ptr ADB' -> CString -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tcadbaddint"
  c_tcadbaddint :: Ptr ADB' -> Ptr Word8 -> CInt -> CInt -> IO CInt

foreign import ccall safe "tcadbadddouble"
  c_tcadbadddouble :: Ptr ADB' -> Ptr Word8 -> CInt -> CDouble -> IO CDouble

foreign import ccall safe "tcadbsync"
  c_tcadbsync :: Ptr ADB' -> IO Bool

foreign import ccall safe "tcadboptimize"
  c_tcadboptimize :: Ptr ADB' -> CString -> IO Bool

foreign import ccall safe "tcadbvanish"
  c_tcadbvanish :: Ptr ADB' -> IO Bool

foreign import ccall safe "tcadbcopy"
  c_tcadbcopy :: Ptr ADB' -> CString -> IO Bool

foreign import ccall safe "tcadbtranbegin"
  c_tcadbtranbegin :: Ptr ADB' -> IO Bool

foreign import ccall safe "tcadbtrancommit"
  c_tcadbtrancommit :: Ptr ADB' -> IO Bool

foreign import ccall safe "tcadbtranabort"
  c_tcadbtranabort :: Ptr ADB' -> IO Bool

foreign import ccall safe "tcadbpath"
  c_tcadbpath :: Ptr ADB' -> IO CString

foreign import ccall safe "tcadbrnum"
  c_tcadbrnum :: Ptr ADB' -> IO Word64

foreign import ccall safe "tcadbsize"
  c_tcadbsize :: Ptr ADB' -> IO Word64

foreign import ccall safe "tcadbmisc"
  c_tcadbmisc :: Ptr ADB' -> CString -> Ptr LIST -> IO (Ptr LIST)

