{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Database.TokyoCabinet.BDB.Cursor.C where

import Database.TokyoCabinet.BDB.C

import Foreign.Ptr

import Foreign.C.Types
import Foreign.C.String

import Data.Word

#include <tcbdb.h>

data CursorPutMode = CPCURRENT |
                     CPBEFORE  |
                     CPAFTER
                     deriving (Eq, Ord, Show)

cpToCInt :: CursorPutMode -> CInt
cpToCInt CPCURRENT = #const BDBCPCURRENT
cpToCInt CPBEFORE  = #const BDBCPBEFORE
cpToCInt CPAFTER   = #const BDBCPAFTER

data CUR

foreign import ccall safe "tcbdbcurnew"
  c_tcbdbcurnew :: Ptr BDB' -> IO (Ptr CUR)

foreign import ccall safe "tcbdbcurdel"
  c_tcbdbcurdel :: Ptr CUR -> IO ()

foreign import ccall safe "&tcbdbcurdel"
  tcbdbcurFinalizer :: FunPtr (Ptr CUR -> IO ())

foreign import ccall safe "tcbdbcurfirst"
  c_tcbdbcurfirst :: Ptr CUR -> IO Bool

foreign import ccall safe "tcbdbcurlast"
  c_tcbdbcurlast :: Ptr CUR -> IO Bool

foreign import ccall safe "tcbdbcurjump"
  c_tcbdbcurjump :: Ptr CUR -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcbdbcurjump2"
  c_tcbdbcurjump2 :: Ptr CUR -> CString -> IO Bool

foreign import ccall safe "tcbdbcurprev"
  c_tcbdbcurprev :: Ptr CUR -> IO Bool

foreign import ccall safe "tcbdbcurnext"
  c_tcbdbcurnext :: Ptr CUR -> IO Bool

foreign import ccall safe "tcbdbcurput"
  c_tcbdbcurput :: Ptr CUR -> Ptr Word8 -> CInt -> CInt -> IO Bool

foreign import ccall safe "tcbdbcurput2"
  c_tcbdbcurput2 :: Ptr CUR -> CString -> CInt -> IO Bool

foreign import ccall safe "tcbdbcurout"
  c_tcbdbcurout :: Ptr CUR -> IO Bool

foreign import ccall safe "tcbdbcurkey"
  c_tcbdbcurkey :: Ptr CUR -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcbdbcurkey2"
  c_tcbdbcurkey2 :: Ptr CUR -> IO CString

foreign import ccall safe "tcbdbcurkey3"
  c_tcbdbcurkey3 :: Ptr CUR -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcbdbcurval"
  c_tcbdbcurval :: Ptr CUR -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcbdbcurval2"
  c_tcbdbcurval2 :: Ptr CUR -> IO CString

foreign import ccall safe "tcbdbcurval3"
  c_tcbdbcurval3 :: Ptr CUR -> Ptr CInt -> IO (Ptr Word8)
