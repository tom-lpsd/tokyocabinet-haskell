{-# INCLUDE <tcutil.h> #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Database.TokyoCabinet.Map.C where

import Data.Word

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.ForeignPtr

import Database.TokyoCabinet.List.C

data Map k v = Map { unMap :: !(ForeignPtr MAP) }

data MAP

foreign import ccall safe "tcmapnew"
  c_tcmapnew :: IO (Ptr MAP)

foreign import ccall safe "tcmapnew2"
  c_tcmapnew2 :: Word32 -> IO (Ptr MAP)

foreign import ccall safe "tcmapdup"
  c_tcmapdup :: Ptr MAP -> IO (Ptr MAP)

foreign import ccall safe "tcmapdel"
  c_tcmapdel :: Ptr MAP -> IO ()

foreign import ccall safe "&tcmapdel"
  tcmapFinalizer :: FunPtr (Ptr MAP -> IO ())

foreign import ccall safe "tcmapput"
  c_tcmapput :: Ptr MAP -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO ()

foreign import ccall safe "tcmapput2"
  c_tcmapput2 :: Ptr MAP -> CString -> CString -> IO ()

foreign import ccall safe "tcmapputkeep"
  c_tcmapputkeep :: Ptr MAP -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcmapputkeep2"
  c_tcmapputkeep2 :: Ptr MAP -> CString -> CString -> IO Bool

foreign import ccall safe "tcmapputcat"
  c_tcmapputcat :: Ptr MAP -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO ()

foreign import ccall safe "tcmapputcat2"
  c_tcmapputcat2 :: Ptr MAP -> CString -> CString -> IO ()

foreign import ccall safe "tcmapout"
  c_tcmapout :: Ptr MAP -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcmapout2"
  c_tcmapout2 :: Ptr MAP -> CString -> IO Bool

foreign import ccall safe "tcmapget"
  c_tcmapget :: Ptr MAP -> Ptr Word8 -> CInt -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcmapget2"
  c_tcmapget2 :: Ptr MAP -> CString -> IO CString

foreign import ccall safe "tcmapmove"
  c_tcmapmove :: Ptr MAP -> Ptr Word8 -> CInt -> Bool -> IO Bool

foreign import ccall safe "tcmapmove2"
  c_tcmapmove2 :: Ptr MAP -> CString -> Bool -> IO Bool

foreign import ccall safe "tcmapiterinit"
  c_tcmapiterinit :: Ptr MAP -> IO ()

foreign import ccall safe "tcmapiternext"
  c_tcmapiternext :: Ptr MAP -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcmapiternext2"
  c_tcmapiternext2 :: Ptr MAP -> IO CString

foreign import ccall safe "tcmaprnum"
  c_tcmaprnum :: Ptr MAP -> IO Word64

foreign import ccall safe "tcmapmsiz"
  c_tcmapmsiz :: Ptr MAP -> IO Word64

foreign import ccall safe "tcmapkeys"
  c_tcmapkeys :: Ptr MAP -> IO (Ptr LIST)

foreign import ccall safe "tcmapvals"
  c_tcmapvals :: Ptr MAP -> IO (Ptr LIST)

foreign import ccall safe "tcmapaddint"
  c_tcmapaddint :: Ptr MAP -> Ptr Word8 -> CInt -> CInt -> IO CInt

foreign import ccall safe "tcmapadddouble"
  c_tcmapadddouble :: Ptr MAP -> Ptr Word8 -> CInt -> CDouble -> IO CDouble

foreign import ccall safe "tcmapclear"
  c_tcmapclear :: Ptr MAP -> IO ()

foreign import ccall safe "tcmapcutfront"
  c_tcmapcutfront :: Ptr MAP -> CInt -> IO ()

foreign import ccall safe "tcmapdump"
  c_tcmapdump :: Ptr MAP -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcmapload"
  c_tcmapload :: Ptr Word8 -> Ptr CInt -> IO (Ptr MAP)
