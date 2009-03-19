{-# INCLUDE <tcutil.h> #-}
{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Database.TokyoCabinet.List.C where

import Data.Word

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

data LIST

foreign import ccall safe "tclistnew"
  c_tclistnew :: IO (Ptr LIST)

foreign import ccall safe "tclistnew2"
  c_tclistnew2 :: CInt -> IO (Ptr LIST)

foreign import ccall safe "tclistdup"
  c_tclistdup :: Ptr LIST -> IO (Ptr LIST)

foreign import ccall safe "tclistdel"
  c_tclistdel :: Ptr LIST -> IO ()

foreign import ccall "&tclistdel"
  tclistFinalizer :: FunPtr (Ptr LIST -> IO ())

foreign import ccall safe "tclistnum"
  c_tclistnum :: Ptr LIST -> IO CInt

foreign import ccall safe "tclistval"
  c_tclistval :: Ptr LIST -> CInt -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tclistval2"
  c_tclistval2 :: Ptr LIST -> CInt -> IO CString

foreign import ccall safe "tclistpush"
  c_tclistpush :: Ptr LIST -> Ptr Word8 -> CInt -> IO ()

foreign import ccall safe "tclistpush2"
  c_tclistpush2 :: Ptr LIST -> CString -> IO ()

foreign import ccall safe "tclistpop"
  c_tclistpop :: Ptr LIST -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tclistpop2"
  c_tclistpop2 :: Ptr LIST -> IO CString

foreign import ccall safe "tclistunshift"
  c_tclistunshift :: Ptr LIST -> Ptr Word8 -> CInt -> IO ()

foreign import ccall safe "tclistunshift2"
  c_tclistunshift2 :: Ptr LIST -> Ptr Word8 -> IO ()

foreign import ccall safe "tclistshift"
  c_tclistshift :: Ptr LIST -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tclistshift2"
  c_tclistshift2 :: Ptr LIST -> IO CString

foreign import ccall safe "tclistinsert"
  c_tclistinsert :: Ptr LIST -> CInt -> Ptr Word8 -> CInt -> IO ()

foreign import ccall safe "tclistinsert2"
  c_tclistinsert2 :: Ptr LIST -> CInt -> Ptr Word8 -> IO ()

foreign import ccall safe "tclistremove"
  c_tclistremove :: Ptr LIST -> CInt -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tclistremove2"
  c_tclistremove2 :: Ptr LIST -> CInt -> IO CString

foreign import ccall safe "tclistover"
  c_tclistover :: Ptr LIST -> CInt -> Ptr Word8 -> CInt -> IO ()

foreign import ccall safe "tclistover2"
  c_tclistover2 :: Ptr LIST -> CInt -> CString -> IO ()

foreign import ccall safe "tclistsort"
  c_tclistsort :: Ptr LIST -> IO ()

foreign import ccall safe "tclistlsearch"
  c_tclistlsearch :: Ptr LIST -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall safe "tclistbsearch"
  c_tclistbsearch :: Ptr LIST -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall safe "tclistclear"
  c_tclistclear :: Ptr LIST -> IO ()

foreign import ccall safe "tclistdump"
  c_tclistdump :: Ptr LIST -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tclistload"
  c_tclistload :: Ptr Word8 -> CInt -> IO (Ptr LIST)
