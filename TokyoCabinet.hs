{-# LANGUAGE ForeignFunctionInterface #-}
module TokyoCabinet where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

newtype Tcadb = Tcadb (Ptr Tcadb)

foreign import ccall "tcadb.h tcadbnew" c_tcadbnew :: IO Tcadb
foreign import ccall "tcadb.h tcadbopen" c_tcadbopen
    :: Tcadb -> CString -> IO Bool
foreign import ccall "tcadb.h tcadbput2" c_tcadbput2
    :: Tcadb -> CString -> CString -> IO Bool
foreign import ccall "tcadb.h tcadbclose" c_tcadbclose
    :: Tcadb -> IO Bool
