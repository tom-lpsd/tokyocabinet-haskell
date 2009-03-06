{-# INCLUDE <tcadb.h> #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Database.TokyoCabinet where

import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

newtype Tcadb = Tcadb (Ptr Tcadb)

foreign import ccall "tcadbnew" c_tcadbnew :: IO Tcadb
foreign import ccall "tcadbopen" c_tcadbopen
    :: Tcadb -> CString -> IO Bool
foreign import ccall "tcadbput2" c_tcadbput2
    :: Tcadb -> CString -> CString -> IO Bool
foreign import ccall "tcadbclose" c_tcadbclose
    :: Tcadb -> IO Bool
