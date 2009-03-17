{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Database.TokyoCabinet.FDB.C where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.Int
import Data.Word

import Database.TokyoCabinet.List.C (LIST)

#include <tcfdb.h>

newtype OpenMode = OpenMode { unOpenMode :: CInt }
    deriving (Eq, Show)

#{enum OpenMode, OpenMode
 , oREADER = FDBOREADER
 , oWRITER = FDBOWRITER
 , oCREAT  = FDBOCREAT
 , oTRUNC  = FDBOTRUNC
 , oNOLCK  = FDBONOLCK
 , oLCKNB  = FDBOLCKNB
}

newtype ID = ID { unID :: CInt }

#{enum ID, ID
 , idMIN  = FDBIDMIN
 , idPREV = FDBIDPREV
 , idMAX  = FDBIDMAX
 , idNEXT = FDBIDNEXT
};

data FDB

foreign import ccall safe "tcfdberrmsg"
  c_tcfdberrmsg :: CInt -> CString

foreign import ccall safe "tcfdbnew"
  c_tcfdbnew :: IO (Ptr FDB)

foreign import ccall safe "tcfdbdel"
  c_tcfdbdel :: Ptr FDB -> IO ()

foreign import ccall safe "&tcfdbdel"
  tcfdbFinalizer :: FunPtr (Ptr FDB -> IO ())

foreign import ccall safe "tcfdbecode"
  c_tcfdbecode :: Ptr FDB -> IO CInt

foreign import ccall safe "tcfdbsetmutex"
  c_tcfdbsetmutex :: Ptr FDB -> IO Bool

foreign import ccall safe "tcfdbtune"
  c_tcfdbtune :: Ptr FDB -> Int32 -> Int64 -> IO Bool

foreign import ccall safe "tcfdbopen"
  c_tcfdbopen :: Ptr FDB -> CString -> CInt -> IO Bool

foreign import ccall safe "tcfdbclose"
  c_tcfdbclose :: Ptr FDB -> IO Bool

foreign import ccall safe "tcfdbput"
  c_tcfdbput :: Ptr FDB -> Int64 -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcfdbput2"
  c_tcfdbput2 :: Ptr FDB -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcfdbput3"
  c_tcfdbput3 :: Ptr FDB -> CString -> CString -> IO Bool

foreign import ccall safe "tcfdbputkeep"
  c_tcfdbputkeep :: Ptr FDB -> Int64 -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcfdbputkeep2"
  c_tcfdbputkeep2 ::
      Ptr FDB -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcfdbputkeep3"
  c_tcfdbputkeep3 :: Ptr FDB -> CString -> CString -> IO Bool

foreign import ccall safe "tcfdbputcat"
  c_tcfdbputcat :: Ptr FDB -> Int64 -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcfdbputcat2"
  c_tcfdbputcat2 :: Ptr FDB -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcfdbputcat3"
  c_tcfdbputcat3 :: Ptr FDB -> CString -> CString -> IO Bool

foreign import ccall safe "tcfdbout"
  c_tcfdbout :: Ptr FDB -> Int64 -> IO Bool

foreign import ccall safe "tcfdbout2"
  c_tcfdbout2 :: Ptr FDB -> Ptr Word8 -> CInt -> IO Bool

foreign import ccall safe "tcfdbout3"
  c_tcfdbout3 :: Ptr FDB -> CString -> IO Bool

foreign import ccall safe "tcfdbget"
  c_tcfdbget :: Ptr FDB -> Int64 -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcfdbget2"
  c_tcfdbget2 :: Ptr FDB -> Ptr Word8 -> CInt -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcfdbget3"
  c_tcfdbget3 :: Ptr FDB -> CString -> IO CString

foreign import ccall safe "tcfdbget4"
  c_tcfdbget4 :: Ptr FDB -> Int64 -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall safe "tcfdbvsiz"
  c_tcfdbvsiz :: Ptr FDB -> Int64 -> IO CInt

foreign import ccall safe "tcfdbvsiz2"
  c_tcfdbvsiz2 :: Ptr FDB -> Ptr Word8 -> CInt -> IO CInt

foreign import ccall safe "tcfdbvsiz3"
  c_tcfdbvsiz3 :: Ptr FDB -> CString -> IO CInt

foreign import ccall safe "tcfdbiterinit"
  c_tcfdbiterinit :: Ptr FDB -> IO Bool

foreign import ccall safe "tcfdbiternext"
  c_tcfdbiternext :: Ptr FDB -> IO Int64

foreign import ccall safe "tcfdbiternext2"
  c_tcfdbiternext2 :: Ptr FDB -> Ptr CInt -> IO (Ptr Word8)

foreign import ccall safe "tcfdbiternext3"
  c_tcfdbiternext3 :: Ptr FDB -> IO CString

foreign import ccall safe "tcfdbrange"
  c_tcfdbrange ::
      Ptr FDB -> Int64 -> Int64 -> CInt -> Ptr CInt -> IO (Ptr Int64)

foreign import ccall safe "tcfdbrange2"
  c_tcfdbrange2 ::
      Ptr FDB -> Ptr Word8 -> CInt -> Ptr Word8 -> CInt -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tcfdbrange3"
  c_tcfdbrange3 :: Ptr FDB -> CString -> CString -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tcfdbrange4"
  c_tcfdbrange4 :: Ptr FDB -> Ptr Word8 -> CInt -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tcfdbrange5"
  c_tcfdbrange5 :: Ptr FDB -> Ptr Word8 -> CInt -> IO (Ptr LIST)

foreign import ccall safe "tcfdbaddint"
  c_tcfdbaddint :: Ptr FDB -> Int64 -> CInt -> IO CInt

foreign import ccall safe "tcfdbadddouble"
  c_tcfdbadddouble :: Ptr FDB -> Int64 -> CDouble -> IO CDouble

foreign import ccall safe "tcfdbsync"
  c_tcfdbsync :: Ptr FDB -> IO Bool

foreign import ccall safe "tcfdboptimize"
  c_tcfdboptimize :: Ptr FDB -> Int32 -> Int64 -> IO Bool

foreign import ccall safe "tcfdbvanish"
  c_tcfdbvanish :: Ptr FDB -> IO Bool

foreign import ccall safe "tcfdbcopy"
  c_tcfdbcopy :: Ptr FDB -> CString -> IO Bool

foreign import ccall safe "tcfdbpath"
  c_tcfdbpath :: Ptr FDB -> IO CString

foreign import ccall safe "tcfdbrnum"
  c_tcfdbrnum :: Ptr FDB -> IO Int64

foreign import ccall safe "tcfdbfsiz"
  c_tcfdbfsiz :: Ptr FDB -> IO Int64
