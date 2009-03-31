module Database.TokyoCabinet.Sequence where

import Foreign.Ptr
import Foreign.Storable (peek)
import Foreign.Marshal (alloca)
import Foreign.ForeignPtr

import Database.TokyoCabinet.List.C
import Database.TokyoCabinet.Storable

class Sequence a where
    withList  :: (Storable s) => a s -> (Ptr LIST -> IO b) -> IO b
    peekList' :: (Storable s) => Ptr LIST -> IO (a s)
    empty :: (Storable s) => IO (a s)

instance Sequence List where
    withList xs action = withForeignPtr (unTCList xs) action
    peekList' tcls = List `fmap` newForeignPtr tclistFinalizer tcls
    empty = List `fmap` (c_tclistnew >>= newForeignPtr tclistFinalizer)

instance Sequence [] where
    withList xs action =
        do list <- c_tclistnew
           mapM_ (push list) xs
           result <- action list
           c_tclistdel list
           return result
        where
          push list val = withPtrLen val $ uncurry (c_tclistpush list)

    peekList' tcls = do
        vals <- peekList'' tcls []
        c_tclistdel tcls
        return vals
      where
        peekList'' lis acc =
            alloca $ \sizbuf ->
                do val <- c_tclistpop lis sizbuf
                   siz <- peek sizbuf
                   if val == nullPtr
                     then return acc
                     else do elm <- peekPtrLen (val, siz)
                             peekList'' lis (elm:acc)

    empty = return []
