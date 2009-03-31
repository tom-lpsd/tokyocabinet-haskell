module Database.TokyoCabinet.Sequence where

import Foreign.Ptr
import Foreign.Storable (peek)
import Foreign.Marshal (alloca, mallocBytes, copyBytes)
import Foreign.ForeignPtr

import Database.TokyoCabinet.List.C
import Database.TokyoCabinet.Storable

class Sequence a where
    withList  :: (Storable s) => a s -> (Ptr LIST -> IO b) -> IO b
    peekList' :: (Storable s) => Ptr LIST -> IO (a s)
    empty :: (Storable s) => IO (a s)
    smap :: (Storable s1, Storable s2) => (s1 -> s2) -> a s1 -> IO (a s2)

instance Sequence List where
    withList xs action = withForeignPtr (unTCList xs) action
    peekList' tcls = List `fmap` newForeignPtr tclistFinalizer tcls
    empty = List `fmap` (c_tclistnew >>= newForeignPtr tclistFinalizer)
    smap f tcls =
        withForeignPtr (unTCList tcls) $ \tcls' ->
            alloca $ \sizbuf ->
                do num <- c_tclistnum tcls'
                   vals <- c_tclistnew
                   loop tcls' 0 num sizbuf vals
        where
          loop tcls' n num sizbuf acc
                | n < num = do vbuf <- c_tclistval tcls' n sizbuf
                               vsiz <- peek sizbuf
                               buf <- mallocBytes (fromIntegral vsiz)
                               copyBytes buf vbuf (fromIntegral vsiz)
                               val <- f `fmap` peekPtrLen (buf, vsiz)
                               withPtrLen val $ uncurry (c_tclistpush acc)
                               loop tcls' (n+1) num sizbuf acc
                | otherwise = List `fmap` newForeignPtr tclistFinalizer acc

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
    smap f = return . (map f)
