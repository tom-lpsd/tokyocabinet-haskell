module Database.TokyoCabinet.Internal where

import Database.TokyoCabinet.List.C
import Database.TokyoCabinet.Storable

import Foreign.Ptr
import Foreign.Storable (peek)
import Foreign.Marshal (alloca)

peekTCListAndFree :: (Storable a) => Ptr LIST -> IO [a]
peekTCListAndFree list = do
  vals <- peekTCList' list []
  c_tclistdel list
  return vals
 where
   peekTCList' tclist acc = 
       alloca $ \sizbuf ->
           do val <- c_tclistpop tclist sizbuf
              siz <- peek sizbuf
              if val == nullPtr
                then return acc
                else do elm <- peekPtrLen (val, siz)
                        peekTCList' tclist (elm:acc)
