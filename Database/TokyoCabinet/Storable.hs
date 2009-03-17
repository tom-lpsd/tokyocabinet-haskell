{-# LANGUAGE TypeSynonymInstances #-}
module Database.TokyoCabinet.Storable where

import Data.Int
import Data.Word

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal (free)
import Data.ByteString.Unsafe

import qualified Foreign.Storable as F
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Foreign.Marshal.Utils as U

type PtrLen = (Ptr CChar, Int)

class Storable a where
    withPtrLen :: a -> (PtrLen -> IO b) -> IO b
    peekPtrLen :: PtrLen -> IO a
    peekPtrLen = undefined

instance Storable S.ByteString where
    withPtrLen = unsafeUseAsCStringLen
    peekPtrLen (p, len) = unsafePackCStringFinalizer (castPtr p) len (free p)

instance Storable L.ByteString where
    withPtrLen = unsafeUseAsCStringLen . S.concat . L.toChunks

instance Storable String where
    withPtrLen = withCStringLen
    peekPtrLen str@(p, _) = do
      val <- peekCStringLen str
      free p
      return val

instance Storable CInt where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)
    peekPtrLen (p, _) = do val <- F.peek (castPtr p)
                           free p
                           return val

instance Storable Int where
    withPtrLen n = withPtrLen (fromIntegral n :: CInt)
    peekPtrLen (p, _) = do val <- F.peek (castPtr p)
                           free p
                           return val

instance Storable CDouble where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)
    peekPtrLen (p, _) = do val <- F.peek (castPtr p)
                           free p
                           return val

instance Storable Double where
    withPtrLen n = withPtrLen (realToFrac n :: CDouble)
    peekPtrLen (p, _) = do val <- F.peek (castPtr p)
                           free p
                           return val

instance Storable CFloat where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)
    peekPtrLen (p, _) = do val <- F.peek (castPtr p)
                           free p
                           return val

instance Storable Int8 where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)
    peekPtrLen (p, _) = do val <- F.peek (castPtr p)
                           free p
                           return val

instance Storable Int16 where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)
    peekPtrLen (p, _) = do val <- F.peek (castPtr p)
                           free p
                           return val

instance Storable Int32 where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)
    peekPtrLen (p, _) = do val <- F.peek (castPtr p)
                           free p
                           return val

instance Storable Word8 where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)
    peekPtrLen (p, _) = do val <- F.peek (castPtr p)
                           free p
                           return val

instance Storable Word16 where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)
    peekPtrLen (p, _) = do val <- F.peek (castPtr p)
                           free p
                           return val

instance Storable Word32 where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)
    peekPtrLen (p, _) = do val <- F.peek (castPtr p)
                           free p
                           return val

