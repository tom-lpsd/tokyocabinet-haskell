{-# LANGUAGE TypeSynonymInstances #-}
module Database.TokyoCabinet.Storable where

import Data.Int
import Data.Word

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString.Unsafe

import qualified Foreign.Storable as F
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Foreign.Marshal.Utils as U

type PtrLen = (Ptr CChar, Int)

class Storable a where
    withPtrLen :: a -> (PtrLen -> IO b) -> IO b

instance Storable S.ByteString where
    withPtrLen = unsafeUseAsCStringLen

instance Storable L.ByteString where
    withPtrLen = unsafeUseAsCStringLen . S.concat . L.toChunks

instance Storable String where
    withPtrLen = withCStringLen

instance Storable CInt where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)

instance Storable CDouble where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)

instance Storable CFloat where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)

instance Storable Int8 where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)

instance Storable Int16 where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)

instance Storable Int32 where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)

instance Storable Word8 where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)

instance Storable Word16 where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)

instance Storable Word32 where
    withPtrLen n f = U.with n $ \p -> f (castPtr p, F.sizeOf n)
