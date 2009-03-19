{-# LANGUAGE TypeSynonymInstances #-}
module Database.TokyoCabinet.Storable where

import Data.Int
import Data.Word

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal (peekArray, free)
import Data.ByteString.Unsafe

import qualified Foreign.Storable as F
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Foreign.Marshal.Utils as U

type PtrLen = (Ptr Word8, CInt)

liftPL :: (a -> (CStringLen -> IO b) -> IO b) -> a -> (PtrLen -> IO b) -> IO b
liftPL f val action =
    f val $ \(buf, siz) ->
        action (castPtr buf, fromIntegral siz)

class Storable a where
    withPtrLen :: a -> (PtrLen -> IO b) -> IO b
    peekPtrLen :: PtrLen -> IO a

instance Storable S.ByteString where
    withPtrLen = liftPL unsafeUseAsCStringLen
    peekPtrLen (p, len) =
        unsafePackCStringFinalizer p (fromIntegral len) (free p)

instance Storable L.ByteString where
    withPtrLen = liftPL unsafeUseAsCStringLen . S.concat . L.toChunks
    peekPtrLen (p, len) = do xs <- peekArray (fromIntegral len) p
                             free p
                             return $ L.pack xs

instance Storable String where
    withPtrLen = liftPL withCStringLen
    peekPtrLen (buf, siz) = do
      val <- peekCStringLen (castPtr buf, fromIntegral siz)
      free buf
      return val

withPtrLenForFStorable :: (F.Storable a) => a -> (PtrLen -> IO b) -> IO b
withPtrLenForFStorable n f =
    U.with n $ \p -> f (castPtr p, fromIntegral $ F.sizeOf n)

peekPtrLenForFStorable :: (F.Storable a) => PtrLen -> IO a
peekPtrLenForFStorable (p, _) = do val <- F.peek (castPtr p)
                                   free p
                                   return val

instance Storable CInt where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

instance Storable Int where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

instance Storable CDouble where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

instance Storable Double where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

instance Storable CFloat where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

instance Storable Int8 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

instance Storable Int16 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

instance Storable Int32 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

instance Storable Int64 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

instance Storable Word8 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

instance Storable Word16 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

instance Storable Word32 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

instance Storable Word64 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable

