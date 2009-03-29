module Database.TokyoCabinet.Storable where

import Data.Int
import Data.Char
import Data.Word

import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Marshal (peekArray, free)
import Foreign.Marshal.Array (withArray)
import Data.ByteString.Unsafe

import qualified Foreign.Storable as F
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Foreign.Marshal.Utils as U

type PtrLen = (Ptr Word8, CInt)

liftPL :: (a -> (CStringLen -> IO b) -> IO b) -> a -> (PtrLen -> IO b) -> IO b
liftPL f val action =
    f val $ \(buf, siz) ->
        action (castPtr buf, fromIntegral siz)

class (Show a, Read a) => Storable a where
    withPtrLen :: a -> (PtrLen -> IO b) -> IO b
    withPtrLenL :: [a] -> (PtrLen -> IO b) -> IO b

    peekPtrLenL :: PtrLen -> a -> IO [a]
    peekPtrLen :: PtrLen -> IO a

    toInt64 :: a -> Int64
    toInt64L :: [a] -> Int64

    fromString :: String -> a
    fromStringL :: String -> [a]

    toInt64 = read . show
    toInt64L = read . concatMap show

    fromString = read
    fromStringL = (:[]) . fromString

    withPtrLenL = undefined
    peekPtrLenL = undefined

instance Storable C.ByteString where
    withPtrLen = liftPL unsafeUseAsCStringLen
    peekPtrLen (p, len) =
        unsafePackCStringFinalizer p (fromIntegral len) (free p)
    toInt64 = read . C.unpack
    fromString = C.pack

instance Storable L.ByteString where
    withPtrLen = liftPL unsafeUseAsCStringLen . C.concat . L.toChunks
    peekPtrLen (p, len) = do xs <- peekArray (fromIntegral len) p
                             free p
                             return $ L.pack xs
    toInt64 = read . LC.unpack
    fromString = LC.pack

withPtrLenForFStorable :: (F.Storable a) => a -> (PtrLen -> IO b) -> IO b
withPtrLenForFStorable n f =
    U.with n $ \p -> f (castPtr p, fromIntegral $ F.sizeOf n)

peekPtrLenForFStorable :: (F.Storable a) => PtrLen -> IO a
peekPtrLenForFStorable (p, _) = do val <- F.peek (castPtr p)
                                   free p
                                   return val

withPtrLenLForFStorable :: (F.Storable a) => [a] -> (PtrLen -> IO b) -> IO b
withPtrLenLForFStorable xs f =
    withArray xs $ \p ->
        f (castPtr p, fromIntegral $ (F.sizeOf $ head xs) * length xs)

peekPtrLenLForFStorable :: (F.Storable a) => PtrLen -> a -> IO [a]
peekPtrLenLForFStorable (p, size) x = do
  peekArray (fromIntegral size `div` (F.sizeOf x)) (castPtr p)

instance Storable Char where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = liftPL withCStringLen
    peekPtrLenL (buf, siz) _ =
        do val <- peekCStringLen (castPtr buf, fromIntegral siz)
           free buf
           return val

    toInt64 c | isDigit c = fromIntegral $ digitToInt c
    toInt64 c | otherwise = fromIntegral $ ord c

    toInt64L cs = read cs

    fromString str = read ('\'':str ++ "'")
    fromStringL = id

instance Storable CInt where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable Int where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable CDouble where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable Double where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable CFloat where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable Float where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable Int8 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable Int16 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable Int32 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable Int64 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable Word8 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable Word16 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable Word32 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance Storable Word64 where
    withPtrLen = withPtrLenForFStorable
    peekPtrLen = peekPtrLenForFStorable
    withPtrLenL = withPtrLenLForFStorable
    peekPtrLenL = peekPtrLenLForFStorable

instance (F.Storable a, Storable a) => Storable [a] where
    withPtrLen = withPtrLenL
    peekPtrLen xs = peekPtrLenL xs undefined
    toInt64 = toInt64L
    fromString = fromStringL
