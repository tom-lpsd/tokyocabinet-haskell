module Main where

import TestUtil
import Test.HUnit hiding (path)

import Database.TokyoCabinet
import Database.TokyoCabinet.Storable
import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L

import Data.Int
import Data.Word

import Control.Monad.Trans (liftIO)

e @=?: a = liftIO $ e @=? a

check :: (TCDB tc, Eq a, Storable a) => tc -> a -> a -> TCM ()
check tc k v = do put tc k v
                  get tc k >>= (Just v @=?:)
                  iterinit tc
                  iternext tc >>= (Just k @=?:)

bdb :: TCM BDB
bdb = new

hdb :: TCM HDB
hdb = new

fdb :: TCM FDB
fdb = new

string = "100"
bstring = S.pack "100"
lstring = L.pack "100000"

int = 100 :: Int
int8 = 100 :: Int8
int16 = 100 :: Int16
int32 = 100 :: Int32
int64 = 100 :: Int64

word8 = 100 :: Word8
word16 = 100 :: Word16
word32 = 100 :: Word32
word64 = 100 :: Word64

double = 100 :: Double
float = 100 :: Float

char = '1'

dbname tc = "foo" ++ (defaultExtension tc)

make_check :: (TCDB tc, Eq a, Storable a) => TCM tc -> a -> IO Bool
make_check tcm k = runTCM $ do
                     tc <- tcm
                     liftIO $ withoutFile (dbname tc) $ \fn ->
                         runTCM $ do
                              open tc fn [OWRITER, OCREAT]
                              check tc k k
                              close tc

tests = test [
          make_check bdb string
        , make_check hdb string
        , make_check fdb string
        , make_check bdb bstring
        , make_check hdb bstring
        , make_check fdb bstring
        , make_check bdb lstring
        , make_check hdb lstring
        , make_check fdb lstring
        , make_check bdb int
        , make_check hdb int
        , make_check fdb int
        , make_check bdb [int]
        , make_check hdb [int]
        , make_check fdb [int]
        , make_check bdb int8
        , make_check hdb int8
        , make_check fdb int8
        , make_check bdb [int8]
        , make_check hdb [int8]
        , make_check fdb [int8]
        , make_check bdb int16
        , make_check hdb int16
        , make_check fdb int16
        , make_check bdb [int16]
        , make_check hdb [int16]
        , make_check fdb [int16]
        , make_check bdb int32
        , make_check hdb int32
        , make_check fdb int32
        , make_check bdb [int32]
        , make_check hdb [int32]
        , make_check fdb [int32]
        , make_check bdb int64
        , make_check hdb int64
        , make_check fdb int64
        , make_check bdb [int64]
        , make_check hdb [int64]
        , make_check fdb [int64]
        , make_check bdb word8
        , make_check hdb word8
        , make_check fdb word8
        , make_check bdb [word8]
        , make_check hdb [word8]
        , make_check fdb [word8]
        , make_check bdb word16
        , make_check hdb word16
        , make_check fdb word16
        , make_check bdb [word16]
        , make_check hdb [word16]
        , make_check fdb [word16]
        , make_check bdb word32
        , make_check hdb word32
        , make_check fdb word32
        , make_check bdb [word32]
        , make_check hdb [word32]
        , make_check fdb [word32]
        , make_check bdb word64
        , make_check hdb word64
        , make_check fdb word64
        , make_check bdb [word64]
        , make_check hdb [word64]
        , make_check fdb [word64]
        , make_check bdb double
        , make_check hdb double
        , make_check bdb [double]
        , make_check hdb [double]
        , make_check bdb float
        , make_check hdb float
        , make_check bdb [float]
        , make_check hdb [float]
        , make_check bdb char
        , make_check hdb char
        , make_check fdb char
        , make_check bdb [char]
        , make_check hdb [char]
        , make_check fdb [char]
        ]
                          

main = runTestTT tests
