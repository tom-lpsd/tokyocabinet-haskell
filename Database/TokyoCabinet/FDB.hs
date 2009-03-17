module Database.TokyoCabinet.FDB
    (
    -- * error type and utility
      TCErrorCode
    , errmsg
    , eSUCCESS
    , eTHREAD
    , eINVALID
    , eNOFILE
    , eNOPERM
    , eMETA
    , eRHEAD
    , eOPEN
    , eCLOSE
    , eTRUNC
    , eSYNC
    , eSTAT
    , eSEEK
    , eREAD
    , eWRITE
    , eMMAP
    , eLOCK
    , eUNLINK
    , eRENAME
    , eMKDIR
    , eRMDIR
    , eKEEP
    , eNOREC
    , eMISC
    -- * open mode
    , oREADER
    , oWRITER
    , oCREAT
    , oTRUNC
    , oNOLCK
    , oLCKNB
    -- * basic api
    , new
    , delete
    , ecode
    , tune
    , open
    , close
    , put
    , putkeep
    , putcat
    , out
    , get
    , vsiz
    , iterinit
    , iternext
    , range
    , addint
    , adddouble
    , sync
    , optimize
    , vanish
    , copy
    , path
    , rnum
    , fsiz
    ) where

import Database.TokyoCabinet.FDB.C
import Database.TokyoCabinet.Error
import qualified Database.TokyoCabinet.Storable as S

import Foreign.ForeignPtr

import Data.Int

data TCFDB = TCFDB !(ForeignPtr FDB)

new :: IO TCFDB
new = undefined

delete :: TCFDB -> IO ()
delete = undefined

ecode :: TCFDB -> IO TCErrorCode
ecode = undefined

tune :: TCFDB -> Int32 -> Int64 -> IO Bool
tune = undefined

open :: TCFDB -> String -> [OpenMode] -> IO Bool
open = undefined

close :: TCFDB -> IO Bool
close = undefined

put :: (S.Storable a, S.Storable b) => TCFDB -> a -> b -> IO Bool
put = undefined

putkeep :: (S.Storable a, S.Storable b) => TCFDB -> a -> b -> IO Bool
putkeep = undefined

putcat :: (S.Storable a, S.Storable b) => TCFDB -> a -> b -> IO Bool
putcat = undefined

out :: (S.Storable a) => TCFDB -> a -> IO Bool
out = undefined

get :: (S.Storable a, S.Storable b) => TCFDB -> a -> IO (Maybe b)
get = undefined

vsiz :: (S.Storable a) => TCFDB -> a -> IO (Maybe Int)
vsiz = undefined

iterinit :: TCFDB -> IO Bool
iterinit = undefined

iternext :: (S.Storable a) => TCFDB -> IO (Maybe a)
iternext = undefined

range :: (S.Storable a) => TCFDB -> String -> IO [a]
range = undefined

addint :: (S.Storable a) => TCFDB -> a -> Int -> IO Int
addint = undefined

adddouble :: (S.Storable a) => TCFDB -> a -> Double -> IO Double
adddouble = undefined

sync :: TCFDB -> IO Bool
sync = undefined

optimize :: TCFDB -> Int32 -> Int64 -> IO Bool
optimize = undefined

vanish :: TCFDB -> IO Bool
vanish = undefined

copy :: TCFDB -> String -> IO Bool
copy = undefined

path :: TCFDB -> IO (Maybe String)
path = undefined

rnum :: TCFDB -> IO Int64
rnum = undefined

fsiz :: TCFDB -> IO Int64
fsiz = undefined
