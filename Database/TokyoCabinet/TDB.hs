module Database.TokyoCabinet.TDB
    (
      TDB
    , ECODE(..)
    , OpenMode(..)
    , TuningOption(..)
    , IndexType(..)
    , new
    , delete
    , ecode
    , errmsg
    , tune
    , setcache
    , setxmsiz
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
    , fwmkeys
    , addint
    , adddouble
    , sync
    , optimize
    , vanish
    , copy
    , tranbegin
    , trancommit
    , tranabort
    , path
    , rnum
    , fsiz
    , setindex
    , genuid
    ) where

import Database.TokyoCabinet.TDB.C
import Database.TokyoCabinet.Map
import Database.TokyoCabinet.Error
import Database.TokyoCabinet.Internal
import Database.TokyoCabinet.Storable

import Data.Int
import Data.Word

import Foreign.ForeignPtr

data TDB = TDB { unTCTDB :: !(ForeignPtr TDB') }

new :: IO TDB
new = TDB `fmap` (c_tctdbnew >>= newForeignPtr tctdbFinalizer)

delete :: TDB -> IO ()
delete tdb = finalizeForeignPtr (unTCTDB tdb)

ecode :: TDB -> IO ECODE
ecode tdb = cintToError `fmap` withForeignPtr (unTCTDB tdb) c_tctdbecode

tune :: TDB -> Int64 -> Int8 -> Int8 -> [TuningOption] -> IO Bool
tune = undefined

setcache :: TDB -> Int32 -> Int32 -> Int32 -> IO Bool
setcache = undefined

setxmsiz :: TDB -> Int64 -> IO Bool
setxmsiz = undefined

open :: TDB -> String -> [OpenMode] -> IO Bool
open = openHelper c_tctdbopen unTCTDB combineOpenMode

close :: TDB -> IO Bool
close tdb = withForeignPtr (unTCTDB tdb) c_tctdbclose

put :: (Storable k, Associative m) => TDB -> k -> m -> IO Bool
put = undefined

putkeep :: (Storable k, Associative m) => TDB -> k -> m -> IO Bool
putkeep = undefined

putcat :: (Storable k, Associative m) => TDB -> k -> m -> IO Bool
putcat = undefined

out :: (Storable k) => TDB -> k -> IO Bool
out = undefined

get :: (Storable k, Associative m) => TDB -> k -> IO (Maybe m)
get = undefined

vsiz :: (Storable k) => TDB -> k -> IO (Maybe Int)
vsiz = undefined

iterinit :: TDB -> IO Bool
iterinit = undefined

iternext :: (Storable k) => TDB -> IO (Maybe k)
iternext = undefined

fwmkeys :: (Storable k1, Storable k2) => TDB -> k1 -> Int -> IO [k2]
fwmkeys = undefined

addint :: (Storable k) => TDB -> k -> Int -> IO (Maybe Int)
addint = undefined

adddouble :: (Storable k) => TDB -> k -> Double -> IO (Maybe Double)
adddouble = undefined

sync :: TDB -> IO Bool
sync = undefined

optimize :: TDB -> Int64 -> Int8 -> Int8 -> [TuningOption] -> IO Bool
optimize = undefined

vanish :: TDB -> IO Bool
vanish = undefined

copy :: TDB -> String -> IO Bool
copy = undefined

tranbegin :: TDB -> IO Bool
tranbegin = undefined

trancommit :: TDB -> IO Bool
trancommit = undefined

tranabort :: TDB -> IO Bool
tranabort = undefined

path :: TDB -> IO String
path = undefined

rnum :: TDB -> IO Word64
rnum = undefined

fsiz :: TDB -> IO Word64
fsiz = undefined

setindex :: TDB -> String -> IndexType -> IO Bool
setindex = undefined

genuid :: TDB -> IO Int64
genuid = undefined
