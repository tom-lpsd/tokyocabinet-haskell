module Database.TokyoCabinet.BDB
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
    , oTSYNC
    , OpenMode
    -- * tuning option
    , tLARGE
    , tDEFLATE
    , tBZIP
    , tTCBS
    , tEXCODEC
    , TuningOption
    , new
    , delete
    , tune
    , setcache
    , setxmsiz
    , open
    , close
    , put
    , putkeep
    , putcat
    , putdup
    , putlist
    , out
    , outlist
    , get
    , getlist
    , vnum
    , vsiz
    , range
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
    ) where

import Foreign.ForeignPtr

import Database.TokyoCabinet.Error
import Database.TokyoCabinet.BDB.C
import qualified Database.TokyoCabinet.Storable as S

data TCBDB = TCBDB !(ForeignPtr BDB)

new = undefined
delete = undefined
tune = undefined
setcache = undefined
setxmsiz = undefined
open = undefined
close = undefined
put = undefined
putkeep = undefined
putcat = undefined
putdup = undefined
putlist = undefined
out = undefined
outlist = undefined
get = undefined
getlist = undefined
vnum = undefined
vsiz = undefined
range = undefined
fwmkeys = undefined
addint = undefined
adddouble = undefined
sync = undefined
optimize = undefined
vanish = undefined
copy = undefined
tranbegin = undefined
trancommit = undefined
tranabort = undefined
path = undefined
rnum = undefined
fsiz = undefined
