{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Database.TokyoCabinet.BDB.Cursor
    (
      CursorPutMode(..)
    , new
    , delete
    , first
    , last
    , jump
    , prev
    , next
    , put
    , out
    , key
    , val
    , TCBDBCUR
    ) where

import Prelude hiding (last)

import Database.TokyoCabinet.BDB.C
import Database.TokyoCabinet.BDB.Cursor.C
import qualified Database.TokyoCabinet.Storable as S

import Foreign.ForeignPtr

import Foreign.Marshal (alloca)
import Foreign.Storable (peek)
import Foreign.Marshal.Utils (maybePeek)

data TCBDBCUR = TCBDBCUR !(ForeignPtr CUR) TCBDB

unTCBDBCUR :: TCBDBCUR -> ForeignPtr CUR
unTCBDBCUR (TCBDBCUR cur _) = cur

new :: TCBDB -> IO TCBDBCUR
new bdb =
    withForeignPtr (unTCBDB bdb) $ \bdb' -> do
      cur <- c_tcbdbcurnew bdb'
      flip TCBDBCUR bdb `fmap` newForeignPtr tcbdbcurFinalizer cur

delete :: TCBDBCUR -> IO ()
delete cur = finalizeForeignPtr (unTCBDBCUR cur)

first :: TCBDBCUR -> IO Bool
first cur = withForeignPtr (unTCBDBCUR cur) c_tcbdbcurfirst

last :: TCBDBCUR -> IO Bool
last cur = withForeignPtr (unTCBDBCUR cur) c_tcbdbcurlast

jump :: (S.Storable k) => TCBDBCUR -> k -> IO Bool
jump cur k =
    withForeignPtr (unTCBDBCUR cur) $ \cur' ->
        S.withPtrLen k $ \(kbuf, ksiz) ->
            c_tcbdbcurjump cur' kbuf ksiz

prev :: TCBDBCUR -> IO Bool
prev cur = withForeignPtr (unTCBDBCUR cur) c_tcbdbcurprev

next :: TCBDBCUR -> IO Bool
next cur = withForeignPtr (unTCBDBCUR cur) c_tcbdbcurnext

put :: (S.Storable v) => TCBDBCUR -> v -> CursorPutMode -> IO Bool
put cur v mode =
    withForeignPtr (unTCBDBCUR cur) $ \cur' ->
        S.withPtrLen v $ \(vbuf, vsiz) ->
            c_tcbdbcurput cur' vbuf vsiz (cpToCInt mode)

out :: TCBDBCUR -> IO Bool
out cur = withForeignPtr (unTCBDBCUR cur) c_tcbdbcurout

key :: (S.Storable k) => TCBDBCUR -> IO (Maybe k)
key cur =
    withForeignPtr (unTCBDBCUR cur) $ \cur' ->
        alloca $ \sizbuf -> do
          vbuf <- c_tcbdbcurkey cur' sizbuf
          vsiz <- fromIntegral `fmap` peek sizbuf
          flip maybePeek vbuf $ \vbuf' -> S.peekPtrLen (vbuf', vsiz) 

val :: (S.Storable v) => TCBDBCUR -> IO (Maybe v)
val cur =
    withForeignPtr (unTCBDBCUR cur) $ \cur' ->
        alloca $ \sizbuf -> do
          vbuf <- c_tcbdbcurval cur' sizbuf
          vsiz <- fromIntegral `fmap` peek sizbuf
          flip maybePeek vbuf $ \vbuf' -> S.peekPtrLen (vbuf', vsiz) 
