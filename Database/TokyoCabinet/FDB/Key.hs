module Database.TokyoCabinet.FDB.Key where

import Database.TokyoCabinet.FDB.C (ID(..), iDMIN, iDMAX, iDPREV, iDNEXT)

import Data.Int
import Data.Word

class Key a where
    toID   :: a -> ID
    fromID :: ID -> a

instance Key Int where
    toID = ID . fromIntegral
    fromID = fromIntegral . unID

instance Key Int8 where
    toID = ID . fromIntegral
    fromID = fromIntegral . unID

instance Key Int16 where
    toID = ID . fromIntegral
    fromID = fromIntegral . unID

instance Key Int32 where
    toID = ID . fromIntegral
    fromID = fromIntegral . unID

instance Key Int64 where
    toID = ID
    fromID = fromIntegral . unID

instance Key Word8 where
    toID = ID . fromIntegral
    fromID = fromIntegral . unID

instance Key Word16 where
    toID = ID . fromIntegral
    fromID = fromIntegral . unID

instance Key Word32 where
    toID = ID . fromIntegral
    fromID = fromIntegral . unID

instance Key Word64 where
    toID = ID . fromIntegral
    fromID = fromIntegral . unID

instance Key ID where
    toID = id
    fromID = id

instance Key String where
    toID "min"  = iDMIN
    toID "max"  = iDMAX
    toID "prev" = iDPREV
    toID "next" = iDNEXT
    toID idstr  = ID (read idstr)
    fromID = show . unID
