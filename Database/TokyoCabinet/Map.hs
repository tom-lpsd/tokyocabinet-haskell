module Database.TokyoCabinet.Map where

import Prelude hiding (lookup)

import Database.TokyoCabinet.Storable

class Associative a where
    lookup :: (Storable k) => k -> a -> v
