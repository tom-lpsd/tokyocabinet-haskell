module Database.TokyoCabinet.Associative where

import Data.Maybe

import Foreign.Ptr
import Foreign.ForeignPtr

import Database.TokyoCabinet.Map
import Database.TokyoCabinet.Map.C
import Database.TokyoCabinet.Storable

class Associative a where
    withMap  :: (Storable k, Storable v) => a k v -> (Ptr MAP -> IO b) -> IO b
    peekMap' :: (Storable k, Storable v) => Ptr MAP -> IO (a k v)

newtype AssocList k v =
    AssocList { unAssocList :: [(k, v)] } deriving (Eq, Ord, Show)

instance Associative AssocList where
    withMap (AssocList alist) action =
        do m <- new
           mapM_ (uncurry $ put m) alist
           result <- withForeignPtr (unMap m) action
           delete m
           return result
    peekMap' ptr = do m <- Map `fmap` newForeignPtr tcmapFinalizer ptr
                      iterinit m
                      AssocList `fmap` accumulate m []
        where
          accumulate m acc = do val <- iternext m
                                case val of
                                  Just k -> do (Just v) <- get m k
                                               ((k, v):) `fmap` accumulate m acc
                                  _ -> return acc
