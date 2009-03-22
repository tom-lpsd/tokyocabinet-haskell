module Main where

import Control.Monad.Trans
import Control.Exception
import Database.TokyoCabinet

withTokyoCabinet :: (TCDB a) => String -> (a -> TCM b) -> TCM b
withTokyoCabinet fname action =
    liftIO $ bracket (runTCM open') (runTCM . close') (runTCM . action)
    where  open' = do tc <- new
                      open tc fname [OREADER, OWRITER, OCREAT]
                      return tc
           close' tc = close tc
