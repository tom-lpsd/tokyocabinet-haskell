import MyApp2.TokyoCabinet

import Control.Monad.Trans
import Control.Monad.Error

main :: IO ()
main = do h <- new :: IO HDB
          let ext = defaultExtension h
          v <- flip runTCM h $ do
                 open ("foo" ++ ext) [OWRITER, OCREAT]
                 put "100" "foo"
--                 throwError EMISC
                 v <- get "100"
                 close
                 return v
               `catchError` const (close >> fail "oops")
          case v of
            Left  e  -> putStrLn (show e)
            Right v' -> putStrLn (show (v' :: Maybe String))
