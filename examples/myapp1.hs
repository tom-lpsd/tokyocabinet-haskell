import System
import MyApp1.TokyoCabinet

main :: IO ()
main = do args <- getArgs
          case head args of
            "-h" -> (new :: IO HDB) >>= main' 
            "-b" -> (new :: IO BDB) >>= main' 
            "-f" -> (new :: IO FDB) >>= main' 
            _ -> putStrLn "./myapp1 [-h|-b|-f]"
    where
      main' tc = do
        let ext = defaultExtension tc
        v <- flip runTCM tc $ do
                 open ("foo" ++ ext) [OWRITER, OCREAT]
                 put "100" "bar"
                 putcat "100" "bar"
                 v <- get "100"
                 close
                 return v
        print (v :: Maybe String)
