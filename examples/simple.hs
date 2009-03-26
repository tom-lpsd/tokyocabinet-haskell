import Database.TokyoCabinet
import Data.ByteString.Char8 (ByteString, pack)

putsample :: String -> [(ByteString, ByteString)] -> TCM Bool
putsample file kv =
    do tc <- new :: TCM TCHDB
       open tc file [OWRITER, OCREAT]
       mapM (uncurry $ put tc) kv
       close tc
              
getsample :: String -> ByteString -> TCM (Maybe ByteString)
getsample file key =
    do tc <- new :: TCM TCHDB
       open tc file [OREADER]
       val <- get tc key
       close tc
       return val

main = runTCM (do putsample "foo.tch" [(pack "foo", pack "bar")]
                  getsample "foo.tch" (pack "foo")) >>=
       maybe (return ()) (putStrLn . show)
