import Control.Monad
import Database.TokyoCabinet.BDB
import qualified Database.TokyoCabinet.BDB.Cursor as C

main :: IO ()
main =
    do bdb <- new
       -- open the database
       open bdb "casket.tcb" [OWRITER, OCREAT] >>= err bdb
       -- store records
       puts bdb [ ("foo", "hop"), ("bar", "step"), ("baz", "jump") ]
                >>= err bdb . (all id)
       -- retrieve records
       get bdb "foo" >>= maybe (error "something goes wrong") putStrLn
       -- traverse records
       cur <- C.new bdb
       C.first cur >>= err bdb
       iter cur >>= putStrLn . show
       -- close the database
       close bdb >>= err bdb
    where
      puts :: BDB -> [(String, String)] -> IO [Bool]
      puts bdb = mapM (uncurry $ put bdb)

      err :: BDB -> Bool -> IO ()
      err bdb = flip unless $ ecode bdb >>= error . show

      iter :: C.BDBCUR -> IO [(String, String)]
      iter cur = do
        [key, value] <- sequence [C.key cur, C.val cur]
        case (key, value) of
          (Just k, Just v) -> C.next cur >> iter cur >>= return . ((k,v):)
          _ -> return []
