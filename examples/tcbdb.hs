import Control.Monad (unless)
import Database.TokyoCabinet.BDB
import qualified Database.TokyoCabinet.BDB.Cursor as C

main :: IO ()
main =
    do bdb <- new
       let err = flip unless (ecode bdb >>= error . show)
       -- open the database
       open bdb "casket.tcb" [OWRITER, OCREAT] >>= err
       -- store records
       puts bdb [ ("foo", "hop")
                , ("bar", "step")
                , ("baz", "jump") ] >>= err . (all id)
       -- retrieve records
       get bdb "foo" >>= maybe (error "something go wrong") putStrLn
       -- traverse records
       cur <- C.new bdb
       C.first cur >>= err
       iter cur >>= putStrLn . show
       -- close the database
       close bdb >>= err
    where
      puts :: TCBDB -> [(String, String)] -> IO [Bool]
      puts bdb = mapM (uncurry $ put bdb)

      iter :: C.TCBDBCUR -> IO [(String, String)]
      iter cur = do
        [key, value] <- sequence [C.key cur, C.val cur]
        case (key, value) of
          (Just k, Just v) -> C.next cur >> iter cur >>= return . ((k,v):)
          _ -> return []
