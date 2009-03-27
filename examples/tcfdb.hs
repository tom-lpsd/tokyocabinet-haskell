import Control.Monad
import Database.TokyoCabinet.FDB

main = do fdb <- new
          -- open the database
          open fdb "casket.tcf" [OWRITER, OCREAT] >>= err fdb
          -- store records
          puts fdb [(1, "one"), (12, "twelve"), (144, "one forty four")]
                    >>= err fdb . (all id)
          -- retrieve records
          get fdb (1 :: Int) >>= maybe (error "something goes wrong") putStrLn
          -- close the database
          close fdb >>= err fdb
    where
      puts :: FDB -> [(Int, String)] -> IO [Bool]
      puts fdb = mapM (uncurry $ put fdb)

      err :: FDB -> Bool -> IO ()
      err fdb = flip unless $ ecode fdb >>= error . show
