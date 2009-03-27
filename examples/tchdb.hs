import Control.Monad
import Database.TokyoCabinet.HDB

main = do hdb <- new
          -- open the database
          open hdb "casket.tch" [OWRITER, OCREAT] >>= err hdb
          -- store records
          puts hdb [("foo", "hop"), ("bar", "step"), ("baz", "jump")]
                   >>= err hdb . (all id)
          -- retrieve records
          get_print hdb "foo"
          -- traverse records
          iterinit hdb
          iter hdb >>= mapM_ (\k -> putStr (k++":") >> get_print hdb k)
          -- close the database
          close hdb >>= err hdb
    where
      puts :: HDB -> [(String, String)] -> IO [Bool]
      puts hdb = mapM (uncurry $ put hdb)

      get_print :: HDB -> String -> IO ()
      get_print hdb key = get hdb key >>=
                          maybe (error "something goes wrong") putStrLn

      err :: HDB -> Bool -> IO ()
      err hdb = flip unless $ ecode hdb >>= error . show

      iter :: HDB -> IO [String]
      iter hdb = iternext hdb >>=
                 maybe (return []) (\x -> return . (x:) =<< iter hdb)
