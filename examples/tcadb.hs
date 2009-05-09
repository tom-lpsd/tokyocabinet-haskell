import Control.Monad
import Database.TokyoCabinet.ADB

main = do adb <- new
          -- open the abstract database object
          -- "+" means that the database will be an on-memory tree database
          open adb "+" >>= err adb "open failed"
          -- store records
          puts adb [("foo", "hop"), ("bar", "step"), ("baz", "jump")]
                   >>= err adb "put failed" . (all id)
          -- retrieve records
          get_print adb "foo"
          -- traverse records
          iterinit adb
          iter adb >>= mapM_ (\k -> putStr (k++":") >> get_print adb k)
          -- close the database
          close adb >>= err adb "close failed"
    where
      puts :: ADB -> [(String, String)] -> IO [Bool]
      puts adb = mapM (uncurry $ put adb)

      get_print :: ADB -> String -> IO ()
      get_print adb key = get adb key >>=
                          maybe (error "something goes wrong") putStrLn

      err :: ADB -> String -> Bool -> IO ()
      err adb msg = flip unless $ error msg

      iter :: ADB -> IO [String]
      iter adb = iternext adb >>=
                 maybe (return []) (\x -> return . (x:) =<< iter adb)
