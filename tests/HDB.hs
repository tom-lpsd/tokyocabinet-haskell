import Prelude hiding (putStrLn)
import Database.TokyoCabinet.HDB
import Data.ByteString.Char8

main = do
  h <- new
  open h "foo.tch" [oWRITER, oCREAT]
  put h (pack "foo") (pack "bar")
  v <- get h (pack "foo")
  putStrLn v
  close h
