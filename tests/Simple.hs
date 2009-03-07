import Database.TokyoCabinet
import Foreign.C.String

main = do
  file <- newCString "foo.bdb"
  a <- c_tcadbnew
  c_tcadbopen a file
  k <- newCString "foo"
  v <- newCString "bar"
  c_tcadbput2 a k v
  c_tcadbclose a
