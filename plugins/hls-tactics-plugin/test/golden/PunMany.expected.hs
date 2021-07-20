data Many
  = Hello { world :: String }
  | Goodbye { a :: Int, b :: Bool, c :: Many }

test :: Many -> Many
test Hello {world} = _w0
test Goodbye {a, b, c} = _w1

