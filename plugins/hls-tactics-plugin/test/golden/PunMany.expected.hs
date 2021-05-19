data Many
  = Hello { world :: String }
  | Goodbye { a :: Int, b :: Bool, c :: Many }

test :: Many -> Many
test Hello {world} = _
test Goodbye {a, b, c} = _

