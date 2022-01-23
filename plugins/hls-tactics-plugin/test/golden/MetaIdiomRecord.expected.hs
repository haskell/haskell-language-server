data Rec = Rec
  { a :: Int
  , b :: Bool
  }

test :: Maybe Rec
test = (Rec <$> _w0) <*> _w1

