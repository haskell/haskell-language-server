data Rec = Rec
  { a :: Int
  , b :: Bool
  }

test :: Maybe Rec
test = [wingman| idiom (ctor Rec) |]

