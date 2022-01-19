data Rec = Rec { field :: Bool }

test :: Rec
test = [wingman| ctor Rec ; use const , selector  |]

