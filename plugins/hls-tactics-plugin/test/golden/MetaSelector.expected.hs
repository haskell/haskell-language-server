data Rec = Rec { field :: Bool }

test :: Rec
test = Rec (const field _w0)

