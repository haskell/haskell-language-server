test :: Bool -> Bool
test b = id $ (case b of
   False -> _w0
   True -> _w1)

