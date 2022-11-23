{-# LANGUAGE ViewPatterns #-}

splitLookup :: [(Int, String)] -> String
splitLookup (lookup 5 -> Nothing) = _w0
splitLookup (lookup 5 -> (Just s)) = _w1

