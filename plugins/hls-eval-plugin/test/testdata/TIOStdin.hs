-- 1. Support `stdin`
module TIOStdin where

{- `stdin` is empty, so reads see end of file instead of blocking the server.

>>> getLine >>= print
-}

{- Reading `stdin` works repeatedly.

>>> getLine >>= print
-}
