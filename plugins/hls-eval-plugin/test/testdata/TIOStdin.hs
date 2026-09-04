-- 1. Support `stdin`
module TIOStdin where

{- Feed `stdin` with empty data.

Avoid server hangs indefinitely, waiting for `stdin` to terminate.

Shows a clear error message.

>>> getLine >>= print
<stdin>: hGetLine: end of file
-}

{- Check that feeding `stdin` works repeatedly.

>>> getLine >>= print
<stdin>: hGetLine: end of file
-}
