-- 1. Support IO expressions
--
-- 2. Capture and show stdout
module TIOStdout where

import           Control.Concurrent (threadDelay)

{- Capture stdout, returns value.

Has a delay in order to show progress reporting.

>>> threadDelay 2000000 >> print "ABC" >> return "XYZ"
"ABC"
"XYZ"
-}

{- Check that capturing `stdout` works repeatedly.

>>> print "ABC" >> return "XYZ"
"ABC"
"XYZ"
-}
