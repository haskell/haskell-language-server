-- IO expressions are supported, stdout/stderr output is ignored
module TIO where

import Control.Concurrent (threadDelay)

{-
Does not capture stdout, returns value.
Has a delay in order to show progress reporting.

>>> threadDelay 2000000 >> print "ABC" >> return "XYZ"
"XYZ"
-}
