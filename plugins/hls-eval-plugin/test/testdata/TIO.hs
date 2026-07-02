-- 1. Support IO expressions
--
-- 2. Capture and show stdout
module TIO where

import           Control.Concurrent (threadDelay)

{- Capture stdout, returns value.

Small delay only: a large one widens the stdout capture window so the test
reporter's output for concurrent tests races into it under parallel runs.

>>> threadDelay 1000 >> print "ABC" >> return "XYZ"
-}
