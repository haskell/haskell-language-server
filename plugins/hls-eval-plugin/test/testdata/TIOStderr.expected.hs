-- 1. Support IO expressions
--
-- 2. Capture and show stderr
module TIOError where

import           Control.Exception

{- We do not see the error value constructor.

>>> throwIO (TypeError "Doh")
Doh
-}

{- Do we capture `stderr` repeatedly?

>>> throwIO (TypeError "Doh")
Doh
-}
