-- 1. Support IO expressions
--
-- 2. Capture and show stderr
module TIOError where

import           Control.Exception

{-
>>> throwIO (TypeError "Doh")
Doh
-}
