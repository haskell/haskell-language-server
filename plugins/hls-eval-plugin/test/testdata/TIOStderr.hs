-- 1. Support IO expressions
--
-- 2. Capture and show stderr
module TIOStderr where

import           Control.Exception
import           System.IO         (hPutStrLn, stderr)

{- Capture stderr.

>>> hPutStrLn stderr "Doh"
-}

{- We do not see the error value constructor.

>>> throwIO (TypeError "Doh")
-}

{- Capture stderr after an exception.

>>> hPutStrLn stderr "Doh"
-}
