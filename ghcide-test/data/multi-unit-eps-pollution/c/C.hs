{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall #-}
module C where
import A

-- Omit top-level signature so we have a warning we can check against
cFoo = myMethod @AType
