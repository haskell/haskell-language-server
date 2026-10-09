{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module TTypeAppExp where
import Data.Proxy

f :: Proxy Int
f = $([|Proxy @Int|])
