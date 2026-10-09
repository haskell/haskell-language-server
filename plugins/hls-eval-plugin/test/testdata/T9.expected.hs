{-# LANGUAGE DataKinds #-}
module T9 where
import Data.Proxy (Proxy(..))

type P = Proxy

-- >>> Proxy :: Proxy 3
-- Proxy
