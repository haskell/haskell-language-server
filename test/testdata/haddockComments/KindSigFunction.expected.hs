{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module KindSigFunction where

import GHC.TypeLits

f :: KnownSymbol k => (proxy :: k -> *) k -- ^ 
  -> String
f = symbolVal
