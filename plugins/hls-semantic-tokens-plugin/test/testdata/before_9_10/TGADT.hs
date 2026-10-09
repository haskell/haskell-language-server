{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module TGADT where

data Lam :: * -> * where
  Lift :: a                     -> Lam a        -- ^ lifted value
  Lam  :: (Lam a -> Lam b)      -> Lam (a -> b) -- ^ lambda abstraction
