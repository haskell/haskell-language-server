module StaleFunction where

f :: a 
  -> b -- ^ ...
  -> c -> c
f _ _ c = c
