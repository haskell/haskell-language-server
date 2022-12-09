{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module GoldenGADTDestruct where
data E a b where
  E :: forall a b. (b ~ a, Ord a) => b -> E a [a]

ctxGADT :: E a b -> String
ctxGADT (E b) = _w0
