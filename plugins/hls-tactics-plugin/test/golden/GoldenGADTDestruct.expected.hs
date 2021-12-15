{-# LANGUAGE GADTs #-}
module GoldenGADTDestruct where
data CtxGADT where
  MkCtxGADT :: (Show a, Eq a) => a -> CtxGADT

ctxGADT :: CtxGADT -> String
ctxGADT (MkCtxGADT a) = _w0
