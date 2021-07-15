{-# LANGUAGE GADTs #-}
module GoldenGADTAuto where
data CtxGADT a where
  MkCtxGADT :: (Show a, Eq a) => a -> CtxGADT a

ctxGADT :: CtxGADT ()
ctxGADT = MkCtxGADT ()
