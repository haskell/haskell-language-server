-- | This module contains compatibility constructs to write type signatures across
--   multiple ghc-exactprint versions, accepting that anything more ambitious is
--   pretty much impossible with the GHC 9.2 redesign of ghc-exactprint
module Development.IDE.GHC.Compat.ExactPrint
    ( module ExactPrint
    , printA
    , transformA
    ) where

import           Language.Haskell.GHC.ExactPrint as ExactPrint

printA :: (ExactPrint ast) => ast -> String
printA ast = exactPrint ast

transformA
  :: Monad m => ast1 -> (ast1 -> TransformT m ast2) -> m ast2
transformA ast f = do
  (ast',_ ,_) <- runTransformFromT 0 (f ast)
  return ast'
