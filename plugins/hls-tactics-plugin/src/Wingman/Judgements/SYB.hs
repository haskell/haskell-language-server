{-# LANGUAGE RankNTypes #-}

-- | Custom SYB traversals
module Wingman.Judgements.SYB where

import Data.Generics
import Data.Foldable (foldl')
import Development.IDE.GHC.Compat


everythingWithin
    :: forall r
     . Monoid r
    => SrcSpan
    -> GenericQ r
    -> GenericQ r
everythingWithin dst f = go
  where
    go :: GenericQ r
    go x =
      case genericIsSubspan dst x of
        Just False -> mempty
        _ -> foldl' (<>) (f x) (gmapQ go x)


genericIsSubspan
    :: SrcSpan
    -> GenericQ (Maybe Bool)
genericIsSubspan dst = mkQ Nothing $ \case
  (L span _ :: LHsExpr GhcTc) -> Just $ dst `isSubspanOf` span

