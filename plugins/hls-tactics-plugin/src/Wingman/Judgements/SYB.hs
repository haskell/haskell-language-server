{-# LANGUAGE RankNTypes #-}

-- | Custom SYB traversals
module Wingman.Judgements.SYB where

import Data.Generics
import Data.Foldable (foldl')
import Development.IDE.GHC.Compat


------------------------------------------------------------------------------
-- | Like 'everything', but only looks inside of the given 'SrcSpan'.
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


------------------------------------------------------------------------------
-- | Helper function for implementing 'everythingWithin'
--
-- NOTE(sandy): Subtly broken. In an ideal world, this function shuld return
-- @Just False@ for nodes of /any type/ which do not contain the span. But if
-- this functionality exists anywhere within the SYB machinery, I have yet to
-- find it.
genericIsSubspan
    :: SrcSpan
    -> GenericQ (Maybe Bool)
genericIsSubspan dst = mkQ Nothing $ \case
  (L span _ :: LHsExpr GhcTc) -> Just $ dst `isSubspanOf` span

