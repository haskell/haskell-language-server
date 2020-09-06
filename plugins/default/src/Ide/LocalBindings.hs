{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Ide.LocalBindings
  ( Bindings
  , getLocalScope
  , mostSpecificSpan
  , holify
  , bindings
  ) where

import           Data.Function
import           Data.Generics
import           Data.IntervalMap.FingerTree (IntervalMap, Interval (..))
import qualified Data.IntervalMap.FingerTree as IM
import           Data.List
import qualified Data.Map as M
import           Data.Maybe
import           Data.Ord
import           Data.Set (Set)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat (GhcTc, RefMap, identType, identInfo, noExt, getScopeFromContext, Scope(..))
import           HsExpr
import           Id
import           OccName
import           SrcLoc

------------------------------------------------------------------------------
-- | Turn a 'RealSrcSpan' into an 'Interval'.
realSrcSpanToInterval :: RealSrcSpan -> Interval RealSrcLoc
realSrcSpanToInterval rss =
  Interval
    (realSrcSpanStart rss)
    (realSrcSpanEnd rss)


------------------------------------------------------------------------------
-- | Compute which identifiers are in scpoe at every point in the AST. Use
-- 'getLocalScope' to find the results.
bindings :: RefMap -> Bindings
bindings refmap = Bindings $ foldr (uncurry IM.insert) mempty  $ do
  (ident, refs)      <- M.toList refmap
  Right name         <- pure ident
  (ref_span, ident_details) <- refs
  Just ty     <- pure $ identType ident_details
  info        <- S.toList $ identInfo ident_details
  Just scopes <- pure $ getScopeFromContext info
  scope <- scopes >>= \case
    ModuleScope -> pure $
      let file = srcSpanFile ref_span
       in Interval
            (mkRealSrcLoc file minBound minBound)
            (mkRealSrcLoc file maxBound maxBound)
    LocalScope scope -> pure $ realSrcSpanToInterval scope
    NoScope -> []
  pure ( scope
       , S.singleton $ mkVanillaGlobal name ty
       )

------------------------------------------------------------------------------
-- | The available bindings at every point in a Haskell tree.
newtype Bindings = Bindings
  { getBindings :: IntervalMap RealSrcLoc (Set Id)
  } deriving newtype (Eq, Ord, Semigroup, Monoid)


------------------------------------------------------------------------------
-- | Given a 'Bindings' get every identifier in scope at the given
-- 'RealSrcSpan',
getLocalScope :: Bindings -> RealSrcSpan -> Set Id
getLocalScope bs rss
  = foldMap snd
  $ IM.dominators (realSrcSpanToInterval rss)
  $ getBindings bs


------------------------------------------------------------------------------
-- | How many lines and columns does a SrcSpan span?
srcSpanSize :: SrcSpan -> (Int, Int)
srcSpanSize (UnhelpfulSpan _) = maxBound
srcSpanSize (RealSrcSpan span) =
  ( srcSpanEndLine span - srcSpanStartLine span
  , srcSpanEndCol span - srcSpanStartCol span
  )


------------------------------------------------------------------------------
-- | Given a SrcSpan, find the smallest LHsExpr that entirely contains that
-- span. Useful for determining what node in the tree your cursor is hovering over.
mostSpecificSpan :: (Data a, Typeable pass) => SrcSpan -> a -> Maybe (LHsExpr pass)
mostSpecificSpan span z
  = listToMaybe
  $ sortBy (comparing srcSpanSize `on` getLoc)
  $ everything (<>) (mkQ mempty $ \case
      l@(L span' _) | span `isSubspanOf` span' -> [l]
      _                                        -> [])
  $ z


------------------------------------------------------------------------------
-- | Convert an HsVar back into an HsUnboundVar if it isn't actually in scope.
-- TODO(sandy): this will throw away the type >:(
holify :: Bindings -> LHsExpr GhcTc -> LHsExpr GhcTc
holify bs v@(L s@(RealSrcSpan span) (HsVar _ (L _ var))) =
  let occ = occName var
      binds = getLocalScope bs span
   in -- Make sure the binding is not in scope and that it begins with an
      -- underscore
      case not (S.member var binds) && take 1 (occNameString occ) == "_" of
        True  -> L s $ HsUnboundVar noExt $ TrueExprHole occ
        False -> v
holify _ v = v

