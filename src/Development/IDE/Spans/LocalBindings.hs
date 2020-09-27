{-# LANGUAGE DerivingStrategies         #-}

module Development.IDE.Spans.LocalBindings
  ( Bindings
  , getLocalScope
  , getFuzzyScope
  , bindings
  ) where

import           Control.DeepSeq
import           Data.IntervalMap.FingerTree (IntervalMap, Interval (..))
import qualified Data.IntervalMap.FingerTree as IM
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import           Development.IDE.GHC.Compat (RefMap, identType, identInfo, getScopeFromContext, Scope(..), Name, Type)
import           Development.IDE.Types.Location
import           Development.IDE.GHC.Error
import           SrcLoc
import           NameEnv

------------------------------------------------------------------------------
-- | Turn a 'RealSrcSpan' into an 'Interval'.
realSrcSpanToInterval :: RealSrcSpan -> Interval Position
realSrcSpanToInterval rss =
  Interval
    (realSrcLocToPosition $ realSrcSpanStart rss)
    (realSrcLocToPosition $ realSrcSpanEnd   rss)

------------------------------------------------------------------------------
-- | Compute which identifiers are in scope at every point in the AST. Use
-- 'getLocalScope' to find the results.
bindings :: RefMap -> Bindings
bindings refmap = Bindings $ L.foldl' (flip (uncurry IM.insert)) mempty  $ do
  (ident, refs)      <- M.toList refmap
  Right name         <- pure ident
  (_, ident_details) <- refs
  let ty = identType ident_details
  info        <- S.toList $ identInfo ident_details
  Just scopes <- pure $ getScopeFromContext info
  scope <- scopes >>= \case
    LocalScope scope -> pure $ realSrcSpanToInterval scope
    _ -> []
  pure ( scope
       , unitNameEnv name (name,ty)
       )

------------------------------------------------------------------------------
-- | The available bindings at every point in a Haskell tree.
newtype Bindings = Bindings
  { getBindings :: IntervalMap Position (NameEnv (Name, Maybe Type))
  } deriving newtype (Semigroup, Monoid)
instance NFData Bindings where
    rnf = rwhnf
instance Show Bindings where
    show _ = "<bindings>"


------------------------------------------------------------------------------
-- | Given a 'Bindings' get every identifier in scope at the given
-- 'RealSrcSpan',
getLocalScope :: Bindings -> RealSrcSpan -> [(Name, Maybe Type)]
getLocalScope bs rss
  = nameEnvElts
  $ foldMap snd
  $ IM.dominators (realSrcSpanToInterval rss)
  $ getBindings bs

-- | Lookup all names in scope in any span that intersects the interval
-- defined by the two positions.
-- This is meant for use with the fuzzy `PositionRange` returned by `PositionMapping`
getFuzzyScope :: Bindings -> Position -> Position -> [(Name, Maybe Type)]
getFuzzyScope bs a b
  = nameEnvElts
  $ foldMap snd
  $ IM.intersections (Interval a b)
  $ getBindings bs
