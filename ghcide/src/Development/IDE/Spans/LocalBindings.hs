{-# LANGUAGE DerivingStrategies #-}

module Development.IDE.Spans.LocalBindings
  ( Bindings
  , getLocalScope
  , getFuzzyScope
  , getDefiningBindings
  , getFuzzyDefiningBindings
  , bindings
  ) where

import           Control.DeepSeq
import           Control.Monad
import           Data.Bifunctor
import           Data.IntervalMap.FingerTree (IntervalMap, Interval (..))
import qualified Data.IntervalMap.FingerTree as IM
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import           Development.IDE.GHC.Compat (RefMap, identType, identInfo, getScopeFromContext, getBindSiteFromContext, Scope(..), Name, Type)
import           Development.IDE.GHC.Error
import           Development.IDE.Types.Location
import           NameEnv
import           SrcLoc

------------------------------------------------------------------------------
-- | Turn a 'RealSrcSpan' into an 'Interval'.
realSrcSpanToInterval :: RealSrcSpan -> Interval Position
realSrcSpanToInterval rss =
  Interval
    (realSrcLocToPosition $ realSrcSpanStart rss)
    (realSrcLocToPosition $ realSrcSpanEnd   rss)

bindings :: RefMap -> Bindings
bindings = uncurry Bindings . localBindings

------------------------------------------------------------------------------
-- | Compute which identifiers are in scope at every point in the AST. Use
-- 'getLocalScope' to find the results.
localBindings
    :: RefMap
    -> ( IntervalMap Position (NameEnv (Name, Maybe Type))
       , IntervalMap Position (NameEnv (Name, Maybe Type))
       )
localBindings refmap = bimap mk mk $ unzip $ do
  (ident, refs)      <- M.toList refmap
  Right name         <- pure ident
  (_, ident_details) <- refs
  let ty = identType ident_details
  info <- S.toList $ identInfo ident_details
  pure
    ( do
        Just scopes <- pure $ getScopeFromContext info
        scope <- scopes >>= \case
          LocalScope scope -> pure $ realSrcSpanToInterval scope
          _ -> []
        pure ( scope
            , unitNameEnv name (name,ty)
            )
    , do
        Just scope <- pure $ getBindSiteFromContext info
        pure ( realSrcSpanToInterval scope
            , unitNameEnv name (name,ty)
            )
    )
  where
    mk = L.foldl' (flip (uncurry IM.insert)) mempty . join

------------------------------------------------------------------------------
-- | The available bindings at every point in a Haskell tree.
data Bindings = Bindings
  { getLocalBindings
        :: IntervalMap Position (NameEnv (Name, Maybe Type))
  , getBindingSites
        :: IntervalMap Position (NameEnv (Name, Maybe Type))
  }

instance Semigroup Bindings where
  Bindings a1 b1 <> Bindings a2 b2
    = Bindings (a1 <> a2) (b1 <> b2)

instance Monoid Bindings where
  mempty = Bindings mempty mempty

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
  $ getLocalBindings bs

------------------------------------------------------------------------------
-- | Given a 'Bindings', get every binding currently active at a given
-- 'RealSrcSpan',
getDefiningBindings :: Bindings -> RealSrcSpan -> [(Name, Maybe Type)]
getDefiningBindings bs rss
  = nameEnvElts
  $ foldMap snd
  $ IM.dominators (realSrcSpanToInterval rss)
  $ getBindingSites bs


-- | Lookup all names in scope in any span that intersects the interval
-- defined by the two positions.
-- This is meant for use with the fuzzy `PositionRange` returned by `PositionMapping`
getFuzzyScope :: Bindings -> Position -> Position -> [(Name, Maybe Type)]
getFuzzyScope bs a b
  = nameEnvElts
  $ foldMap snd
  $ IM.intersections (Interval a b)
  $ getLocalBindings bs

------------------------------------------------------------------------------
-- | Given a 'Bindings', get every binding that intersects the interval defined
-- by the two positions.
-- This is meant for use with the fuzzy `PositionRange` returned by
-- `PositionMapping`
getFuzzyDefiningBindings :: Bindings -> Position -> Position -> [(Name, Maybe Type)]
getFuzzyDefiningBindings bs a b
  = nameEnvElts
  $ foldMap snd
  $ IM.intersections (Interval a b)
  $ getBindingSites bs

