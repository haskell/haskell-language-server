{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE RankNTypes #-}
module Wingman.Range
  ( module Wingman.Range
  ) where

import           Control.DeepSeq (NFData)
import           Data.Aeson
import           Data.Coerce (coerce)
import           Data.Kind (Type)
import           Development.IDE hiding (rangeToRealSrcSpan)
import qualified Development.IDE.Core.PositionMapping as P
import qualified FastString as FS
import           SrcLoc
import Generics.SYB
import Data.Functor.Identity (Identity(Identity))



------------------------------------------------------------------------------
-- | Convert a DAML compiler Range to a GHC SrcSpan
-- TODO(sandy): this doesn't belong here
rangeToSrcSpan :: String -> Range -> SrcSpan
rangeToSrcSpan file range = RealSrcSpan $ rangeToRealSrcSpan file range


rangeToRealSrcSpan :: String -> Range -> RealSrcSpan
rangeToRealSrcSpan file (Range (Position startLn startCh) (Position endLn endCh)) =
  mkRealSrcSpan
    (mkRealSrcLoc (FS.fsLit file) (startLn + 1) (startCh + 1))
    (mkRealSrcLoc (FS.fsLit file) (endLn + 1) (endCh + 1))


data Age = Current | Stale Type

newtype Tracked (age :: Age) a  = Tracked
  { unTrack :: a
  }
  deriving stock Functor
  deriving newtype (Eq, Ord, Show, Read, ToJSON, FromJSON, NFData)
  deriving Applicative via Identity


newtype PositionMapping s = PositionMapping
  { getPositionMapping :: P.PositionMapping
  }


data TrackedStale a where
  TrackedStale
      :: Tracked (Stale s) a
      -> PositionMapping s
      -> TrackedStale a

instance Functor TrackedStale where
  fmap f (TrackedStale t pm) = TrackedStale (fmap f t) pm



cautiousToCurrent :: age ->  Tracked 'Current age
cautiousToCurrent = coerce


cautiousToStale :: age -> Tracked (Stale s) age
cautiousToStale = coerce

cautiousCopyAge :: Tracked age a -> b -> Tracked age b
cautiousCopyAge _ = coerce


fromCurrentRange
    :: PositionMapping s
    -> Tracked 'Current Range
    -> Maybe (Tracked (Stale s) Range)
fromCurrentRange = coerce P.fromCurrentRange


toCurrentRange
    :: PositionMapping s
    -> Tracked (Stale s) Range
    -> Maybe (Tracked 'Current Range)
toCurrentRange = coerce P.toCurrentRange

unsafeToCurrentRange :: PositionMapping s -> Range -> Maybe Range
unsafeToCurrentRange = coerce P.toCurrentRange


mapRangeOfRealSrcSpan :: Functor f => (Range -> f Range) -> RealSrcSpan -> f RealSrcSpan
mapRangeOfRealSrcSpan f rss
  = fmap (rangeToRealSrcSpan $ FS.unpackFS $ srcSpanFile rss)
  . f
  $ realSrcSpanToRange rss


mapPositionsToCurrent
    :: Data a
    => PositionMapping s
    -> Tracked (Stale s) a
    -> Maybe (Tracked Current a)
mapPositionsToCurrent (PositionMapping am) (Tracked t)
  = fmap Tracked
  $ everywhereM
    (        mkM (P.toCurrentRange am)
      `extM` (mapRangeOfRealSrcSpan (P.toCurrentRange am))
    ) t

