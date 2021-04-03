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


newtype PositionMapping s = PositionMapping
  { getPositionMapping :: P.PositionMapping
  }


data TrackedStale a where
  TrackedStale
      :: Tracked (Stale s) a
      -> PositionMapping s
      -> TrackedStale a


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

