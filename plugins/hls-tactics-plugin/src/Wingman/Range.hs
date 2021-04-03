{-# LANGUAGE KindSignatures #-}

{-# LANGUAGE RankNTypes #-}
module Wingman.Range
  ( module Wingman.Range
  ) where

import           Control.Arrow
import           Control.Category (Category)
import qualified Control.Category as C
import           Control.DeepSeq (NFData)
import           Data.Aeson
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity(Identity))
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
  deriving stock (Functor, Foldable, Traversable)
  deriving newtype (Eq, Ord, Show, Read, ToJSON, FromJSON, NFData)
  deriving Applicative via Identity


newtype PositionMapping (from :: Age) (to :: Age) = PositionMapping
  { getPositionMapping :: P.PositionMapping
  }

dual :: PositionMapping from to -> PositionMapping to from
dual (PositionMapping (P.PositionMapping (P.PositionDelta from to))) =
  PositionMapping $ P.PositionMapping $ P.PositionDelta to from

instance Category PositionMapping where
  id  = coerce P.zeroMapping
  (.) = coerce P.composeDelta


data TrackedStale a where
  TrackedStale
      :: Tracked (Stale s) a
      -> PositionMapping (Stale s) Current
      -> TrackedStale a

instance Functor TrackedStale where
  fmap f (TrackedStale t pm) = TrackedStale (fmap f t) pm

class MapAge a where
  {-# MINIMAL mapAgeFrom | mapAgeTo #-}
  mapAgeFrom :: PositionMapping from to -> Tracked to   a -> Maybe (Tracked from a)
  mapAgeFrom = mapAgeTo . dual

  mapAgeTo   :: PositionMapping from to -> Tracked from a -> Maybe (Tracked to   a)
  mapAgeTo = mapAgeFrom . dual


instance MapAge Range where
  mapAgeFrom = coerce P.fromCurrentRange
  mapAgeTo   = coerce P.toCurrentRange


instance MapAge RealSrcSpan where
  mapAgeFrom =
    invMapAge (\fs -> rangeToRealSrcSpan (FS.unpackFS fs))
              (srcSpanFile &&& realSrcSpanToRange)
      .  mapAgeFrom

invMapAge
    :: (c -> a -> b)
    -> (b -> (c, a))
    -> (Tracked from a -> Maybe (Tracked to a))
    -> Tracked from b
    -> Maybe (Tracked to b)
invMapAge to from f t =
  let (c, t') = unTrack $ fmap from t
   in fmap (fmap $ to c) $ f $ Tracked t'


cautiousToCurrent :: age ->  Tracked 'Current age
cautiousToCurrent = coerce


cautiousToStale :: age -> Tracked (Stale s) age
cautiousToStale = coerce

cautiousCopyAge :: Tracked age a -> b -> Tracked age b
cautiousCopyAge _ = coerce


mapRangeOfRealSrcSpan :: (Range -> Maybe Range) -> RealSrcSpan -> Maybe RealSrcSpan
mapRangeOfRealSrcSpan f rss
  = fmap (rangeToRealSrcSpan $ FS.unpackFS $ srcSpanFile rss)
  . f
  $ realSrcSpanToRange rss

