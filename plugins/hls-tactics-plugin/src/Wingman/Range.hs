{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Wingman.Range where

import           Control.Arrow
import           Control.Category (Category)
import qualified Control.Category as C
import           Control.DeepSeq (NFData)
import           Data.Aeson
import           Data.Coerce (coerce)
import           Data.Functor.Identity (Identity(Identity))
import           Data.Kind (Type)
import           Development.IDE hiding (rangeToRealSrcSpan, rangeToSrcSpan)
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


------------------------------------------------------------------------------
-- | A data kind for 'Tracked'.
data Age = Current | Stale Type


------------------------------------------------------------------------------
-- | Some value, tagged with its age. All 'Current' ages are considered to be
-- the same thing, but 'Stale' values are protected by an untouchable variable
-- to ensure they can't be unified.
newtype Tracked (age :: Age) a  = UnsafeTracked
  { unTrack :: a
  }
  deriving stock (Functor, Foldable, Traversable)
  deriving newtype (Eq, Ord, Show, Read, ToJSON, FromJSON, NFData)
  deriving (Applicative, Monad) via Identity


------------------------------------------------------------------------------
-- | Like 'P.PositionMapping', but with annotated ages for how 'Tracked' values
-- change. Use the 'Category' instance to compose 'PositionMapping's in order
-- to transform between values of different stale ages.
newtype PositionMapping (from :: Age) (to :: Age) = PositionMapping
  { getPositionMapping :: P.PositionMapping
  }

instance Category PositionMapping where
  id  = coerce P.zeroMapping
  (.) = coerce P.composeDelta


------------------------------------------------------------------------------
-- | Run a 'PositionMapping' backwards.
dual :: PositionMapping from to -> PositionMapping to from
dual (PositionMapping (P.PositionMapping (P.PositionDelta from to))) =
  PositionMapping $ P.PositionMapping $ P.PositionDelta to from


------------------------------------------------------------------------------
-- | A pair containing a @'Tracked' 'Stale'@ value, as well as
-- a 'PositionMapping' that will fast-forward values to the current age.
data TrackedStale a where
  TrackedStale
      :: Tracked (Stale s) a
      -> PositionMapping (Stale s) Current
      -> TrackedStale a

instance Functor TrackedStale where
  fmap f (TrackedStale t pm) = TrackedStale (fmap f t) pm


------------------------------------------------------------------------------
-- | A class for which 'Tracked' values can be run across a 'PositionMapping'
-- to change their ages.
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


------------------------------------------------------------------------------
-- | Helper function for deriving 'MapAge' for values in terms of other
-- instances.
invMapAge
    :: (c -> a -> b)
    -> (b -> (c, a))
    -> (Tracked from a -> Maybe (Tracked to a))
    -> Tracked from b
    -> Maybe (Tracked to b)
invMapAge to from f t =
  let (c, t') = unTrack $ fmap from t
   in fmap (fmap $ to c) $ f $ UnsafeTracked t'


cautiousToCurrent :: age ->  Tracked 'Current age
cautiousToCurrent = coerce


cautiousToStale :: age -> Tracked (Stale s) age
cautiousToStale = coerce

cautiousCopyAge :: Tracked age a -> b -> Tracked age b
cautiousCopyAge _ = coerce

