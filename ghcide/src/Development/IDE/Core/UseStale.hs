{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Development.IDE.Core.UseStale
  ( Age(..)
  , Tracked
  , unTrack
  , PositionMap
  , TrackedStale (..)
  , untrackedStaleValue
  , unsafeMkStale
  , unsafeMkCurrent
  , unsafeCopyAge
  , MapAge (..)
  , dualPositionMap
  , useWithStale
  , useWithStale_
  ) where

import           Control.Arrow
import           Control.Category                     (Category)
import qualified Control.Category                     as C
import           Control.DeepSeq                      (NFData)
import           Data.Aeson
import           Data.Coerce                          (coerce)
import           Data.Functor                         ((<&>))
import           Data.Functor.Identity                (Identity (Identity))
import           Data.Kind                            (Type)
import           Data.String                          (fromString)
import           Development.IDE.GHC.Compat           (RealSrcSpan,
                                                       srcSpanFile)
import           Development.IDE.GHC.Compat.Util      (unpackFS)
import           Development.IDE                      (Action, IdeRule,
                                                       NormalizedFilePath,
                                                       Range,
                                                       rangeToRealSrcSpan,
                                                       realSrcSpanToRange)
import qualified Development.IDE.Core.PositionMapping as P
import qualified Development.IDE.Core.Shake           as IDE


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
newtype PositionMap (from :: Age) (to :: Age) = PositionMap
  { _getPositionMapping :: P.PositionMapping
  }

instance Category PositionMap where
  id  = coerce P.zeroMapping
  (.) = coerce P.composeDelta


------------------------------------------------------------------------------
-- | Get a 'PositionMap' that runs in the opposite direction.
dualPositionMap :: PositionMap from to -> PositionMap to from
dualPositionMap (PositionMap (P.PositionMapping (P.PositionDelta from to))) =
  PositionMap $ P.PositionMapping $ P.PositionDelta to from


------------------------------------------------------------------------------
-- | A pair containing a @'Tracked' 'Stale'@ value, as well as
-- a 'PositionMapping' that will fast-forward values to the current age.
data TrackedStale a where
  TrackedStale
      :: Tracked (Stale s) a
      -> PositionMap (Stale s) Current
      -> TrackedStale a

instance Functor TrackedStale where
  fmap f (TrackedStale t pm) = TrackedStale (fmap f t) pm


untrackedStaleValue :: TrackedStale a -> a
untrackedStaleValue (TrackedStale ta _) = coerce ta


------------------------------------------------------------------------------
-- | A class for which 'Tracked' values can be run across a 'PositionMapping'
-- to change their ages.
class MapAge a where
  {-# MINIMAL mapAgeFrom | mapAgeTo #-}
  mapAgeFrom :: PositionMap from to -> Tracked to   a -> Maybe (Tracked from a)
  mapAgeFrom = mapAgeTo . dualPositionMap

  mapAgeTo   :: PositionMap from to -> Tracked from a -> Maybe (Tracked to   a)
  mapAgeTo = mapAgeFrom . dualPositionMap


instance MapAge Range where
  mapAgeFrom = coerce P.fromCurrentRange
  mapAgeTo   = coerce P.toCurrentRange


instance MapAge RealSrcSpan where
  mapAgeFrom =
    invMapAge (\fs -> rangeToRealSrcSpan (fromString $ unpackFS fs))
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


unsafeMkCurrent :: age ->  Tracked 'Current age
unsafeMkCurrent = coerce


unsafeMkStale :: age -> Tracked (Stale s) age
unsafeMkStale = coerce


unsafeCopyAge :: Tracked age a -> b -> Tracked age b
unsafeCopyAge _ = coerce


-- | Request a Rule result, it not available return the last computed result, if any, which may be stale
useWithStale :: IdeRule k v
    => k -> NormalizedFilePath -> Action (Maybe (TrackedStale v))
useWithStale key file = do
  x <- IDE.useWithStale key file
  pure $ x <&> \(v, pm) ->
    TrackedStale (coerce v) (coerce pm)

-- | Request a Rule result, it not available return the last computed result which may be stale.
--   Errors out if none available.
useWithStale_ :: IdeRule k v
    => k -> NormalizedFilePath -> Action (TrackedStale v)
useWithStale_ key file = do
  (v, pm) <- IDE.useWithStale_ key file
  pure $ TrackedStale (coerce v) (coerce pm)

