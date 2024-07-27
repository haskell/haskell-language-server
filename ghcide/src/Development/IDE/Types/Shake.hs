{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE TypeFamilies       #-}
module Development.IDE.Types.Shake
  ( Q (..),
    A (..),
    Value (..),
    ValueWithDiagnostics (..),
    Values,
    Key,
    BadDependency (..),
    ShakeValue(..),
    currentValue,
    isBadDependency,
  toShakeValue,encodeShakeValue,decodeShakeValue,toKey,toNoFileKey,fromKey,fromKeyType,WithHieDb,WithHieDbShield(..))
where

import           Control.DeepSeq
import           Control.Exception
import qualified Data.ByteString.Char8                as BS
import           Data.Dynamic
import           Data.Hashable
import           Data.Typeable                        (cast)
import           Data.Vector                          (Vector)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes       (FileVersion)
import           Development.IDE.Graph                (Key, RuleResult, newKey,
                                                       pattern Key)
import qualified Development.IDE.Graph                as Shake
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.Location
import           GHC.Generics
import           HieDb.Types                          (HieDb)
import qualified StmContainers.Map                    as STM
import           Type.Reflection                      (SomeTypeRep (SomeTypeRep),
                                                       pattern App, pattern Con,
                                                       typeOf, typeRep,
                                                       typeRepTyCon)
import           Unsafe.Coerce                        (unsafeCoerce)

-- | Intended to represent HieDb calls wrapped with (currently) retry
-- functionality
type WithHieDb = forall a. (HieDb -> IO a) -> IO a

-- used to smuggle RankNType WithHieDb through dbMVar
newtype WithHieDbShield = WithHieDbShield WithHieDb

data Value v
    = Succeeded (Maybe FileVersion) v
    | Stale (Maybe PositionDelta) (Maybe FileVersion) v
    | Failed Bool -- True if we already tried the persistent rule
    deriving (Functor, Generic, Show)

instance NFData v => NFData (Value v)

-- | Convert a Value to a Maybe. This will only return `Just` for
-- up2date results not for stale values.
currentValue :: Value v -> Maybe v
currentValue (Succeeded _ v) = Just v
currentValue (Stale _ _ _)   = Nothing
currentValue Failed{}        = Nothing

data ValueWithDiagnostics
  = ValueWithDiagnostics !(Value Dynamic) !(Vector FileDiagnostic)

-- | The state of the all values and diagnostics
type Values = STM.Map Key ValueWithDiagnostics

-- | When we depend on something that reported an error, and we fail as a direct result, throw BadDependency
--   which short-circuits the rest of the action
newtype BadDependency = BadDependency String deriving Show
instance Exception BadDependency

isBadDependency :: SomeException -> Bool
isBadDependency x
    | Just (_ :: BadDependency) <- fromException x = True
    | otherwise = False

toKey :: Shake.ShakeValue k => k -> NormalizedFilePath -> Key
toKey = (newKey.) . curry Q

fromKey :: Typeable k => Key -> Maybe (k, NormalizedFilePath)
fromKey (Key k)
  | Just (Q (k', f)) <- cast k = Just (k', f)
  | otherwise = Nothing

-- | fromKeyType (Q (k,f)) = (typeOf k, f)
fromKeyType :: Key -> Maybe (SomeTypeRep, NormalizedFilePath)
fromKeyType (Key k) = case typeOf k of
    App (Con tc) a | tc == typeRepTyCon (typeRep @Q)
        -> case unsafeCoerce k of
         Q (_ :: (), f) -> Just (SomeTypeRep a, f)
    _ -> Nothing

toNoFileKey :: (Show k, Typeable k, Eq k, Hashable k) => k -> Key
toNoFileKey k = newKey $ Q (k, emptyFilePath)

newtype Q k = Q (k, NormalizedFilePath)
    deriving newtype (Eq, Hashable, NFData)

instance Show k => Show (Q k) where
    show (Q (k, file)) = show k ++ "; " ++ fromNormalizedFilePath file

-- | Invariant: the 'v' must be in normal form (fully evaluated).
--   Otherwise we keep repeatedly 'rnf'ing values taken from the Shake database
newtype A v = A (Value v)
    deriving Show

instance NFData (A v) where rnf (A v) = v `seq` ()

-- In the Shake database we only store one type of key/result pairs,
-- namely Q (question) / A (answer).
type instance RuleResult (Q k) = A (RuleResult k)


toShakeValue :: (BS.ByteString -> ShakeValue) -> Maybe BS.ByteString -> ShakeValue
toShakeValue = maybe ShakeNoCutoff

data ShakeValue
  = -- | This is what we use when we get Nothing from
    -- a rule.
    ShakeNoCutoff
  | -- | This is used both for `Failed`
    -- as well as `Succeeded`.
    ShakeResult !BS.ByteString
  | ShakeStale !BS.ByteString
  deriving (Generic, Show)

instance NFData ShakeValue

encodeShakeValue :: ShakeValue -> BS.ByteString
encodeShakeValue = \case
  ShakeNoCutoff -> BS.empty
  ShakeResult r -> BS.cons 'r' r
  ShakeStale r  -> BS.cons 's' r

decodeShakeValue :: BS.ByteString -> ShakeValue
decodeShakeValue bs = case BS.uncons bs of
  Nothing -> ShakeNoCutoff
  Just (x, xs)
    | x == 'r' -> ShakeResult xs
    | x == 's' -> ShakeStale xs
    | otherwise -> error $ "Failed to parse shake value " <> show bs
