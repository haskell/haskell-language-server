{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
module Development.IDE.Types.Shake
  ( Q (..),
    A (..),
    Value (..),
    ValueWithDiagnostics (..),
    Values,
    Key (..),
    BadDependency (..),
    ShakeValue(..),
    currentValue,
    isBadDependency,
  toShakeValue,encodeShakeValue,decodeShakeValue)
where

import Control.DeepSeq
import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Data.Dynamic
import Data.Hashable
import Data.HashMap.Strict
import Data.Vector (Vector)
import Data.Typeable
import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.Shake (RuleResult, ShakeException (shakeExceptionInner))
import Development.Shake.Classes
import GHC.Generics
import Language.LSP.Types
import Development.IDE.Core.PositionMapping

data Value v
    = Succeeded TextDocumentVersion v
    | Stale (Maybe PositionDelta) TextDocumentVersion v
    | Failed Bool -- True if we already tried the persistent rule
    deriving (Functor, Generic, Show)

instance NFData v => NFData (Value v)

-- | Convert a Value to a Maybe. This will only return `Just` for
-- up2date results not for stale values.
currentValue :: Value v -> Maybe v
currentValue (Succeeded _ v) = Just v
currentValue (Stale _ _ _) = Nothing
currentValue Failed{} = Nothing

data ValueWithDiagnostics
  = ValueWithDiagnostics !(Value Dynamic) !(Vector FileDiagnostic)

-- | The state of the all values and diagnostics
type Values = HashMap (NormalizedFilePath, Key) ValueWithDiagnostics

-- | Key type
data Key = forall k . (Typeable k, Hashable k, Eq k, Show k) => Key k

instance Show Key where
  show (Key k) = show k

instance Eq Key where
    Key k1 == Key k2 | Just k2' <- cast k2 = k1 == k2'
                     | otherwise = False

instance Hashable Key where
    hashWithSalt salt (Key key) = hashWithSalt salt (typeOf key, key)

-- | When we depend on something that reported an error, and we fail as a direct result, throw BadDependency
--   which short-circuits the rest of the action
newtype BadDependency = BadDependency String deriving Show
instance Exception BadDependency

isBadDependency :: SomeException -> Bool
isBadDependency x
    | Just (x :: ShakeException) <- fromException x = isBadDependency $ shakeExceptionInner x
    | Just (_ :: BadDependency) <- fromException x = True
    | otherwise = False

newtype Q k = Q (k, NormalizedFilePath)
    deriving newtype (Eq, Hashable, NFData)

instance Binary k => Binary (Q k) where
    put (Q (k, fp)) = put (k, fp)
    get = do
        (k, fp) <- get
        -- The `get` implementation of NormalizedFilePath
        -- does not handle empty file paths so we
        -- need to handle this ourselves here.
        pure (Q (k, toNormalizedFilePath' fp))

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
  ShakeStale r -> BS.cons 's' r

decodeShakeValue :: BS.ByteString -> ShakeValue
decodeShakeValue bs = case BS.uncons bs of
  Nothing -> ShakeNoCutoff
  Just (x, xs)
    | x == 'r' -> ShakeResult xs
    | x == 's' -> ShakeStale xs
    | otherwise -> error $ "Failed to parse shake value " <> show bs
