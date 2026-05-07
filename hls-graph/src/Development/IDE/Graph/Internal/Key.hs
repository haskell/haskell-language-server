{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE ViewPatterns       #-}

module Development.IDE.Graph.Internal.Key
    ( Key -- Opaque - don't expose constructor, use newKey to create
    , KeyValue (..)
    , pattern Key
    , pattern DirectKey
    , newKey
    , renderKey
    -- * KeyMap
    , KeyMap
    , mapKeyMap
    , insertKeyMap
    , lookupKeyMap
    , lookupDefaultKeyMap
    , fromListKeyMap
    , fromListWithKeyMap
    , toListKeyMap
    , elemsKeyMap
    , restrictKeysKeyMap
    -- * KeySet
    , KeySet
    , nullKeySet
    , insertKeySet
    , memberKeySet
    , toListKeySet
    , lengthKeySet
    , filterKeySet
    , singletonKeySet
    , fromListKeySet
    , deleteKeySet
    , differenceKeySet
    , unionKeySet
    , notMemberKeySet
    , newDirectKey
    ) where

--import Control.Monad.IO.Class ()
import           Control.Exception             (evaluate)
import           Data.Coerce
import           Data.Dynamic
import qualified Data.HashMap.Strict           as Map
import           Data.IntMap                   (IntMap)
import qualified Data.IntMap.Strict            as IM
import           Data.IntSet                   (IntSet)
import qualified Data.IntSet                   as IS
import           Data.IORef
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Typeable
import           Development.IDE.Graph.Classes
import           Prettyprinter
import           System.IO.Unsafe


newtype Key = UnsafeMkKey Int


pattern Key :: () => (Typeable a, Hashable a, Show a) => a -> Key
pattern Key a <- (lookupKeyValue -> (KeyValue a _))
pattern DirectKey :: Int -> Key
pattern DirectKey a <- (lookupKeyValue -> (DirectKeyValue a))
{-# COMPLETE Key, DirectKey #-}

instance Pretty Key where
  pretty = pretty . renderKey

data KeyValue = forall a . (Typeable a, Hashable a, Show a) =>
    KeyValue a Text |
    DirectKeyValue Int

instance Eq KeyValue where
    KeyValue a _ == KeyValue b _         = Just a == cast b
    DirectKeyValue a == DirectKeyValue b = a == b
    _ == _                               = False
instance Hashable KeyValue where

    hashWithSalt i (KeyValue x _)     = hashWithSalt i (typeOf x, x)
    hashWithSalt i (DirectKeyValue x) = hashWithSalt i (typeOf x, x)
instance Show KeyValue where
    show (KeyValue _ t)     = T.unpack t
    show (DirectKeyValue i) = "DirectKeyValue " ++ show i

data GlobalKeyValueMap = GlobalKeyValueMap !(Map.HashMap KeyValue Key) !(IntMap KeyValue) {-# UNPACK #-} !Int

keyMap :: IORef GlobalKeyValueMap
keyMap = unsafePerformIO $ newIORef (GlobalKeyValueMap Map.empty IM.empty 0)
{-# NOINLINE keyMap #-}

-- | Create a new key that is guaranteed not to collide with any other key.
-- This is useful for keys that are not based on user data, e.g., for
-- tracking temporary actions.
newDirectKey :: Int -> Key
newDirectKey i = UnsafeMkKey (- abs i)

newKey :: (Typeable a, Hashable a, Show a) => a -> Key
newKey k = unsafePerformIO $ do
  let !newKey = KeyValue k (T.pack (show k))
  atomicModifyIORef' keyMap $ \km@(GlobalKeyValueMap hm im n) ->
    let new_key = Map.lookup newKey hm
    in case new_key of
          Just v  -> (km, v)
          Nothing ->
            let !new_index = UnsafeMkKey n
            in (GlobalKeyValueMap (Map.insert newKey new_index hm) (IM.insert n newKey im) (n+1), new_index)
{-# NOINLINE newKey #-}

lookupKeyValue :: Key -> KeyValue
lookupKeyValue (UnsafeMkKey x) = unsafePerformIO $ do
  -- NOTE:
  -- The reason for this evaluate is that the x, if not forced yet, is a thunk
  -- that forces the atomicModifyIORef' in the creation of the new key. If it
  -- isn't forced *before* reading the keyMap, the keyMap will only obtain the new
  -- key (x) *after* the IntMap is already copied out of the keyMap reference,
  -- i.e. when it is forced for the lookup in the IntMap.
  k <- evaluate x
  GlobalKeyValueMap _ im _ <- readIORef keyMap
  case im IM.!? k of
    Just v  -> pure $! v
    Nothing -> pure $! DirectKeyValue k

{-# NOINLINE lookupKeyValue #-}

instance Eq Key where
  UnsafeMkKey a == UnsafeMkKey b = a == b
instance Hashable Key where
  hashWithSalt i (UnsafeMkKey x) = hashWithSalt i x
instance Show Key where
  show (Key x)       = show x
  show (DirectKey x) = "DirectKey " ++ show x

renderKey :: Key -> Text
renderKey (lookupKeyValue -> (KeyValue _ t)) = t
renderKey (lookupKeyValue -> (DirectKeyValue i)) = T.pack ("DirectKeyValue " ++ show i)

newtype KeySet = KeySet IntSet
  deriving newtype (Eq, Ord, Semigroup, Monoid, NFData, Hashable)

instance Pretty KeySet where
  pretty (KeySet is) = pretty (coerce (IS.toList is) :: [Key])

instance Show KeySet where
  showsPrec p (KeySet is)= showParen (p > 10) $
      showString "fromList " . shows ks
    where ks = coerce (IS.toList is) :: [Key]

insertKeySet :: Key -> KeySet -> KeySet
insertKeySet = coerce IS.insert

memberKeySet :: Key -> KeySet -> Bool
memberKeySet = coerce IS.member

notMemberKeySet :: Key -> KeySet -> Bool
notMemberKeySet = coerce IS.notMember

toListKeySet :: KeySet -> [Key]
toListKeySet = coerce IS.toList

nullKeySet :: KeySet -> Bool
nullKeySet = coerce IS.null

differenceKeySet :: KeySet -> KeySet -> KeySet
differenceKeySet = coerce IS.difference


unionKeySet :: KeySet -> KeySet -> KeySet
unionKeySet = coerce IS.union

deleteKeySet :: Key -> KeySet -> KeySet
deleteKeySet = coerce IS.delete

fromListKeySet :: [Key] -> KeySet
fromListKeySet = coerce IS.fromList

singletonKeySet :: Key -> KeySet
singletonKeySet = coerce IS.singleton

filterKeySet :: (Key -> Bool) -> KeySet -> KeySet
filterKeySet = coerce IS.filter

lengthKeySet :: KeySet -> Int
lengthKeySet = coerce IS.size

newtype KeyMap a = KeyMap (IntMap a)
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance Show a => Show (KeyMap a) where
  showsPrec p (KeyMap im)= showParen (p > 10) $
      showString "fromList " . shows ks
    where ks = coerce (IM.toList im) :: [(Key,a)]

mapKeyMap :: (a -> b) -> KeyMap a -> KeyMap b
mapKeyMap f (KeyMap m) = KeyMap (IM.map f m)

insertKeyMap :: Key -> a -> KeyMap a -> KeyMap a
insertKeyMap (UnsafeMkKey k) v (KeyMap m) = KeyMap (IM.insert k v m)

lookupKeyMap :: Key -> KeyMap a -> Maybe a
lookupKeyMap (UnsafeMkKey k) (KeyMap m) = IM.lookup k m

lookupDefaultKeyMap :: a -> Key -> KeyMap a -> a
lookupDefaultKeyMap a (UnsafeMkKey k) (KeyMap m) = IM.findWithDefault a k m

fromListKeyMap :: [(Key,a)] -> KeyMap a
fromListKeyMap xs = KeyMap (IM.fromList (coerce xs))

fromListWithKeyMap :: (a -> a -> a) -> [(Key,a)] -> KeyMap a
fromListWithKeyMap f xs = KeyMap (IM.fromListWith f (coerce xs))

toListKeyMap :: KeyMap a -> [(Key,a)]
toListKeyMap (KeyMap m) = coerce (IM.toList m)

elemsKeyMap :: KeyMap a -> [a]
elemsKeyMap (KeyMap m) = IM.elems m

restrictKeysKeyMap :: KeyMap a -> KeySet -> KeyMap a
restrictKeysKeyMap (KeyMap m) (KeySet s) = KeyMap (IM.restrictKeys m s)
