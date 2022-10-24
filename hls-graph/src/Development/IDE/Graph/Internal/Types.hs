{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ViewPatterns               #-}

module Development.IDE.Graph.Internal.Types where

import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                    (FromJSON, ToJSON)
import           Data.Bifunctor                (second)
import qualified Data.ByteString               as BS
import           Data.Coerce
import           Data.Dynamic
import qualified Data.HashMap.Strict           as Map
import qualified Data.IntMap.Strict            as IM
import           Data.IntMap                   (IntMap)
import qualified Data.IntSet                   as IS
import           Data.IntSet                   (IntSet)
import qualified Data.Text                     as T
import           Data.Text                     (Text)
import           Data.IORef
import           Data.List                     (intercalate)
import           Data.Maybe
import           Data.Typeable
import           Development.IDE.Graph.Classes
import           GHC.Conc                      (TVar, atomically)
import           GHC.Generics                  (Generic)
import qualified ListT
import qualified StmContainers.Map             as SMap
import           StmContainers.Map             (Map)
import           System.Time.Extra             (Seconds)
import           System.IO.Unsafe
import           UnliftIO                      (MonadUnliftIO)


unwrapDynamic :: forall a . Typeable a => Dynamic -> a
unwrapDynamic x = fromMaybe (error msg) $ fromDynamic x
    where msg = "unwrapDynamic failed: Expected " ++ show (typeRep (Proxy :: Proxy a)) ++
                ", but got " ++ show (dynTypeRep x)

---------------------------------------------------------------------
-- RULES

type TheRules = Map.HashMap TypeRep Dynamic

newtype Rules a = Rules (ReaderT SRules IO a)
    deriving newtype (Monad, Applicative, Functor, MonadIO, MonadFail)

data SRules = SRules {
    rulesExtra   :: !Dynamic,
    rulesActions :: !(IORef [Action ()]),
    rulesMap     :: !(IORef TheRules)
    }


---------------------------------------------------------------------
-- ACTIONS

newtype Action a = Action {fromAction :: ReaderT SAction IO a}
    deriving newtype (Monad, Applicative, Functor, MonadIO, MonadFail, MonadThrow, MonadCatch, MonadMask, MonadUnliftIO)

data SAction = SAction {
    actionDatabase :: !Database,
    actionDeps     :: !(IORef ResultDeps),
    actionStack    :: !Stack
    }

getDatabase :: Action Database
getDatabase = Action $ asks actionDatabase

---------------------------------------------------------------------
-- DATABASE

data ShakeDatabase = ShakeDatabase !Int [Action ()] Database

newtype Step = Step Int
    deriving newtype (Eq,Ord,Hashable)

---------------------------------------------------------------------
-- Keys

data KeyValue = forall a . (Eq a, Typeable a, Hashable a, Show a) => KeyValue a Text

newtype Key = UnsafeMkKey Int

pattern Key a <- (lookupKeyValue -> KeyValue a _)

data GlobalKeyValueMap = GlobalKeyValueMap !(Map.HashMap KeyValue Key) !(IntMap KeyValue) {-# UNPACK #-} !Int

keyMap :: IORef GlobalKeyValueMap
keyMap = unsafePerformIO $ newIORef (GlobalKeyValueMap Map.empty IM.empty 0)

{-# NOINLINE keyMap #-}

newKey :: (Eq a, Typeable a, Hashable a, Show a) => a -> Key
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
  GlobalKeyValueMap _ im _ <- readIORef keyMap
  pure $! im IM.! x

{-# NOINLINE lookupKeyValue #-}

instance Eq Key where
  UnsafeMkKey a == UnsafeMkKey b = a == b
instance Hashable Key where
  hashWithSalt i (UnsafeMkKey x) = hashWithSalt i x
instance Show Key where
  show (Key x) = show x

instance Eq KeyValue where
    KeyValue a _ == KeyValue b _ = Just a == cast b
instance Hashable KeyValue where
    hashWithSalt i (KeyValue x _) = hashWithSalt i (typeOf x, x)
instance Show KeyValue where
    show (KeyValue x t) = T.unpack t

renderKey :: Key -> Text
renderKey (lookupKeyValue -> KeyValue _ t) = t

newtype KeySet = KeySet IntSet
  deriving newtype (Eq, Ord, Semigroup, Monoid)

instance Show KeySet where
  showsPrec p (KeySet is)= showParen (p > 10) $
      showString "fromList " . shows ks
    where ks = coerce (IS.toList is) :: [Key]

insertKeySet :: Key -> KeySet -> KeySet
insertKeySet = coerce IS.insert

memberKeySet :: Key -> KeySet -> Bool
memberKeySet = coerce IS.member

toListKeySet :: KeySet -> [Key]
toListKeySet = coerce IS.toList

nullKeySet :: KeySet -> Bool
nullKeySet = coerce IS.null

differenceKeySet :: KeySet -> KeySet -> KeySet
differenceKeySet = coerce IS.difference

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


newtype Value = Value Dynamic

data KeyDetails = KeyDetails {
    keyStatus      :: !Status,
    keyReverseDeps :: !KeySet
    }

onKeyReverseDeps :: (KeySet -> KeySet) -> KeyDetails -> KeyDetails
onKeyReverseDeps f it@KeyDetails{..} =
    it{keyReverseDeps = f keyReverseDeps}

data Database = Database {
    databaseExtra  :: Dynamic,
    databaseRules  :: TheRules,
    databaseStep   :: !(TVar Step),
    databaseValues :: !(Map Key KeyDetails)
    }

getDatabaseValues :: Database -> IO [(Key, Status)]
getDatabaseValues = atomically
                  . (fmap.fmap) (second keyStatus)
                  . ListT.toList
                  . SMap.listT
                  . databaseValues

data Status
    = Clean !Result
    | Dirty (Maybe Result)
    | Running {
        runningStep   :: !Step,
        runningWait   :: !(IO ()),
        runningResult :: Result,     -- LAZY
        runningPrev   :: !(Maybe Result)
        }

viewDirty :: Step -> Status -> Status
viewDirty currentStep (Running s _ _ re) | currentStep /= s = Dirty re
viewDirty _ other = other

getResult :: Status -> Maybe Result
getResult (Clean re)           = Just re
getResult (Dirty m_re)         = m_re
getResult (Running _ _ _ m_re) = m_re -- watch out: this returns the previous result

data Result = Result {
    resultValue     :: !Value,
    resultBuilt     :: !Step, -- ^ the step when it was last recomputed
    resultChanged   :: !Step, -- ^ the step when it last changed
    resultVisited   :: !Step, -- ^ the step when it was last looked up
    resultDeps      :: !ResultDeps,
    resultExecution :: !Seconds, -- ^ How long it took, last time it ran
    resultData      :: !BS.ByteString
    }

data ResultDeps = UnknownDeps | AlwaysRerunDeps !KeySet | ResultDeps !KeySet
  deriving (Eq, Show)

getResultDepsDefault :: KeySet -> ResultDeps -> KeySet
getResultDepsDefault _ (ResultDeps ids)      = ids
getResultDepsDefault _ (AlwaysRerunDeps ids) = ids
getResultDepsDefault def UnknownDeps         = def

mapResultDeps :: (KeySet -> KeySet) -> ResultDeps -> ResultDeps
mapResultDeps f (ResultDeps ids)      = ResultDeps $ f ids
mapResultDeps f (AlwaysRerunDeps ids) = AlwaysRerunDeps $ f ids
mapResultDeps _ UnknownDeps           = UnknownDeps

instance Semigroup ResultDeps where
    UnknownDeps <> x = x
    x <> UnknownDeps = x
    AlwaysRerunDeps ids <> x = AlwaysRerunDeps (ids <> getResultDepsDefault mempty x)
    x <> AlwaysRerunDeps ids = AlwaysRerunDeps (getResultDepsDefault mempty x <> ids)
    ResultDeps ids <> ResultDeps ids' = ResultDeps (ids <> ids')

instance Monoid ResultDeps where
    mempty = UnknownDeps

---------------------------------------------------------------------
-- Running builds

-- | What mode a rule is running in, passed as an argument to 'BuiltinRun'.
data RunMode
    = RunDependenciesSame -- ^ My dependencies have not changed.
    | RunDependenciesChanged -- ^ At least one of my dependencies from last time have changed, or I have no recorded dependencies.
      deriving (Eq,Show)

instance NFData RunMode where rnf x = x `seq` ()

-- | How the output of a rule has changed.
data RunChanged
    = ChangedNothing -- ^ Nothing has changed.
    | ChangedStore -- ^ The stored value has changed, but in a way that should be considered identical (used rarely).
    | ChangedRecomputeSame -- ^ I recomputed the value and it was the same.
    | ChangedRecomputeDiff -- ^ I recomputed the value and it was different.
      deriving (Eq,Show,Generic)
      deriving anyclass (FromJSON, ToJSON)

instance NFData RunChanged where rnf x = x `seq` ()

-- | The result of 'BuiltinRun'.
data RunResult value = RunResult
    {runChanged :: RunChanged
        -- ^ How has the 'RunResult' changed from what happened last time.
    ,runStore   :: BS.ByteString
        -- ^ The value to store in the Shake database.
    ,runValue   :: value
        -- ^ The value to return from 'Development.Shake.Rule.apply'.
    } deriving Functor

instance NFData value => NFData (RunResult value) where
    rnf (RunResult x1 x2 x3) = rnf x1 `seq` x2 `seq` rnf x3

---------------------------------------------------------------------
-- EXCEPTIONS

data GraphException = forall e. Exception e => GraphException {
    target :: String, -- ^ The key that was being built
    stack  :: [String], -- ^ The stack of keys that led to this exception
    inner  :: e -- ^ The underlying exception
}
  deriving (Typeable, Exception)

instance Show GraphException where
    show GraphException{..} = unlines $
        ["GraphException: " ++ target] ++
        stack ++
        ["Inner exception: " ++ show inner]

fromGraphException :: Typeable b => SomeException -> Maybe b
fromGraphException x = do
    GraphException _ _ e <- fromException x
    cast e

---------------------------------------------------------------------
-- CALL STACK

data Stack = Stack [Key] !KeySet

instance Show Stack where
    show (Stack kk _) = "Stack: " <> intercalate " -> " (map show kk)

newtype StackException = StackException Stack
  deriving (Typeable, Show)

instance Exception StackException where
    fromException = fromGraphException
    toException this@(StackException (Stack stack _)) = toException $
        GraphException (show$ last stack) (map show stack) this

addStack :: Key -> Stack -> Either StackException Stack
addStack k (Stack ks is)
    | k `memberKeySet` is = Left $ StackException stack2
    | otherwise = Right stack2
    where stack2 = Stack (k:ks) (insertKeySet k is)

memberStack :: Key -> Stack -> Bool
memberStack k (Stack _ ks) = k `memberKeySet` ks

emptyStack :: Stack
emptyStack = Stack [] mempty
---------------------------------------------------------------------
-- INSTANCES

instance Semigroup a => Semigroup (Rules a) where
    a <> b = liftA2 (<>) a b

instance Monoid a => Monoid (Rules a) where
    mempty = pure mempty
