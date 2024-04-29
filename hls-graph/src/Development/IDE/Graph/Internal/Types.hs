{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}

module Development.IDE.Graph.Internal.Types where

import           Control.Concurrent.STM             (STM)
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Bifunctor                     (second)
import qualified Data.ByteString                    as BS
import           Data.Dynamic
import           Data.Foldable                      (fold)
import qualified Data.HashMap.Strict                as Map
import           Data.IORef
import           Data.List                          (intercalate)
import           Data.Maybe
import           Data.Typeable
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Key
import           GHC.Conc                           (TVar, atomically)
import           GHC.Generics                       (Generic)
import qualified ListT
import qualified StmContainers.Map                  as SMap
import           StmContainers.Map                  (Map)
import           System.Time.Extra                  (Seconds)
import           UnliftIO                           (MonadUnliftIO)

#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative                (liftA2)
#endif

unwrapDynamic :: forall a . Typeable a => Dynamic -> a
unwrapDynamic x = fromMaybe (error msg) $ fromDynamic x
    where msg = "unwrapDynamic failed: Expected " ++ show (typeRep (Proxy :: Proxy a)) ++
                ", but got " ++ show (dynTypeRep x)

---------------------------------------------------------------------
-- RULES

type TheRules = Map.HashMap TypeRep Dynamic

-- | A computation that defines all the rules that form part of the computation graph.
--
-- 'Rules' has access to 'IO' through 'MonadIO'. Use of 'IO' is at your own risk: if
-- you write 'Rules' that throw exceptions, then you need to make sure to handle them
-- yourself when you run the resulting 'Rules'.
newtype Rules a = Rules (ReaderT SRules IO a)
    deriving newtype (Monad, Applicative, Functor, MonadIO)

data SRules = SRules {
    rulesExtra   :: !Dynamic,
    rulesActions :: !(IORef [Action ()]),
    rulesMap     :: !(IORef TheRules)
    }

---------------------------------------------------------------------
-- ACTIONS

-- | An action representing something that can be run as part of a 'Rule'.
--
-- 'Action's can be pure functions but also have access to 'IO' via 'MonadIO' and 'MonadUnliftIO.
-- It should be assumed that actions throw exceptions, these can be caught with
-- 'Development.IDE.Graph.Internal.Action.actionCatch'. In particular, it is
-- permissible to use the 'MonadFail' instance, which will lead to an 'IOException'.
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

-- Notice, invariant to maintain:
-- the ![KeySet] in ResultDeps need to be stored in reverse order,
-- so that we can append to it efficiently, and we need the ordering
-- so we can do a linear dependency refreshing in refreshDeps.
data ResultDeps = UnknownDeps | AlwaysRerunDeps !KeySet | ResultDeps ![KeySet]
  deriving (Eq, Show)

getResultDepsDefault :: KeySet -> ResultDeps -> KeySet
getResultDepsDefault _ (ResultDeps ids)      = fold ids
getResultDepsDefault _ (AlwaysRerunDeps ids) = ids
getResultDepsDefault def UnknownDeps         = def

mapResultDeps :: (KeySet -> KeySet) -> ResultDeps -> ResultDeps
mapResultDeps f (ResultDeps ids)      = ResultDeps $ fmap f ids
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
    ,runHook    :: STM ()
        -- ^ The hook to run after the rule completes.
    } deriving Functor

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
