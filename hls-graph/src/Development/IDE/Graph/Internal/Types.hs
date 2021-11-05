

{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Development.IDE.Graph.Internal.Types where

import           Control.Applicative
import           Control.Concurrent.Extra
import           Control.Monad.Catch
-- Needed in GHC 8.6.5
import           Control.Monad.Fail
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString                       as BS
import           Data.Dynamic
import qualified Data.HashMap.Strict                   as Map
import           Data.IORef
import           Data.IntSet                           (IntSet)
import           Data.Maybe
import           Data.Typeable
import           Development.IDE.Graph.Classes
import           Development.IDE.Graph.Internal.Ids
import           Development.IDE.Graph.Internal.Intern
import           System.Time.Extra                     (Seconds)


unwrapDynamic :: forall a . Typeable a => Dynamic -> a
unwrapDynamic x = fromMaybe (error msg) $ fromDynamic x
    where msg = "unwrapDynamic failed: Expected " ++ show (typeRep (Proxy :: Proxy a)) ++
                ", but got " ++ show (dynTypeRep x)

---------------------------------------------------------------------
-- RULES

type TheRules = Map.HashMap TypeRep Dynamic

newtype Rules a = Rules (ReaderT SRules IO a)
    deriving (Monad, Applicative, Functor, MonadIO, MonadFail)

data SRules = SRules {
    rulesExtra   :: !Dynamic,
    rulesActions :: !(IORef [Action ()]),
    rulesMap     :: !(IORef TheRules)
    }


---------------------------------------------------------------------
-- ACTIONS

newtype Action a = Action {fromAction :: ReaderT SAction IO a}
    deriving (Monad, Applicative, Functor, MonadIO, MonadFail, MonadThrow, MonadCatch, MonadMask)

data SAction = SAction {
    actionDatabase :: !Database,
    actionDeps     :: !(IORef ResultDeps)
    }

getDatabase :: Action Database
getDatabase = Action $ asks actionDatabase

---------------------------------------------------------------------
-- DATABASE

newtype Step = Step Int
    deriving (Eq,Ord,Hashable)

data Key = forall a . (Typeable a, Eq a, Hashable a, Show a) => Key a

instance Eq Key where
    Key a == Key b = Just a == cast b

instance Hashable Key where
    hashWithSalt i (Key x) = hashWithSalt i (typeOf x, x)

instance Show Key where
    show (Key x) = show x

newtype Value = Value Dynamic

data Database = Database {
    databaseExtra           :: Dynamic,
    databaseRules           :: TheRules,
    databaseStep            :: !(IORef Step),
    -- Hold the lock while mutating Ids/Values
    databaseLock            :: !Lock,
    databaseIds             :: !(IORef (Intern Key)),
    databaseValues          :: !(Ids (Key, Status)),
    databaseReverseDeps     :: !(Ids IntSet),
    databaseReverseDepsLock :: !Lock
    }

data Status
    = Clean Result
    | Dirty (Maybe Result)
    | Running (IO ()) Result (Maybe Result)

getResult :: Status -> Maybe Result
getResult (Clean re)         = Just re
getResult (Dirty m_re)       = m_re
getResult (Running _ _ m_re) = m_re

data Result = Result {
    resultValue     :: !Value,
    resultBuilt     :: !Step, -- ^ the step when it was last recomputed
    resultChanged   :: !Step, -- ^ the step when it last changed
    resultVisited   :: !Step, -- ^ the step when it was last looked up
    resultDeps      :: !ResultDeps,
    resultExecution :: !Seconds, -- ^ How long it took, last time it ran
    resultData      :: BS.ByteString
    }

data ResultDeps = UnknownDeps | AlwaysRerunDeps ![Id] | ResultDeps ![Id]

getResultDepsDefault :: [Id] -> ResultDeps -> [Id]
getResultDepsDefault _ (ResultDeps ids)      = ids
getResultDepsDefault _ (AlwaysRerunDeps ids) = ids
getResultDepsDefault def UnknownDeps         = def

mapResultDeps :: ([Id] -> [Id]) -> ResultDeps -> ResultDeps
mapResultDeps f (ResultDeps ids)      = ResultDeps $ f ids
mapResultDeps f (AlwaysRerunDeps ids) = AlwaysRerunDeps $ f ids
mapResultDeps _ UnknownDeps           = UnknownDeps

instance Semigroup ResultDeps where
    UnknownDeps <> x = x
    x <> UnknownDeps = x
    AlwaysRerunDeps ids <> x = AlwaysRerunDeps (ids <> getResultDepsDefault [] x)
    x <> AlwaysRerunDeps ids = AlwaysRerunDeps (getResultDepsDefault [] x <> ids)
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
      deriving (Eq,Show)

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
-- INSTANCES

instance Semigroup a => Semigroup (Rules a) where
    a <> b = liftA2 (<>) a b

instance Monoid a => Monoid (Rules a) where
    mempty = pure mempty
