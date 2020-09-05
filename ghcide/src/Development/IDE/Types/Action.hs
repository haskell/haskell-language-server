module Development.IDE.Types.Action
  ( DelayedAction (..),
    DelayedActionInternal,
    ActionQueue,
    newQueue,
    pushQueue,
    popQueue,
    doneQueue,
    peekInProgress,
  )
where

import           Control.Concurrent.STM       (STM, TQueue, TVar, atomically,
                                               modifyTVar, newTQueue, newTVar,
                                               readTQueue, readTVar,
                                               writeTQueue)
import           Data.Hashable                (Hashable (..))
import           Data.HashSet                 (HashSet)
import qualified Data.HashSet                 as Set
import           Data.Unique                  (Unique)
import           Development.IDE.Types.Logger
import           Development.Shake            (Action)

data DelayedAction a = DelayedAction
  { uniqueID       :: Maybe Unique,
    -- | Name we use for debugging
    actionName     :: String,
    -- | Priority with which to log the action
    actionPriority :: Priority,
    -- | The payload
    getAction      :: Action a
  }
  deriving (Functor)

type DelayedActionInternal = DelayedAction ()

instance Eq (DelayedAction a) where
  a == b = uniqueID a == uniqueID b

instance Hashable (DelayedAction a) where
  hashWithSalt s = hashWithSalt s . uniqueID

instance Show (DelayedAction a) where
  show d = "DelayedAction: " ++ actionName d

------------------------------------------------------------------------------

data ActionQueue = ActionQueue
  { newActions :: TQueue DelayedActionInternal,
    inProgress :: TVar (HashSet DelayedActionInternal)
  }

newQueue :: IO ActionQueue
newQueue = atomically $ do
  newActions <- newTQueue
  inProgress <- newTVar mempty
  return ActionQueue {..}

pushQueue :: DelayedActionInternal -> ActionQueue -> STM ()
pushQueue act ActionQueue {..} = writeTQueue newActions act

-- | You must call 'doneQueue' to signal completion
popQueue :: ActionQueue -> STM DelayedActionInternal
popQueue ActionQueue {..} = do
  x <- readTQueue newActions
  modifyTVar inProgress (Set.insert x)
  return x

-- | Completely remove an action from the queue
doneQueue :: DelayedActionInternal -> ActionQueue -> STM ()
doneQueue x ActionQueue {..} =
  modifyTVar inProgress (Set.delete x)

peekInProgress :: ActionQueue -> STM [DelayedActionInternal]
peekInProgress ActionQueue {..} = Set.toList <$> readTVar inProgress
