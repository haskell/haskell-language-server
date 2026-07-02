module Development.IDE.Types.Action
  ( Action
  , DelayedAction (..)
  , DelayedActionInternal
  , ActionQueue
  , newQueue
  , pushQueue
  , popQueue
  , doneQueue
  , peekInProgress
  , abortQueue
  , countQueue
  , isActionQueueEmpty
  , unGetQueue
  , countInProgress
  )
where

import           Control.Concurrent.STM
import           Development.IDE.Graph.Internal.Types (Action, ActionQueue,
                                                       DelayedAction (..),
                                                       DelayedActionInternal,
                                                       abortQueue, countQueue,
                                                       doneQueue,
                                                       isActionQueueEmpty,
                                                       newQueue, peekInProgress,
                                                       popQueue, pushQueue,
                                                       unGetQueue)

countInProgress :: ActionQueue -> STM Int
countInProgress queue = length <$> peekInProgress queue
