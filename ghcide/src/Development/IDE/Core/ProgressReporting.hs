{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Development.IDE.Core.ProgressReporting
  ( ProgressEvent (..),
    ProgressReporting (..),
    ProgressReportingNoTrace,
    noProgressReporting,
    progressReporting,
    progressReportingNoTrace,
    -- utilities, reexported for use in Core.Shake
    mRunLspT,
    mRunLspTCallback,
    -- for tests
    recordProgress,
    InProgressState (..),
    progressStop,
    progressUpdate
  )
where

import           Control.Concurrent.STM         (STM)
import           Control.Concurrent.STM.Stats   (TVar, atomically,
                                                 atomicallyNamed, modifyTVar',
                                                 newTVarIO, readTVar, retry)
import           Control.Concurrent.Strict      (modifyVar_, newVar,
                                                 threadDelay)
import           Control.Monad.Extra            hiding (loop)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      (lift)
import           Data.Functor                   (($>))
import qualified Data.Text                      as T
import           Development.IDE.GHC.Orphans    ()
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import qualified Focus
import           Language.LSP.Protocol.Types
import           Language.LSP.Server            (ProgressAmount (..),
                                                 ProgressCancellable (..),
                                                 withProgress)
import qualified Language.LSP.Server            as LSP
import qualified StmContainers.Map              as STM
import           UnliftIO                       (Async, MonadUnliftIO, async,
                                                 bracket, cancel)

data ProgressEvent
  = ProgressNewStarted
  | ProgressCompleted
  | ProgressStarted

data ProgressReportingNoTrace m = ProgressReportingNoTrace
  { progressUpdateI :: ProgressEvent -> m (),
    progressStopI   :: IO ()
    -- ^ we are using IO here because creating and stopping the `ProgressReporting`
    -- is different from how we use it.
  }

data ProgressReporting m = ProgressReporting
  {
    inProgress             :: forall a. NormalizedFilePath -> m a -> m a,
    -- ^ see Note [ProgressReporting API and InProgressState]
    progressReportingInner :: ProgressReportingNoTrace m
  }


class ProgressReportingClass a where
    type M a :: * -> *
    progressUpdate ::  a -> ProgressEvent -> M a ()
    progressStop :: a -> IO ()

instance ProgressReportingClass (ProgressReportingNoTrace m) where
    type M (ProgressReportingNoTrace m) = m
    progressUpdate = progressUpdateI
    progressStop = progressStopI

instance ProgressReportingClass (ProgressReporting m) where
    type M (ProgressReporting m) = m
    progressUpdate :: ProgressReporting m -> ProgressEvent -> M (ProgressReporting m) ()
    progressUpdate = progressUpdateI . progressReportingInner
    progressStop = progressStopI . progressReportingInner

{- Note [ProgressReporting API and InProgressState]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The progress of tasks can be tracked in two ways:

1. `InProgressState`: This is an internal state that actively tracks the progress.
   Changes to the progress are made directly to this state.

2. `InProgressStateOutSide`: This is an external state that tracks the progress.
   The external state is converted into an STM Int for the purpose of reporting progress.

The `inProgress` function is only useful when we are using `InProgressState`.

An alternative design could involve using GADTs to eliminate this discrepancy between
`InProgressState` and `InProgressStateOutSide`.
-}

noProgressReportingNoTrace :: (MonadUnliftIO m) => (ProgressReportingNoTrace m)
noProgressReportingNoTrace = ProgressReportingNoTrace
      { progressUpdateI = const $ pure (),
        progressStopI = pure ()
      }
noProgressReporting :: (MonadUnliftIO m) => IO (ProgressReporting m)
noProgressReporting =
  return $
    ProgressReporting
      { inProgress = const id,
        progressReportingInner = noProgressReportingNoTrace
      }

-- | State used in 'delayedProgressReporting'
data State
  = NotStarted
  | Stopped
  | Running (Async ())

-- | State transitions used in 'delayedProgressReporting'
data Transition = Event ProgressEvent | StopProgress

updateState :: IO () -> Transition -> State -> IO State
updateState _ _ Stopped = pure Stopped
updateState start (Event ProgressNewStarted) NotStarted = Running <$> async start
updateState start (Event ProgressNewStarted) (Running job) = cancel job >> Running <$> async start
updateState start (Event ProgressStarted) NotStarted = Running <$> async start
updateState _ (Event ProgressStarted) (Running job) = return (Running job)
updateState _ (Event ProgressCompleted) (Running job) = cancel job $> NotStarted
updateState _ (Event ProgressCompleted) st = pure st
updateState _ StopProgress (Running job) = cancel job $> Stopped
updateState _ StopProgress st = pure st

-- | Data structure to track progress across the project
-- see Note [ProgressReporting API and InProgressState]
data InProgressState
  = InProgressState
      { -- | Number of files to do
        todoVar    :: TVar Int,
        -- | Number of files done
        doneVar    :: TVar Int,
        currentVar :: STM.Map NormalizedFilePath Int
      }


newInProgress :: IO InProgressState
newInProgress = InProgressState <$> newTVarIO 0 <*> newTVarIO 0 <*> STM.newIO

recordProgress :: InProgressState -> NormalizedFilePath -> (Int -> Int) -> IO ()
recordProgress InProgressState {..} file shift = do
  (prev, new) <- atomicallyNamed "recordProgress" $ STM.focus alterPrevAndNew file currentVar
  atomicallyNamed "recordProgress2" $ do
    case (prev, new) of
      (Nothing, 0) -> modifyTVar' doneVar (+ 1) >> modifyTVar' todoVar (+ 1)
      (Nothing, _) -> modifyTVar' todoVar (+ 1)
      (Just 0, 0)  -> pure ()
      (Just 0, _)  -> modifyTVar' doneVar pred
      (Just _, 0)  -> modifyTVar' doneVar (+ 1)
      (Just _, _)  -> pure ()
  where
    alterPrevAndNew = do
      prev <- Focus.lookup
      Focus.alter alter
      new <- Focus.lookupWithDefault 0
      return (prev, new)
    alter x = let x' = maybe (shift 0) shift x in Just x'


-- | `progressReportingNoTrace` initiates a new progress reporting session.
-- It functions similarly to `progressReporting`, but it utilizes an external state for progress tracking.
-- Refer to Note [ProgressReporting API and InProgressState] for more details.
progressReportingNoTrace ::
  (MonadUnliftIO m, MonadIO m) =>
  STM Int ->
  STM Int ->
  Maybe (LSP.LanguageContextEnv c) ->
  T.Text ->
  ProgressReportingStyle ->
  IO (ProgressReportingNoTrace m)
progressReportingNoTrace _ _ Nothing _title _optProgressStyle = return noProgressReportingNoTrace
progressReportingNoTrace todo done (Just lspEnv) title optProgressStyle = do
  progressState <- newVar NotStarted
  let progressUpdateI event = liftIO $ updateStateVar $ Event event
      progressStopI = updateStateVar StopProgress
      updateStateVar = modifyVar_ progressState . updateState (progressCounter lspEnv title optProgressStyle todo done)
  return ProgressReportingNoTrace {..}

-- | `progressReporting` initiates a new progress reporting session.
-- It necessitates the active tracking of progress using the `inProgress` function.
-- Refer to Note [ProgressReporting API and InProgressState] for more details.
progressReporting ::
  forall c m.
  (MonadUnliftIO m, MonadIO m) =>
  Maybe (LSP.LanguageContextEnv c) ->
  T.Text ->
  ProgressReportingStyle ->
  IO (ProgressReporting m)
progressReporting Nothing _title _optProgressStyle = noProgressReporting
progressReporting (Just lspEnv) title optProgressStyle = do
  inProgressState <- newInProgress
  progressReportingInner <- progressReportingNoTrace (readTVar $ todoVar inProgressState)
                                (readTVar $ doneVar inProgressState) (Just lspEnv) title optProgressStyle
  let
      inProgress :: forall a. NormalizedFilePath -> m a -> m a
      inProgress = updateStateForFile inProgressState
  return ProgressReporting {..}
  where
    updateStateForFile inProgress file = UnliftIO.bracket (liftIO $ f succ) (const $ liftIO $ f pred) . const
      where
        -- This functions are deliberately eta-expanded to avoid space leaks.
        -- Do not remove the eta-expansion without profiling a session with at
        -- least 1000 modifications.

        f shift = recordProgress inProgress file shift

-- Kill this to complete the progress session
progressCounter ::
  LSP.LanguageContextEnv c ->
  T.Text ->
  ProgressReportingStyle ->
  STM Int ->
  STM Int ->
  IO ()
progressCounter lspEnv title optProgressStyle getTodo getDone =
  LSP.runLspT lspEnv $ withProgress title Nothing NotCancellable $ \update -> loop update 0
  where
    loop _ _ | optProgressStyle == NoProgress = forever $ liftIO $ threadDelay maxBound
    loop update prevPct = do
      (todo, done, nextPct) <- liftIO $ atomically $ do
        todo <- getTodo
        done <- getDone
        let nextFrac :: Double
            nextFrac = if todo == 0 then 0 else fromIntegral done / fromIntegral todo
            nextPct :: UInt
            nextPct = floor $ 100 * nextFrac
        when (nextPct == prevPct) retry
        pure (todo, done, nextPct)

      _ <- update (ProgressAmount (Just nextPct) (Just $ T.pack $ show done <> "/" <> show todo))
      loop update nextPct

mRunLspT :: (Applicative m) => Maybe (LSP.LanguageContextEnv c) -> LSP.LspT c m () -> m ()
mRunLspT (Just lspEnv) f = LSP.runLspT lspEnv f
mRunLspT Nothing _       = pure ()

mRunLspTCallback ::
  (Monad m) =>
  Maybe (LSP.LanguageContextEnv c) ->
  (LSP.LspT c m a -> LSP.LspT c m a) ->
  m a ->
  m a
mRunLspTCallback (Just lspEnv) f g = LSP.runLspT lspEnv $ f (lift g)
mRunLspTCallback Nothing _ g       = g
