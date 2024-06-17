module Development.IDE.Core.ProgressReporting
  ( ProgressEvent (..),
    ProgressReporting (..),
    noProgressReporting,
    progressReporting,
    progressReportingOutsideState,
    -- utilities, reexported for use in Core.Shake
    mRunLspT,
    mRunLspTCallback,
    -- for tests
    recordProgress,
    InProgressState (..),
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
  = ProgressStarted
  | ProgressCompleted

data ProgressReporting m = ProgressReporting
  { progressUpdate :: ProgressEvent -> IO (),
    inProgress     :: forall a. NormalizedFilePath -> m a -> m a,
    progressStop   :: IO ()
  }

noProgressReporting :: IO (ProgressReporting m)
noProgressReporting =
  return $
    ProgressReporting
      { progressUpdate = const $ pure (),
        inProgress = const id,
        progressStop = pure ()
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
updateState start (Event ProgressStarted) NotStarted = Running <$> async start
updateState start (Event ProgressStarted) (Running job) = cancel job >> Running <$> async start
updateState _ (Event ProgressCompleted) (Running job) = cancel job $> NotStarted
updateState _ (Event ProgressCompleted) st = pure st
updateState _ StopProgress (Running job) = cancel job $> Stopped
updateState _ StopProgress st = pure st

-- | Data structure to track progress across the project
data InProgressState
  = InProgressState
      { -- | Number of files to do
        todoVar    :: TVar Int,
        -- | Number of files done
        doneVar    :: TVar Int,
        currentVar :: STM.Map NormalizedFilePath Int
      }
  | InProgressStateOutSide
    -- we transform the outside state into STM Int for progress reporting purposes
      { -- | Number of files to do
        todo :: STM Int,
        -- | Number of files done
        done :: STM Int
      }

newInProgress :: IO InProgressState
newInProgress = InProgressState <$> newTVarIO 0 <*> newTVarIO 0 <*> STM.newIO

recordProgress :: InProgressState -> NormalizedFilePath -> (Int -> Int) -> IO ()
recordProgress InProgressStateOutSide {} _ _ = return ()
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

progressReporting ::
  (MonadUnliftIO m, MonadIO m) =>
  Maybe (LSP.LanguageContextEnv c) ->
  T.Text ->
  ProgressReportingStyle ->
  IO (ProgressReporting m)
progressReporting = progressReporting' newInProgress

progressReportingOutsideState ::
  (MonadUnliftIO m, MonadIO m) =>
  STM Int ->
  STM Int ->
  Maybe (LSP.LanguageContextEnv c) ->
  T.Text ->
  ProgressReportingStyle ->
  IO (ProgressReporting m)
progressReportingOutsideState todo done = progressReporting' (pure $ InProgressStateOutSide todo done)

progressReporting' ::
  (MonadUnliftIO m, MonadIO m) =>
  IO InProgressState ->
  Maybe (LSP.LanguageContextEnv c) ->
  T.Text ->
  ProgressReportingStyle ->
  IO (ProgressReporting m)
progressReporting' _newState Nothing _title _optProgressStyle = noProgressReporting
progressReporting' newState (Just lspEnv) title optProgressStyle = do
  inProgressState <- newState
  progressState <- newVar NotStarted
  let progressUpdate event = updateStateVar $ Event event
      progressStop = updateStateVar StopProgress
      updateStateVar = modifyVar_ progressState . updateState (lspShakeProgressNew inProgressState)
      inProgress = updateStateForFile inProgressState
  return ProgressReporting {..}
  where
    lspShakeProgressNew :: InProgressState -> IO ()
    lspShakeProgressNew InProgressStateOutSide {..} = progressCounter lspEnv todo done
    lspShakeProgressNew InProgressState {..} = progressCounter lspEnv (readTVar todoVar) (readTVar doneVar)
    -- Kill this to complete the progress session
    progressCounter
      :: LSP.LanguageContextEnv c
      -> STM Int
      -> STM Int
      -> IO ()
    progressCounter lspEnv getTodo getDone =
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
    updateStateForFile inProgress file = UnliftIO.bracket (liftIO $ f succ) (const $ liftIO $ f pred) . const
      where
        -- This functions are deliberately eta-expanded to avoid space leaks.
        -- Do not remove the eta-expansion without profiling a session with at
        -- least 1000 modifications.

        f shift = recordProgress inProgress file shift

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
