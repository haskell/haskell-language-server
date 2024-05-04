module Development.IDE.Core.ProgressReporting
  ( ProgressEvent(..)
  , ProgressReporting(..)
  , noProgressReporting
  , delayedProgressReporting
  -- utilities, reexported for use in Core.Shake
  , mRunLspT
  , mRunLspTCallback
  -- for tests
  , recordProgress
  , InProgressState(..)
  )
   where

import           Control.Concurrent.STM.Stats   (TVar, atomicallyNamed,
                                                 modifyTVar', newTVarIO,
                                                 readTVarIO)
import           Control.Concurrent.Strict      (Barrier, MVar, modifyVar_,
                                                 newBarrier, newEmptyMVar,
                                                 newVar, signalBarrier,
                                                 threadDelay, waitBarrier)
import           Control.Monad.Extra            hiding (loop)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      (lift)
import           Data.Aeson                     (ToJSON (toJSON))
import           Data.Foldable                  (for_)
import           Data.Functor                   (($>))
import qualified Data.Text                      as T
import           Data.Unique
import           Development.IDE.GHC.Orphans    ()
import           Development.IDE.Graph          hiding (ShakeValue)
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import qualified Focus
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types    as LSP
import           Language.LSP.Server            (ProgressAmount (..),
                                                 ProgressCancellable (..),
                                                 withProgress)
import qualified Language.LSP.Server            as LSP
import qualified StmContainers.Map              as STM
import           System.Time.Extra
import           UnliftIO                       (MonadUnliftIO (..),
                                                 UnliftIO (unliftIO), async,
                                                 newMVar, putMVar, race,
                                                 readMVar, toIO, wait, waitAny,
                                                 waitAnyCancel)
import           UnliftIO.Exception             (bracket_)

data ProgressEvent
    = KickStarted
    | KickCompleted

data ProgressReporting  = ProgressReporting
  { progressUpdate :: ProgressEvent -> IO ()
  , inProgress     :: forall a. NormalizedFilePath -> Action a -> Action a
  , progressStop   :: IO ()
  }

noProgressReporting :: IO ProgressReporting
noProgressReporting = return $ ProgressReporting
  { progressUpdate = const $ pure ()
  , inProgress = const id
  , progressStop   = pure ()
  }

-- | State used in 'delayedProgressReporting'
data State
    = NotStarted
    | Stopped
    | Running (IO ())

-- | State transitions used in 'delayedProgressReporting'
data Transition = Event ProgressEvent | StopProgress

updateState :: IO () -> Transition -> State -> IO State
updateState _      _                    Stopped     = pure Stopped
updateState start (Event KickStarted)   NotStarted  = pure $ Running start
updateState start (Event KickStarted)   (Running a) = a $> Running start
updateState _     (Event KickCompleted) (Running a) = a $> NotStarted
updateState _     (Event KickCompleted) st          = pure st
updateState _     StopProgress          (Running a) = a $> Stopped
updateState _     StopProgress          st          = pure st

-- | Data structure to track progress across the project
data InProgressState = InProgressState
    { todoVar    :: TVar Int  -- ^ Number of files to do
    , doneVar    :: TVar Int  -- ^ Number of files done
    , currentVar :: STM.Map NormalizedFilePath Int
    }

newInProgress :: IO InProgressState
newInProgress = InProgressState <$> newTVarIO 0 <*> newTVarIO 0 <*> STM.newIO

recordProgress :: InProgressState -> NormalizedFilePath -> (Int -> Int) -> IO ()
recordProgress InProgressState{..} file shift = do
    (prev, new) <- atomicallyNamed "recordProgress" $ STM.focus alterPrevAndNew file currentVar
    atomicallyNamed "recordProgress2" $ do
        case (prev,new) of
            (Nothing,0) -> modifyTVar' doneVar (+1) >> modifyTVar' todoVar (+1)
            (Nothing,_) -> modifyTVar' todoVar (+1)
            (Just 0, 0) -> pure ()
            (Just 0, _) -> modifyTVar' doneVar pred
            (Just _, 0) -> modifyTVar' doneVar (+1)
            (Just _, _) -> pure ()
  where
    alterPrevAndNew = do
        prev <- Focus.lookup
        Focus.alter alter
        new <- Focus.lookupWithDefault 0
        return (prev, new)
    alter x = let x' = maybe (shift 0) shift x in Just x'


-- | A 'ProgressReporting' that enqueues Begin and End notifications in a new
--   thread, with a grace period (nothing will be sent if 'KickCompleted' arrives
--   before the end of the grace period).
delayedProgressReporting
  :: Seconds  -- ^ Grace period before starting
  -> Seconds  -- ^ sampling delay
  -> Maybe (LSP.LanguageContextEnv c)
  -> ProgressReportingStyle
  -> IO ProgressReporting
delayedProgressReporting _before _after Nothing _optProgressStyle = noProgressReporting
delayedProgressReporting before after (Just lspEnv) optProgressStyle = do
    inProgressState <- newInProgress
    progressState <- newVar NotStarted
    let progressUpdate event = updateStateVar $ Event event
        progressStop   =  updateStateVar StopProgress
        updateStateVar tran = do
            start <- lspShakeProgressNew inProgressState
            modifyVar_ progressState $ updateState start tran
        inProgress = updateStateForFile inProgressState
    return ProgressReporting{..}
    where
        lspShakeProgressNew InProgressState{..} = do
            -- first sleep a bit, so we only show progress messages if it's going to take
            -- a "noticable amount of time" (we often expect a thread kill to arrive before the sleep finishes)
            liftIO $ sleep before
            cancelProgressB <- newBarrier
            job <- async $ LSP.runLspT lspEnv $ withProgress "Processing" Nothing Cancellable $ \update ->
                race (liftIO $ waitBarrier cancelProgressB) (loop update 0)
            return (signalBarrier cancelProgressB () >> wait job >> return ())
            where
                loop _ _ | optProgressStyle == NoProgress =
                    forever $ liftIO $ threadDelay maxBound
                loop update prevPct = do
                    done <- liftIO $ readTVarIO doneVar
                    todo <- liftIO $ readTVarIO todoVar
                    liftIO $ sleep after
                    if todo == 0 then loop update 0 else do
                        let nextFrac :: Double
                            nextFrac = fromIntegral done / fromIntegral todo
                            nextPct :: UInt
                            nextPct = floor $ 100 * nextFrac
                        when (nextPct /= prevPct) $
                            update (ProgressAmount (Just nextPct) (Just $ T.pack $ show done <> "/" <> show todo))
                        loop update nextPct
        updateStateForFile inProgress file = actionBracket (f succ) (const $ f pred) . const
            -- This functions are deliberately eta-expanded to avoid space leaks.
            -- Do not remove the eta-expansion without profiling a session with at
            -- least 1000 modifications.
            where
              f shift = recordProgress inProgress file shift

mRunLspT :: Applicative m => Maybe (LSP.LanguageContextEnv c ) -> LSP.LspT c m () -> m ()
mRunLspT (Just lspEnv) f = LSP.runLspT lspEnv f
mRunLspT Nothing _       = pure ()

mRunLspTCallback :: Monad m
                 => Maybe (LSP.LanguageContextEnv c)
                 -> (LSP.LspT c m a -> LSP.LspT c m a)
                 -> m a
                 -> m a
mRunLspTCallback (Just lspEnv) f g = LSP.runLspT lspEnv $ f (lift g)
mRunLspTCallback Nothing _ g       = g
