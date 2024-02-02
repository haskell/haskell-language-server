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

import           Control.Concurrent.Async
import           Control.Concurrent.STM.Stats   (TVar, atomicallyNamed,
                                                 modifyTVar', newTVarIO,
                                                 readTVarIO)
import           Control.Concurrent.Strict
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
import qualified Language.LSP.Server            as LSP
import qualified StmContainers.Map              as STM
import           System.Time.Extra
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
    | Running (Async ())

-- | State transitions used in 'delayedProgressReporting'
data Transition = Event ProgressEvent | StopProgress

updateState :: IO (Async ()) -> Transition -> State -> IO State
updateState _      _                    Stopped     = pure Stopped
updateState start (Event KickStarted)   NotStarted  = Running <$> start
updateState start (Event KickStarted)   (Running a) = cancel a >> Running <$> start
updateState _     (Event KickCompleted) (Running a) = cancel a $> NotStarted
updateState _     (Event KickCompleted) st          = pure st
updateState _     StopProgress          (Running a) = cancel a $> Stopped
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
            (Just _, _) -> pure()
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
        updateStateVar = modifyVar_ progressState . updateState (lspShakeProgress inProgressState)

        inProgress = updateStateForFile inProgressState
    return ProgressReporting{..}
    where
        lspShakeProgress InProgressState{..} = do
            -- first sleep a bit, so we only show progress messages if it's going to take
            -- a "noticable amount of time" (we often expect a thread kill to arrive before the sleep finishes)
            liftIO $ sleep before
            u <- ProgressToken . InR . T.pack . show . hashUnique <$> liftIO newUnique

            b <- liftIO newBarrier
            void $ LSP.runLspT lspEnv $ LSP.sendRequest SMethod_WindowWorkDoneProgressCreate
                LSP.WorkDoneProgressCreateParams { _token = u } $ liftIO . signalBarrier b
            liftIO $ async $ do
                ready <- waitBarrier b
                LSP.runLspT lspEnv $ for_ ready $ const $ bracket_ (start u) (stop u) (loop u 0)
            where
                start token = LSP.sendNotification SMethod_Progress $
                    LSP.ProgressParams
                        { _token = token
                        , _value = toJSON $ WorkDoneProgressBegin
                          { _kind = AString @"begin"
                          ,  _title = "Processing"
                          , _cancellable = Nothing
                          , _message = Nothing
                          , _percentage = Nothing
                          }
                        }
                stop token = LSP.sendNotification SMethod_Progress
                    LSP.ProgressParams
                        { _token = token
                        , _value = toJSON $ WorkDoneProgressEnd
                          { _kind = AString @"end"
                           , _message = Nothing
                          }
                        }
                loop _ _ | optProgressStyle == NoProgress =
                    forever $ liftIO $ threadDelay maxBound
                loop token prevPct = do
                    done <- liftIO $ readTVarIO doneVar
                    todo <- liftIO $ readTVarIO todoVar
                    liftIO $ sleep after
                    if todo == 0 then loop token 0 else do
                        let
                            nextFrac :: Double
                            nextFrac = fromIntegral done / fromIntegral todo
                            nextPct :: UInt
                            nextPct = floor $ 100 * nextFrac
                        when (nextPct /= prevPct) $
                          LSP.sendNotification SMethod_Progress $
                          LSP.ProgressParams
                              { _token = token
                              , _value = case optProgressStyle of
                                  Explicit -> toJSON $ WorkDoneProgressReport
                                    { _kind = AString @"report"
                                    , _cancellable = Nothing
                                    , _message = Just $ T.pack $ show done <> "/" <> show todo
                                    , _percentage = Nothing
                                    }
                                  Percentage -> toJSON $ WorkDoneProgressReport
                                    { _kind = AString @"report"
                                    , _cancellable = Nothing
                                    , _message = Nothing
                                    , _percentage = Just nextPct
                                    }
                                  NoProgress -> error "unreachable"
                              }
                        loop token nextPct

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
