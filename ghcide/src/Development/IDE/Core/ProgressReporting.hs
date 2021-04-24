{-# LANGUAGE RankNTypes #-}
module Development.IDE.Core.ProgressReporting
  ( ProgressEvent(..)
  , ProgressReporting(..)
  , noProgressReporting
  , delayedProgressReporting
  -- utilities, reexported for use in Core.Shake
  , mRunLspT
  , mRunLspTCallback
  )
   where

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Concurrent.Strict
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Control.Monad.STM              as STM
import           Control.Monad.Trans.Class      (lift)
import qualified Data.HashMap.Strict            as HMap
import qualified Data.Text                      as T
import           Data.Unique
import           Development.IDE.GHC.Orphans    ()
import           Development.IDE.Graph          hiding (ShakeValue)
import           Development.IDE.Types.Location
import           Development.IDE.Types.Options
import qualified Language.LSP.Server            as LSP
import           Language.LSP.Types
import qualified Language.LSP.Types             as LSP
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

-- | A 'ProgressReporting' that enqueues Begin and End notifications in a new
--   thread, with a grace period (nothing will be sent if 'KickCompleted' arrives
--   before the end of the grace period).
--   Avoid using in tests where progress notifications are used to assert invariants.
delayedProgressReporting
  :: Seconds  -- ^ Grace period before starting
  -> Seconds  -- ^ sampling delay
  -> Maybe (LSP.LanguageContextEnv c)
  -> ProgressReportingStyle
  -> IO ProgressReporting
delayedProgressReporting before after lspEnv optProgressStyle = do
    inProgressVar <- newVar (HMap.empty @NormalizedFilePath @Int)
    mostRecentProgressEvent <- newTVarIO KickCompleted
    progressAsync <- async $
            progressThread optProgressStyle mostRecentProgressEvent inProgressVar
    let progressUpdate = atomically . writeTVar mostRecentProgressEvent
        progressStop   = cancel progressAsync
        inProgress :: NormalizedFilePath -> Action a -> Action a
        inProgress = withProgressVar inProgressVar
    return ProgressReporting{..}
    where
        -- The progress thread is a state machine with two states:
        --   1. Idle
        --   2. Reporting a kick event
        -- And two transitions, modelled by 'ProgressEvent':
        --   1. KickCompleted - transitions from Reporting into Idle
        --   2. KickStarted - transitions from Idle into Reporting
        progressThread style mostRecentProgressEvent inProgress = progressLoopIdle
          where
            progressLoopIdle = do
                atomically $ do
                    v <- readTVar mostRecentProgressEvent
                    case v of
                        KickCompleted -> STM.retry
                        KickStarted   -> return ()
                asyncReporter <- async $ mRunLspT lspEnv lspShakeProgress
                progressLoopReporting asyncReporter
            progressLoopReporting asyncReporter = do
                atomically $ do
                    v <- readTVar mostRecentProgressEvent
                    case v of
                        KickStarted   -> STM.retry
                        KickCompleted -> return ()
                cancel asyncReporter
                progressLoopIdle

            lspShakeProgress :: LSP.LspM config ()
            lspShakeProgress = do
                -- first sleep a bit, so we only show progress messages if it's going to take
                -- a "noticable amount of time" (we often expect a thread kill to arrive before the sleep finishes)
                liftIO $ sleep before
                u <- ProgressTextToken . T.pack . show . hashUnique <$> liftIO newUnique

                void $ LSP.sendRequest LSP.SWindowWorkDoneProgressCreate
                    LSP.WorkDoneProgressCreateParams { _token = u } $ const (pure ())

                bracket_
                  (start u)
                  (stop u)
                  (loop u 0)
                where
                    start id = LSP.sendNotification LSP.SProgress $
                        LSP.ProgressParams
                            { _token = id
                            , _value = LSP.Begin $ WorkDoneProgressBeginParams
                              { _title = "Processing"
                              , _cancellable = Nothing
                              , _message = Nothing
                              , _percentage = Nothing
                              }
                            }
                    stop id = LSP.sendNotification LSP.SProgress
                        LSP.ProgressParams
                            { _token = id
                            , _value = LSP.End WorkDoneProgressEndParams
                              { _message = Nothing
                              }
                            }
                    loop id prev = do
                        liftIO $ sleep after
                        current <- liftIO $ readVar inProgress
                        let done = length $ filter (== 0) $ HMap.elems current
                        let todo = HMap.size current
                        let next = 100 * fromIntegral done / fromIntegral todo
                        when (next /= prev) $
                          LSP.sendNotification LSP.SProgress $
                          LSP.ProgressParams
                              { _token = id
                              , _value = LSP.Report $ case style of
                                  Explicit -> LSP.WorkDoneProgressReportParams
                                    { _cancellable = Nothing
                                    , _message = Just $ T.pack $ show done <> "/" <> show todo
                                    , _percentage = Nothing
                                    }
                                  Percentage -> LSP.WorkDoneProgressReportParams
                                    { _cancellable = Nothing
                                    , _message = Nothing
                                    , _percentage = Just next
                                    }
                                  NoProgress -> LSP.WorkDoneProgressReportParams
                                    { _cancellable = Nothing
                                    , _message = Nothing
                                    , _percentage = Nothing
                                    }
                              }
                        loop id next

        withProgressVar var file = actionBracket (f succ) (const $ f pred) . const
            -- This functions are deliberately eta-expanded to avoid space leaks.
            -- Do not remove the eta-expansion without profiling a session with at
            -- least 1000 modifications.
            where f shift = void $ modifyVar' var $ HMap.insertWith (\_ x -> shift x) file (shift 0)

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
