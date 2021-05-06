{-# LANGUAGE RankNTypes #-}
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
  , InProgress(..)
  )
   where

import           Control.Concurrent.Async
import           Control.Concurrent.Strict
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      (lift)
import           Data.Foldable                  (for_)
import           Data.Functor                   (($>))
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

-- | State used in 'delayedProgressReporting'
data State
    = NotStarted
    | Stopped
    | Running (Async ())

-- | State transitions used in 'delayedProgressReporting'
data Transition = Event ProgressEvent | StopProgress

updateState :: IO () -> Transition -> State -> IO State
updateState _      _                    Stopped     = pure Stopped
updateState start (Event KickStarted)   NotStarted  = Running <$> async start
updateState start (Event KickStarted)   (Running a) = cancel a >> Running <$> async start
updateState _     (Event KickCompleted) (Running a) = cancel a $> NotStarted
updateState _     (Event KickCompleted) st          = pure st
updateState _     StopProgress          (Running a) = cancel a $> Stopped
updateState _     StopProgress          st          = pure st

-- | Data structure to track progress across the project
data InProgress = InProgress
    { todo    :: !Int  -- ^ Number of files to do
    , done    :: !Int  -- ^ Number of files done
    , current :: !(HMap.HashMap NormalizedFilePath Int)
    }

recordProgress :: NormalizedFilePath -> (Int -> Int) -> InProgress -> InProgress
recordProgress file shift InProgress{..} = case HMap.alterF alter file current of
    ((prev, new), m') ->
        let (done',todo') =
                case (prev,new) of
                    (Nothing,0) -> (done+1, todo+1)
                    (Nothing,_) -> (done,   todo+1)
                    (Just 0, 0) -> (done  , todo)
                    (Just 0, _) -> (done-1, todo)
                    (Just _, 0) -> (done+1, todo)
                    (Just _, _) -> (done  , todo)
        in InProgress todo' done' m'
  where
    alter x = let x' = maybe (shift 0) shift x in ((x,x'), Just x')

-- | A 'ProgressReporting' that enqueues Begin and End notifications in a new
--   thread, with a grace period (nothing will be sent if 'KickCompleted' arrives
--   before the end of the grace period).
delayedProgressReporting
  :: Seconds  -- ^ Grace period before starting
  -> Seconds  -- ^ sampling delay
  -> Maybe (LSP.LanguageContextEnv c)
  -> ProgressReportingStyle
  -> IO ProgressReporting
delayedProgressReporting before after lspEnv optProgressStyle = do
    inProgressVar <- newVar $ InProgress 0 0 mempty
    progressState <- newVar NotStarted
    let progressUpdate event = updateStateVar $ Event event
        progressStop   =  updateStateVar StopProgress
        updateStateVar = modifyVar_ progressState . updateState (mRunLspT lspEnv $ lspShakeProgress inProgressVar)

        inProgress :: NormalizedFilePath -> Action a -> Action a
        inProgress = withProgressVar inProgressVar
    return ProgressReporting{..}
    where
        lspShakeProgress inProgress = do
            -- first sleep a bit, so we only show progress messages if it's going to take
            -- a "noticable amount of time" (we often expect a thread kill to arrive before the sleep finishes)
            liftIO $ sleep before
            u <- ProgressTextToken . T.pack . show . hashUnique <$> liftIO newUnique

            b <- liftIO newBarrier
            void $ LSP.sendRequest LSP.SWindowWorkDoneProgressCreate
                LSP.WorkDoneProgressCreateParams { _token = u } $ liftIO . signalBarrier b
            ready <- liftIO $ waitBarrier b

            for_ ready $ const $ bracket_ (start u) (stop u) (loop u 0)
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
                loop _ _ | optProgressStyle == NoProgress =
                    forever $ liftIO $ threadDelay maxBound
                loop id prev = do
                    InProgress{..} <- liftIO $ readVar inProgress
                    liftIO $ sleep after
                    if todo == 0 then loop id 0 else do
                        let next = 100 * fromIntegral done / fromIntegral todo
                        when (next /= prev) $
                          LSP.sendNotification LSP.SProgress $
                          LSP.ProgressParams
                              { _token = id
                              , _value = LSP.Report $ case optProgressStyle of
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
                                  NoProgress -> error "unreachable"
                              }
                        loop id next

        withProgressVar var file = actionBracket (f succ) (const $ f pred) . const
            -- This functions are deliberately eta-expanded to avoid space leaks.
            -- Do not remove the eta-expansion without profiling a session with at
            -- least 1000 modifications.
            where
              f shift = modifyVar' var $ recordProgress file shift

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
