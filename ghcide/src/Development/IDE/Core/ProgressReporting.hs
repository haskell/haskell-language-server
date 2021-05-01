{-# LANGUAGE RankNTypes #-}
module Development.IDE.Core.ProgressReporting
  ( ProgressEvent(..)
  , ProgressReporting(..)
  , noProgressReporting
  , makeProgressReporting
  -- utilities, reexported for use in Core.Shake
  , mRunLspT
  , mRunLspTCallback
  ) where

import           Control.Concurrent.Async
import           Control.Concurrent.Strict
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class      (lift)
import           Data.Foldable                  (for_, traverse_)
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as HMap
import           Data.IORef
import           Data.IORef.Extra               (atomicModifyIORef'_)
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

-- | A 'ProgressReporting' that sends the WorkDone Begin and End notifications
--   synchronously. Progress notifications are sent from a sampling thread.
--
--  This 'ProgressReporting' is currently used only in tests.
makeProgressReporting
    :: Seconds -- ^ sleep before reporting
    -> Seconds -- ^ sleep after reporting
    -> Maybe (LSP.LanguageContextEnv config)
    -> ProgressReportingStyle
    -> IO ProgressReporting
makeProgressReporting before after env style = do
    st <- newIORef Nothing
    inProgressVar <- newIORef (HMap.empty @NormalizedFilePath @Int)

    let progressUpdate KickStarted = do
          readIORef st >>= traverse_ (mRunLspT env . stop)
          u <- newProgressToken
          mRunLspT env $ do
              ready <- create u
              for_ ready $ \_ -> do
                  start u
                  liftIO $ writeIORef st (Just u)
        progressUpdate KickCompleted = do
            mbToken <- atomicModifyIORef st (Nothing,)
            for_ mbToken $ \u ->
                mRunLspT env $ stop u

        inProgress file = actionBracket (f file succ) (const $ f file pred) . const
        -- This function is deliberately eta-expanded to avoid space leaks.
        -- Do not remove the eta-expansion without profiling a session with at
        -- least 1000 modifications.
        f file shift = atomicModifyIORef'_ inProgressVar $
                HMap.insertWith (\_ x -> shift x) file (shift 0)

        progressLoop :: Seconds -> LSP.LspM a ()
        progressLoop prev = do
            liftIO $ sleep before
            mbToken <- liftIO $ readIORef st
            next <- case mbToken of
                Nothing ->
                    pure 0
                Just t -> do
                    current <- liftIO $ readIORef inProgressVar
                    progress style prev current t
            liftIO $ sleep after
            progressLoop next

    progressThread <- async $ mRunLspT env $ progressLoop 0
    let progressStop = cancel progressThread

    pure ProgressReporting {..}

newProgressToken :: IO ProgressToken
newProgressToken = ProgressTextToken . T.pack . show . hashUnique <$> liftIO newUnique

create
    :: LSP.MonadLsp config f
    => ProgressToken
    -> f (Either ResponseError Empty)
create u = do
    b <- liftIO newBarrier
    _ <- LSP.sendRequest LSP.SWindowWorkDoneProgressCreate
            LSP.WorkDoneProgressCreateParams { _token = u }
            (liftIO . signalBarrier b)
    liftIO $ waitBarrier b

start :: LSP.MonadLsp config f => ProgressToken -> f ()
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
stop :: LSP.MonadLsp config f => ProgressToken -> f ()
stop id = LSP.sendNotification LSP.SProgress
    LSP.ProgressParams
        { _token = id
        , _value = LSP.End WorkDoneProgressEndParams
            { _message = Nothing
            }
        }

progress :: (LSP.MonadLsp config f) =>
  ProgressReportingStyle -> Seconds -> HashMap NormalizedFilePath Int -> ProgressToken -> f Seconds
progress style prev current id = do
    let done = length $ filter (== 0) $ HMap.elems current
    let todo = HMap.size current
    let next = 100 * fromIntegral done / fromIntegral todo
    when (next /= prev) $ LSP.sendNotification LSP.SProgress $ LSP.ProgressParams
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
    return next

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
