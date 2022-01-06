-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE RankNTypes #-}
-- | This is a compatibility module that abstracts over the
-- concrete choice of logging framework so users can plug in whatever
-- framework they want to.
module Development.IDE.Types.Logger
  ( Priority(..)
  , Logger(..)
  , Recorder(..)
  , logError, logWarning, logInfo, logDebug, logTelemetry
  , noLogging
  , WithPriority(..)
  , logWith, cmap, cmapIO, cfilter, withDefaultTextWithPriorityRecorder) where

import           Control.Concurrent         (myThreadId)
import           Control.Concurrent.Extra   (newLock, withLock)
import           Control.Monad              (when, (>=>))
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Data.Functor.Contravariant (Contravariant (contramap))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           Data.Time                  (defaultTimeLocale, formatTime,
                                             getCurrentTime)
import           GHC.Stack                  (HasCallStack,
                                             SrcLoc (SrcLoc, srcLocModule, srcLocStartLine),
                                             getCallStack, withFrozenCallStack)
import           System.IO                  (Handle, IOMode (AppendMode),
                                             hFlush, stderr)
import           UnliftIO                   (MonadUnliftIO, withFile)


data Priority
-- Don't change the ordering of this type or you will mess up the Ord
-- instance
    = Telemetry -- ^ Events that are useful for gathering user metrics.
    | Debug -- ^ Verbose debug logging.
    | Info  -- ^ Useful information in case an error has to be understood.
    | Warning
      -- ^ These error messages should not occur in a expected usage, and
      -- should be investigated.
    | Error -- ^ Such log messages must never occur in expected usage.
    deriving (Eq, Show, Ord, Enum, Bounded)


-- | Note that this is logging actions _of the program_, not of the user.
--   You shouldn't call warning/error if the user has caused an error, only
--   if our code has gone wrong and is itself erroneous (e.g. we threw an exception).
newtype Logger = Logger {logPriority :: Priority -> T.Text -> IO ()}

instance Semigroup Logger where
    l1 <> l2 = Logger $ \p t -> logPriority l1 p t >> logPriority l2 p t

instance Monoid Logger where
    mempty = Logger $ \_ _ -> pure ()

logError :: Logger -> T.Text -> IO ()
logError x = logPriority x Error

logWarning :: Logger -> T.Text -> IO ()
logWarning x = logPriority x Warning

logInfo :: Logger -> T.Text -> IO ()
logInfo x = logPriority x Info

logDebug :: Logger -> T.Text -> IO ()
logDebug x = logPriority x Debug

logTelemetry :: Logger -> T.Text -> IO ()
logTelemetry x = logPriority x Telemetry


noLogging :: Logger
noLogging = Logger $ \_ _ -> return ()

data WithPriority a = WithPriority { priority :: Priority, payload :: a } deriving Functor

-- | Note that this is logging actions _of the program_, not of the user.
--   You shouldn't call warning/error if the user has caused an error, only
--   if our code has gone wrong and is itself erroneous (e.g. we threw an exception).
data Recorder msg = Recorder
  { logger_ :: forall m. (HasCallStack, MonadIO m) => msg -> m ()
  }

logWith :: (HasCallStack, MonadIO m) => Recorder msg -> msg -> m ()
logWith recorder msg = withFrozenCallStack $ logger_ recorder msg

instance Semigroup (Recorder msg) where
  (<>) Recorder{ logger_ = logger_1 } Recorder{ logger_ = logger_2 } =
    Recorder
      { logger_ = \msg -> logger_1 msg >> logger_2 msg
      }

instance Monoid (Recorder msg) where
  mempty =
    Recorder
      { logger_ = \_ -> pure ()
      }

instance Contravariant Recorder where
  contramap f Recorder{ logger_ } =
    Recorder
      { logger_ = logger_ . f
      }

cmap :: (a -> b) -> Recorder b -> Recorder a
cmap = contramap

cmapIO :: (a -> IO b) -> Recorder b -> Recorder a
cmapIO f Recorder{ logger_ } =
  Recorder
    { logger_ = (liftIO . f) >=> logger_
    }

cfilter :: (a -> Bool) -> Recorder a -> Recorder a
cfilter p Recorder{ logger_ } =
  Recorder
    { logger_ = \msg -> when (p msg) (logger_ msg)
    }

textHandleRecorder :: Handle -> Recorder Text
textHandleRecorder handle =
  Recorder
    { logger_ = \text -> liftIO $ Text.hPutStrLn handle text *> hFlush handle
    }

textStderrRecorder :: Recorder Text
textStderrRecorder = textHandleRecorder stderr

-- | Cheap stderr logger_ that relies on LineBuffering
threadSafeTextStderrRecorder :: IO (Recorder Text)
threadSafeTextStderrRecorder = do
  lock <- newLock
  let Recorder{ logger_ } = textStderrRecorder
  pure $ Recorder
    { logger_ = \msg -> liftIO $ withLock lock (logger_ msg)
    }

makeThreadSafeTextStderrRecorder :: MonadIO m => m (Recorder Text)
makeThreadSafeTextStderrRecorder = liftIO threadSafeTextStderrRecorder

withTextFileRecorder :: MonadUnliftIO m => FilePath -> (Recorder Text -> m a) -> m a
withTextFileRecorder path action = withFile path AppendMode $ \handle ->
  action (textHandleRecorder handle)

-- | if no file path given use stderr, else use stderr and file
withDefaultTextRecorder :: MonadUnliftIO m => Maybe FilePath -> (Recorder Text -> m a) -> m a
withDefaultTextRecorder path action = do
  textStderrRecorder <- makeThreadSafeTextStderrRecorder
  case path of
    Nothing -> action textStderrRecorder
    Just path -> withTextFileRecorder path $ \textFileRecorder ->
      action (textStderrRecorder <> textFileRecorder)

withDefaultTextWithPriorityRecorder :: MonadUnliftIO m => Maybe FilePath -> (Recorder (WithPriority Text) -> m a) -> m a
withDefaultTextWithPriorityRecorder path action = do
  withDefaultTextRecorder path $ \textRecorder ->
    action (cmapIO textWithPriorityToText textRecorder)

textWithPriorityToText :: WithPriority Text -> IO Text
textWithPriorityToText = \case
  WithPriority{ priority, payload } -> do
    threadId <- myThreadId
    utcTime <- getCurrentTime
    pure $ Text.intercalate " | "
      [ utcTimeToText utcTime
      -- , callStackToLocationText callStack
      , threadIdToText threadId
      -- , priorityToText priority
      , payload ]
    where
      utcTimeToText utcTime = Text.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" utcTime

      threadIdToText = Text.pack . show

      callStackToLocationText callStack = srcLocText
        where
          srcLocText =
            case getCallStack callStack of
              []                                 -> "unknown"
              [(_name, srcLoc)]                  -> srcLocToText srcLoc
              (_, srcLoc) : (_callerName, _) : _ -> srcLocToText srcLoc

      srcLocToText SrcLoc{srcLocModule, srcLocStartLine} =
        Text.pack srcLocModule <> ":" <> Text.pack (show srcLocStartLine)

      priorityToText :: Priority -> Text
      priorityToText = Text.pack . show


