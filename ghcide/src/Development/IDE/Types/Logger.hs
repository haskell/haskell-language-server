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
  , logWith, cmap, cmapIO, cfilter, withDefaultRecorder, makeDefaultStderrRecorder) where

import           Control.Concurrent                    (myThreadId)
import           Control.Concurrent.Extra              (Lock, newLock, withLock)
import           Control.Exception                     (IOException, try)
import           Control.Monad                         (forM_, when, (>=>))
import           Control.Monad.IO.Class                (MonadIO (liftIO))
import           Data.Functor.Contravariant            (Contravariant (contramap))
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import qualified Data.Text                             as Text
import qualified Data.Text.IO                          as Text
import           Data.Text.Prettyprint.Doc             (Doc, Pretty (pretty),
                                                        defaultLayoutOptions,
                                                        layoutPretty, (<+>))
import           Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import           Data.Time                             (defaultTimeLocale,
                                                        formatTime,
                                                        getCurrentTime)
import           GHC.Stack                             (HasCallStack,
                                                        SrcLoc (SrcLoc, srcLocModule, srcLocStartLine),
                                                        getCallStack,
                                                        withFrozenCallStack)
import           System.IO                             (Handle,
                                                        IOMode (AppendMode),
                                                        hClose, hFlush,
                                                        hSetEncoding, openFile,
                                                        stderr, utf8)
import qualified System.Log.Formatter                  as HSL
import qualified System.Log.Handler                    as HSL
import qualified System.Log.Handler.Simple             as HSL
import qualified System.Log.Logger                     as HsLogger
import           UnliftIO                              (MonadUnliftIO, finally)

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
  { logger_ :: forall m. (HasCallStack, MonadIO m) => msg -> m () }

logWith :: (HasCallStack, MonadIO m) => Recorder msg -> msg -> m ()
logWith recorder msg = withFrozenCallStack $ logger_ recorder msg

instance Semigroup (Recorder msg) where
  (<>) Recorder{ logger_ = logger_1 } Recorder{ logger_ = logger_2 } =
    Recorder
      { logger_ = \msg -> logger_1 msg >> logger_2 msg }

instance Monoid (Recorder msg) where
  mempty =
    Recorder
      { logger_ = \_ -> pure () }

instance Contravariant Recorder where
  contramap f Recorder{ logger_ } =
    Recorder
      { logger_ = logger_ . f }

cmap :: (a -> b) -> Recorder b -> Recorder a
cmap = contramap

cmapIO :: (a -> IO b) -> Recorder b -> Recorder a
cmapIO f Recorder{ logger_ } =
  Recorder
    { logger_ = (liftIO . f) >=> logger_ }

cfilter :: (a -> Bool) -> Recorder a -> Recorder a
cfilter p Recorder{ logger_ } =
  Recorder
    { logger_ = \msg -> when (p msg) (logger_ msg) }

textHandleRecorder :: Handle -> Recorder Text
textHandleRecorder handle =
  Recorder
    { logger_ = \text -> liftIO $ Text.hPutStrLn handle text *> hFlush handle }

makeDefaultStderrRecorder :: MonadIO m => HsLogger.Priority -> m (Recorder (WithPriority (Doc a)))
makeDefaultStderrRecorder hsLoggerMinPriority = do
  lock <- liftIO newLock
  makeDefaultHandleRecorder hsLoggerMinPriority lock stderr

-- | If no path given then use stderr, otherwise use file.
-- kinda complicated because we are logging with both hslogger and our own
-- logger simultaneously
withDefaultRecorder
  :: MonadUnliftIO m
  => Maybe FilePath
  -> HsLogger.Priority
  -> (Recorder (WithPriority (Doc d)) -> m a)
  -> m a
withDefaultRecorder path hsLoggerMinPriority action = do
  lock <- liftIO newLock
  let makeHandleRecorder = makeDefaultHandleRecorder hsLoggerMinPriority lock
  case path of
    Nothing -> makeHandleRecorder stderr >>= action
    Just path -> do
      handle :: Either IOException Handle <- liftIO $ try (openFile path AppendMode)
      case handle of
        Left _ -> makeHandleRecorder stderr >>= \recorder ->
          logWith recorder (WithPriority Error $ "Couldn't open log file" <+> pretty path <> "; falling back to stderr.")
          >> action recorder
        Right handle -> finally (makeHandleRecorder handle >>= action) (liftIO $ hClose handle)

makeDefaultHandleRecorder :: MonadIO m => HsLogger.Priority -> Lock -> Handle -> m (Recorder (WithPriority (Doc a)))
makeDefaultHandleRecorder hsLoggerMinPriority lock handle = do
  let Recorder{ logger_ } = textHandleRecorder handle
  let threadSafeRecorder = Recorder { logger_ = \msg -> liftIO $ withLock lock (logger_ msg) }
  let textWithPriorityRecorder = cmapIO textWithPriorityToText threadSafeRecorder
  liftIO $ setupHsLogger lock handle ["hls", "hie-bios"] hsLoggerMinPriority
  pure (cmap docToText textWithPriorityRecorder)
  where
    docToText = fmap (renderStrict . layoutPretty defaultLayoutOptions)

-- taken from LSP.setupLogger
-- used until contravariant logging system is fully in place
setupHsLogger :: Lock -> Handle -> [String] -> HsLogger.Priority -> IO ()
setupHsLogger lock handle extraLogNames level = do
  hSetEncoding handle utf8

  logH <- HSL.streamHandler handle level

  let logHandle  = logH
        { HSL.writeFunc = \a s -> withLock lock $ HSL.writeFunc logH a s }
      logFormatter  = HSL.tfLogFormatter logDateFormat logFormat
      logHandler = HSL.setFormatter logHandle logFormatter

  HsLogger.updateGlobalLogger HsLogger.rootLoggerName $ HsLogger.setHandlers ([] :: [HSL.GenericHandler Handle])
  HsLogger.updateGlobalLogger "haskell-lsp" $ HsLogger.setHandlers [logHandler]
  HsLogger.updateGlobalLogger "haskell-lsp" $ HsLogger.setLevel level

  -- Also route the additional log names to the same log
  forM_ extraLogNames $ \logName -> do
    HsLogger.updateGlobalLogger logName $ HsLogger.setHandlers [logHandler]
    HsLogger.updateGlobalLogger logName $ HsLogger.setLevel level
  where
    logFormat = "$time [$tid] $prio $loggername:\t$msg"
    logDateFormat = "%Y-%m-%d %H:%M:%S%Q"

textWithPriorityToText :: WithPriority Text -> IO Text
textWithPriorityToText = \case
  WithPriority{ priority, payload } -> do
    -- threadId <- myThreadId
    utcTime <- getCurrentTime
    pure $ Text.intercalate " | "
      [ utcTimeToText utcTime
      -- , callStackToLocationText callStack
      -- , threadIdToText threadId
      , priorityToText priority
      , payload ]
    where
      utcTimeToText utcTime = Text.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" utcTime

      -- threadIdToText :: Int -> Text
      -- threadIdToText = Text.pack . show

      -- callStackToLocationText callStack = srcLocText
      --   where
      --     srcLocText =
      --       case getCallStack callStack of
      --         []                                 -> "unknown"
      --         [(_name, srcLoc)]                  -> srcLocToText srcLoc
      --         (_, srcLoc) : (_callerName, _) : _ -> srcLocToText srcLoc

      -- srcLocToText SrcLoc{srcLocModule, srcLocStartLine} =
      --   Text.pack srcLocModule <> ":" <> Text.pack (show srcLocStartLine)

      priorityToText :: Priority -> Text
      priorityToText = Text.pack . show


