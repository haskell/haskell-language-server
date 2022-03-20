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
  , logError, logWarning, logInfo, logDebug
  , noLogging
  , WithPriority(..)
  , logWith
  , cmap
  , cmapIO
  , cfilter
  , withDefaultRecorder
  , makeDefaultStderrRecorder
  , priorityToHsLoggerPriority
  , LoggingColumn(..)
  , cmapWithPrio
  , withBacklog
  , lspClientMessageRecorder
  , lspClientLogRecorder
  , module PrettyPrinterModule
  , renderStrict
  ) where

import           Control.Concurrent            (myThreadId)
import           Control.Concurrent.Extra      (Lock, newLock, withLock)
import           Control.Concurrent.STM        (atomically,
                                                newTVarIO, writeTVar, readTVarIO, newTBQueueIO, flushTBQueue, writeTBQueue, isFullTBQueue)
import           Control.Exception             (IOException)
import           Control.Monad                 (forM_, when, (>=>), unless)
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Data.Foldable                 (for_)
import           Data.Functor.Contravariant    (Contravariant (contramap))
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text                     as Text
import qualified Data.Text.IO                  as Text
import           Data.Time                     (defaultTimeLocale, formatTime,
                                                getCurrentTime)
import           GHC.Stack                     (CallStack, HasCallStack,
                                                SrcLoc (SrcLoc, srcLocModule, srcLocStartCol, srcLocStartLine),
                                                callStack, getCallStack,
                                                withFrozenCallStack)
import           Language.LSP.Server
import qualified Language.LSP.Server           as LSP
import           Language.LSP.Types            (LogMessageParams (..),
                                                MessageType (..),
                                                SMethod (SWindowLogMessage, SWindowShowMessage),
                                                ShowMessageParams (..))
import           Prettyprinter                 as PrettyPrinterModule
import           Prettyprinter.Render.Text     (renderStrict)
import           System.IO                     (Handle, IOMode (AppendMode),
                                                hClose, hFlush, hSetEncoding,
                                                openFile, stderr, utf8)
import qualified System.Log.Formatter          as HSL
import qualified System.Log.Handler            as HSL
import qualified System.Log.Handler.Simple     as HSL
import qualified System.Log.Logger             as HsLogger
import           UnliftIO                      (MonadUnliftIO, displayException,
                                                finally, try)

data Priority
-- Don't change the ordering of this type or you will mess up the Ord
-- instance
    = Debug -- ^ Verbose debug logging.
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

noLogging :: Logger
noLogging = Logger $ \_ _ -> return ()

data WithPriority a = WithPriority { priority :: Priority, callStack_ :: CallStack, payload :: a } deriving Functor

-- | Note that this is logging actions _of the program_, not of the user.
--   You shouldn't call warning/error if the user has caused an error, only
--   if our code has gone wrong and is itself erroneous (e.g. we threw an exception).
newtype Recorder msg = Recorder
  { logger_ :: forall m. (MonadIO m) => msg -> m () }

logWith :: (HasCallStack, MonadIO m) => Recorder (WithPriority msg) -> Priority -> msg -> m ()
logWith recorder priority msg = withFrozenCallStack $ logger_ recorder (WithPriority priority callStack msg)

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

cmapWithPrio :: (a -> b) -> Recorder (WithPriority b) -> Recorder (WithPriority a)
cmapWithPrio f = cmap (fmap f)

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

-- | Priority is actually for hslogger compatibility
makeDefaultStderrRecorder :: MonadIO m => Maybe [LoggingColumn] -> Priority -> m (Recorder (WithPriority (Doc a)))
makeDefaultStderrRecorder columns minPriority = do
  lock <- liftIO newLock
  makeDefaultHandleRecorder columns minPriority lock stderr

-- | If no path given then use stderr, otherwise use file.
-- Kinda complicated because we also need to setup `hslogger` for
-- `hie-bios` log compatibility reasons. If `hie-bios` can be set to use our
-- logger instead or if `hie-bios` doesn't use `hslogger` then `hslogger` can
-- be removed completely. See `setupHsLogger` comment.
withDefaultRecorder
  :: MonadUnliftIO m
  => Maybe FilePath
  -- ^ Log file path. `Nothing` uses stderr
  -> Maybe [LoggingColumn]
  -- ^ logging columns to display. `Nothing` uses `defaultLoggingColumns`
  -> Priority
  -- ^ min priority for hslogger compatibility
  -> (Recorder (WithPriority (Doc d)) -> m a)
  -- ^ action given a recorder
  -> m a
withDefaultRecorder path columns minPriority action = do
  lock <- liftIO newLock
  let makeHandleRecorder = makeDefaultHandleRecorder columns minPriority lock
  case path of
    Nothing -> do
      recorder <- makeHandleRecorder stderr
      let message = "No log file specified; using stderr."
      logWith recorder Info message
      action recorder
    Just path -> do
      fileHandle :: Either IOException Handle <- liftIO $ try (openFile path AppendMode)
      case fileHandle of
        Left e -> do
          recorder <- makeHandleRecorder stderr
          let exceptionMessage = pretty $ displayException e
          let message = vcat [exceptionMessage, "Couldn't open log file" <+> pretty path <> "; falling back to stderr."]
          logWith recorder Warning message
          action recorder
        Right fileHandle -> finally (makeHandleRecorder fileHandle >>= action) (liftIO $ hClose fileHandle)

makeDefaultHandleRecorder
  :: MonadIO m
  => Maybe [LoggingColumn]
  -- ^ built-in logging columns to display. Nothing uses the default
  -> Priority
  -- ^ min priority for hslogger compatibility
  -> Lock
  -- ^ lock to take when outputting to handle
  -> Handle
  -- ^ handle to output to
  -> m (Recorder (WithPriority (Doc a)))
makeDefaultHandleRecorder columns minPriority lock handle = do
  let Recorder{ logger_ } = textHandleRecorder handle
  let threadSafeRecorder = Recorder { logger_ = \msg -> liftIO $ withLock lock (logger_ msg) }
  let loggingColumns = fromMaybe defaultLoggingColumns columns
  let textWithPriorityRecorder = cmapIO (textWithPriorityToText loggingColumns) threadSafeRecorder
  -- see `setupHsLogger` comment
  liftIO $ setupHsLogger lock handle ["hls", "hie-bios"] (priorityToHsLoggerPriority minPriority)
  pure (cmap docToText textWithPriorityRecorder)
  where
    docToText = fmap (renderStrict . layoutPretty defaultLayoutOptions)

priorityToHsLoggerPriority :: Priority -> HsLogger.Priority
priorityToHsLoggerPriority = \case
  Debug   -> HsLogger.DEBUG
  Info    -> HsLogger.INFO
  Warning -> HsLogger.WARNING
  Error   -> HsLogger.ERROR

-- | The purpose of setting up `hslogger` at all is that `hie-bios` uses
-- `hslogger` to output compilation logs. The easiest way to merge these logs
-- with our log output is to setup an `hslogger` that uses the same handle
-- and same lock as our loggers. That way the output from our loggers and
-- `hie-bios` don't interleave strangely.
-- It may be possible to have `hie-bios` use our logger by decorating the
-- `Cradle.cradleOptsProg.runCradle` we get in the Cradle from
-- `HieBios.findCradle`, but I remember trying that and something not good
-- happened. I'd have to try it again to remember if that was a real issue.
-- Once that is figured out or `hie-bios` doesn't use `hslogger`, then all
-- references to `hslogger` can be removed entirely.
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

data LoggingColumn
  = TimeColumn
  | ThreadIdColumn
  | PriorityColumn
  | DataColumn
  | SourceLocColumn

defaultLoggingColumns :: [LoggingColumn]
defaultLoggingColumns = [TimeColumn, PriorityColumn, DataColumn]

textWithPriorityToText :: [LoggingColumn] -> WithPriority Text -> IO Text
textWithPriorityToText columns WithPriority{ priority, callStack_, payload } = do
    textColumns <- mapM loggingColumnToText columns
    pure $ Text.intercalate " | " textColumns
    where
      showAsText :: Show a => a -> Text
      showAsText = Text.pack . show

      utcTimeToText utcTime = Text.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%6QZ" utcTime

      priorityToText :: Priority -> Text
      priorityToText = showAsText

      threadIdToText = showAsText

      callStackToSrcLoc :: CallStack -> Maybe SrcLoc
      callStackToSrcLoc callStack =
        case getCallStack callStack of
          (_, srcLoc) : _ -> Just srcLoc
          _               -> Nothing

      srcLocToText = \case
          Nothing -> "<unknown>"
          Just SrcLoc{ srcLocModule, srcLocStartLine, srcLocStartCol } ->
            Text.pack srcLocModule <> "#" <> showAsText srcLocStartLine <> ":" <> showAsText srcLocStartCol

      loggingColumnToText :: LoggingColumn -> IO Text
      loggingColumnToText = \case
        TimeColumn -> do
          utcTime <- getCurrentTime
          pure (utcTimeToText utcTime)
        SourceLocColumn -> pure $ (srcLocToText . callStackToSrcLoc) callStack_
        ThreadIdColumn -> do
          threadId <- myThreadId
          pure (threadIdToText threadId)
        PriorityColumn -> pure (priorityToText priority)
        DataColumn -> pure payload

-- | Given a 'Recorder' that requires an argument, produces a 'Recorder'
-- that queues up messages until the argument is provided using the callback, at which
-- point it sends the backlog and begins functioning normally.
withBacklog :: (v -> Recorder a) -> IO (Recorder a, v -> IO ())
withBacklog recFun = do
  -- Arbitrary backlog capacity
  backlog <- newTBQueueIO 100
  let backlogRecorder = Recorder $ \it -> liftIO $ atomically $ do
          -- If the queue is full just drop the message on the floor. This is most likely
          -- to happen if the callback is just never going to be called; in which case
          -- we want neither to build up an unbounded backlog in memory, nor block waiting
          -- for space!
          full <- isFullTBQueue backlog
          unless full $ writeTBQueue backlog it

  -- The variable holding the recorder starts out holding the recorder that writes
  -- to the backlog.
  recVar <- newTVarIO backlogRecorder
  -- The callback atomically swaps out the recorder for the final one, and flushes
  -- the backlog to it.
  let cb arg = do
        let recorder = recFun arg
        toRecord <- atomically $ writeTVar recVar recorder >> flushTBQueue backlog
        for_ toRecord (logger_ recorder)

  -- The recorder we actually return looks in the variable and uses whatever is there.
  let varRecorder = Recorder $ \it -> do
          r <- liftIO $ readTVarIO recVar
          logger_ r it

  pure (varRecorder, cb)

-- | Creates a recorder that sends logs to the LSP client via @window/showMessage@ notifications.
lspClientMessageRecorder :: LanguageContextEnv config -> Recorder (WithPriority Text)
lspClientMessageRecorder env = Recorder $ \WithPriority {..} ->
  liftIO $ LSP.runLspT env $ LSP.sendNotification SWindowShowMessage
      ShowMessageParams
        { _xtype = priorityToLsp priority,
          _message = payload
        }

-- | Creates a recorder that sends logs to the LSP client via @window/logMessage@ notifications.
lspClientLogRecorder :: LanguageContextEnv config -> Recorder (WithPriority Text)
lspClientLogRecorder env = Recorder $ \WithPriority {..} ->
  liftIO $ LSP.runLspT env $ LSP.sendNotification SWindowLogMessage
      LogMessageParams
        { _xtype = priorityToLsp priority,
          _message = payload
        }

priorityToLsp :: Priority -> MessageType
priorityToLsp =
  \case
    Debug   -> MtLog
    Info    -> MtInfo
    Warning -> MtWarning
    Error   -> MtError
