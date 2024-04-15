-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | This is a compatibility module that abstracts over the
-- concrete choice of logging framework so users can plug in whatever
-- framework they want to.
module Ide.Logger
  ( Priority(..)
  , Recorder(..)
  , WithPriority(..)
  , logWith
  , cmap
  , cmapIO
  , cfilter
  , withFileRecorder
  , makeDefaultStderrRecorder
  , makeDefaultHandleRecorder
  , LoggingColumn(..)
  , cmapWithPrio
  , withBacklog
  , lspClientMessageRecorder
  , lspClientLogRecorder
  , module PrettyPrinterModule
  , renderStrict
  , toCologActionWithPrio
  , defaultLoggingColumns
  ) where

import           Colog.Core                    (LogAction (..), Severity,
                                                WithSeverity (..))
import qualified Colog.Core                    as Colog
import           Control.Concurrent            (myThreadId)
import           Control.Concurrent.Extra      (Lock, newLock, withLock)
import           Control.Concurrent.STM        (atomically, flushTBQueue,
                                                isFullTBQueue, newTBQueueIO,
                                                newTVarIO, readTVarIO,
                                                writeTBQueue, writeTVar)
import           Control.Exception             (IOException)
import           Control.Monad                 (unless, when, (>=>))
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
import           Language.LSP.Protocol.Message (SMethod (SMethod_WindowLogMessage, SMethod_WindowShowMessage))
import           Language.LSP.Protocol.Types   (LogMessageParams (..),
                                                MessageType (..),
                                                ShowMessageParams (..))
import           Language.LSP.Server
import qualified Language.LSP.Server           as LSP
import           Prettyprinter                 as PrettyPrinterModule
import           Prettyprinter.Render.Text     (renderStrict)
import           System.IO                     (Handle, IOMode (AppendMode),
                                                hClose, hFlush, openFile,
                                                stderr)
import           UnliftIO                      (MonadUnliftIO, finally, try)

data Priority
-- Don't change the ordering of this type or you will mess up the Ord
-- instance
    = Debug -- ^ Verbose debug logging.
    | Info  -- ^ Useful information in case an error has to be understood.
    | Warning
      -- ^ These error messages should not occur in a expected usage, and
      -- should be investigated.
    | Error -- ^ Such log messages must never occur in expected usage.
    deriving (Eq, Show, Read, Ord, Enum, Bounded)

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

makeDefaultStderrRecorder :: MonadIO m => Maybe [LoggingColumn] -> m (Recorder (WithPriority (Doc a)))
makeDefaultStderrRecorder columns = do
  lock <- liftIO newLock
  makeDefaultHandleRecorder columns lock stderr

withFileRecorder
  :: MonadUnliftIO m
  => FilePath
  -- ^ Log file path.
  -> Maybe [LoggingColumn]
  -- ^ logging columns to display. `Nothing` uses `defaultLoggingColumns`
  -> (Either IOException (Recorder (WithPriority (Doc d))) -> m a)
  -- ^ action given a recorder, or the exception if we failed to open the file
  -> m a
withFileRecorder path columns action = do
  lock <- liftIO newLock
  let makeHandleRecorder = makeDefaultHandleRecorder columns lock
  fileHandle :: Either IOException Handle <- liftIO $ try (openFile path AppendMode)
  case fileHandle of
    Left e -> action $ Left e
    Right fileHandle -> finally (makeHandleRecorder fileHandle >>= action . Right) (liftIO $ hClose fileHandle)

makeDefaultHandleRecorder
  :: MonadIO m
  => Maybe [LoggingColumn]
  -- ^ built-in logging columns to display. Nothing uses the default
  -> Lock
  -- ^ lock to take when outputting to handle
  -> Handle
  -- ^ handle to output to
  -> m (Recorder (WithPriority (Doc a)))
makeDefaultHandleRecorder columns lock handle = do
  let Recorder{ logger_ } = textHandleRecorder handle
  let threadSafeRecorder = Recorder { logger_ = \msg -> liftIO $ withLock lock (logger_ msg) }
  let loggingColumns = fromMaybe defaultLoggingColumns columns
  let textWithPriorityRecorder = cmapIO (textWithPriorityToText loggingColumns) threadSafeRecorder
  pure (cmap docToText textWithPriorityRecorder)
  where
    docToText = fmap (renderStrict . layoutPretty defaultLayoutOptions)

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
  liftIO $ LSP.runLspT env $ LSP.sendNotification SMethod_WindowShowMessage
      ShowMessageParams
        { _type_ = priorityToLsp priority,
          _message = payload
        }

-- | Creates a recorder that sends logs to the LSP client via @window/logMessage@ notifications.
lspClientLogRecorder :: LanguageContextEnv config -> Recorder (WithPriority Text)
lspClientLogRecorder env = Recorder $ \WithPriority {..} ->
  liftIO $ LSP.runLspT env $ LSP.sendNotification SMethod_WindowLogMessage
      LogMessageParams
        { _type_ = priorityToLsp priority,
          _message = payload
        }

priorityToLsp :: Priority -> MessageType
priorityToLsp =
  \case
    Debug   -> MessageType_Log
    Info    -> MessageType_Info
    Warning -> MessageType_Warning
    Error   -> MessageType_Error

toCologActionWithPrio :: (MonadIO m, HasCallStack) => Recorder (WithPriority msg) -> LogAction m (WithSeverity msg)
toCologActionWithPrio (Recorder _logger) = LogAction $ \WithSeverity{..} -> do
    let priority = severityToPriority getSeverity
    _logger $ WithPriority priority callStack getMsg
  where
    severityToPriority :: Severity -> Priority
    severityToPriority Colog.Debug   = Debug
    severityToPriority Colog.Info    = Info
    severityToPriority Colog.Warning = Warning
    severityToPriority Colog.Error   = Error
