      -- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
-- WARNING: A copy of DA.Daml.LanguageServer, try to keep them in sync
-- This version removes the daml: handling
module Development.IDE.LSP.LanguageServer
    ( runLanguageServer
    , setupLSP
    , Log(..)
    ) where

import           Control.Concurrent.STM
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Data.Aeson                            (Value)
import           Data.Maybe
import qualified Data.Set                              as Set
import qualified Data.Text                             as T
import           Development.IDE.LSP.Server
import           Development.IDE.Session               (runWithDb)
import           Ide.Types                             (traceWithSpan)
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Server                   as LSP
import           System.IO
import           UnliftIO.Async
import           UnliftIO.Concurrent
import           UnliftIO.Directory
import           UnliftIO.Exception

import qualified Colog.Core                            as Colog
import           Control.Monad.IO.Unlift               (MonadUnliftIO)
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.Shake            hiding (Log, Priority)
import           Development.IDE.Core.Tracing
import qualified Development.IDE.Session               as Session
import           Development.IDE.Types.Shake           (WithHieDb)
import           Ide.Logger
import           Language.LSP.Server                   (LanguageContextEnv,
                                                        LspServerLog,
                                                        type (<~>))
data Log
  = LogRegisteringIdeConfig !IdeConfiguration
  | LogReactorThreadException !SomeException
  | LogReactorMessageActionException !SomeException
  | LogReactorThreadStopped
  | LogCancelledRequest !SomeLspId
  | LogSession Session.Log
  | LogLspServer LspServerLog
  | LogServerShutdownMessage
  deriving Show

instance Pretty Log where
  pretty = \case
    LogRegisteringIdeConfig ideConfig ->
      -- This log is also used to identify if HLS starts successfully in vscode-haskell,
      -- don't forget to update the corresponding test in vscode-haskell if the text in
      -- the next line has been modified.
      "Registering IDE configuration:" <+> viaShow ideConfig
    LogReactorThreadException e ->
      vcat
        [ "ReactorThreadException"
        , pretty $ displayException e ]
    LogReactorMessageActionException e ->
      vcat
        [ "ReactorMessageActionException"
        , pretty $ displayException e ]
    LogReactorThreadStopped ->
      "Reactor thread stopped"
    LogCancelledRequest requestId ->
      "Cancelled request" <+> viaShow requestId
    LogSession msg -> pretty msg
    LogLspServer msg -> pretty msg
    LogServerShutdownMessage -> "Received shutdown message"

-- used to smuggle RankNType WithHieDb through dbMVar
newtype WithHieDbShield = WithHieDbShield WithHieDb

runLanguageServer
    :: forall config a m. (Show config)
    => Recorder (WithPriority Log)
    -> LSP.Options
    -> Handle -- input
    -> Handle -- output
    -> config
    -> (config -> Value -> Either T.Text config)
    -> (config -> m config ())
    -> (MVar ()
        -> IO (LSP.LanguageContextEnv config -> TRequestMessage Method_Initialize -> IO (Either ResponseError (LSP.LanguageContextEnv config, a)),
               LSP.Handlers (m config),
               (LanguageContextEnv config, a) -> m config <~> IO))
    -> IO ()
runLanguageServer recorder options inH outH defaultConfig parseConfig onConfigChange setup = do
    -- This MVar becomes full when the server thread exits or we receive exit message from client.
    -- LSP server will be canceled when it's full.
    clientMsgVar <- newEmptyMVar

    (doInitialize, staticHandlers, interpretHandler) <- setup clientMsgVar

    let serverDefinition = LSP.ServerDefinition
            { LSP.parseConfig = parseConfig
            , LSP.onConfigChange = onConfigChange
            , LSP.defaultConfig = defaultConfig
            -- TODO: magic string
            , LSP.configSection = "haskell"
            , LSP.doInitialize = doInitialize
            , LSP.staticHandlers = const staticHandlers
            , LSP.interpretHandler = interpretHandler
            , LSP.options = modifyOptions options
            }

    let lspCologAction :: MonadIO m2 => Colog.LogAction m2 (Colog.WithSeverity LspServerLog)
        lspCologAction = toCologActionWithPrio (cmapWithPrio LogLspServer recorder)

    void $ untilMVar clientMsgVar $
          void $ LSP.runServerWithHandles
            lspCologAction
            lspCologAction
            inH
            outH
            serverDefinition

setupLSP ::
     forall config err.
     Recorder (WithPriority Log)
  -> (FilePath -> IO FilePath) -- ^ Map root paths to the location of the hiedb for the project
  -> LSP.Handlers (ServerM config)
  -> (LSP.LanguageContextEnv config -> Maybe FilePath -> WithHieDb -> IndexQueue -> IO IdeState)
  -> MVar ()
  -> IO (LSP.LanguageContextEnv config -> TRequestMessage Method_Initialize -> IO (Either err (LSP.LanguageContextEnv config, IdeState)),
         LSP.Handlers (ServerM config),
         (LanguageContextEnv config, IdeState) -> ServerM config <~> IO)
setupLSP  recorder getHieDbLoc userHandlers getIdeState clientMsgVar = do
  -- Send everything over a channel, since you need to wait until after initialise before
  -- LspFuncs is available
  clientMsgChan :: Chan ReactorMessage <- newChan

  -- An MVar to control the lifetime of the reactor loop.
  -- The loop will be stopped and resources freed when it's full
  reactorLifetime <- newEmptyMVar
  let stopReactorLoop = void $ tryPutMVar reactorLifetime ()

  -- Forcefully exit
  let exit = void $ tryPutMVar clientMsgVar ()

  -- The set of requests ids that we have received but not finished processing
  pendingRequests <- newTVarIO Set.empty
  -- The set of requests that have been cancelled and are also in pendingRequests
  cancelledRequests <- newTVarIO Set.empty

  let cancelRequest reqId = atomically $ do
          queued <- readTVar pendingRequests
          -- We want to avoid that the list of cancelled requests
          -- keeps growing if we receive cancellations for requests
          -- that do not exist or have already been processed.
          when (reqId `Set.member` queued) $
              modifyTVar cancelledRequests (Set.insert reqId)
  let clearReqId reqId = atomically $ do
          modifyTVar pendingRequests (Set.delete reqId)
          modifyTVar cancelledRequests (Set.delete reqId)
      -- We implement request cancellation by racing waitForCancel against
      -- the actual request handler.
  let waitForCancel reqId = atomically $ do
          cancelled <- readTVar cancelledRequests
          unless (reqId `Set.member` cancelled) retry

  let asyncHandlers = mconcat
        [ userHandlers
        , cancelHandler cancelRequest
        , exitHandler exit
        , shutdownHandler recorder stopReactorLoop
        ]
        -- Cancel requests are special since they need to be handled
        -- out of order to be useful. Existing handlers are run afterwards.

  let doInitialize = handleInit recorder getHieDbLoc getIdeState reactorLifetime exit clearReqId waitForCancel clientMsgChan

  let interpretHandler (env,  st) = LSP.Iso (LSP.runLspT env . flip (runReaderT . unServerM) (clientMsgChan,st)) liftIO

  pure (doInitialize, asyncHandlers, interpretHandler)


handleInit
    :: Recorder (WithPriority Log)
    -> (FilePath -> IO FilePath)
    -> (LSP.LanguageContextEnv config -> Maybe FilePath -> WithHieDb -> IndexQueue -> IO IdeState)
    -> MVar ()
    -> IO ()
    -> (SomeLspId -> IO ())
    -> (SomeLspId -> IO ())
    -> Chan ReactorMessage
    -> LSP.LanguageContextEnv config -> TRequestMessage Method_Initialize -> IO (Either err (LSP.LanguageContextEnv config, IdeState))
handleInit recorder getHieDbLoc getIdeState lifetime exitClientMsg clearReqId waitForCancel clientMsgChan env (TRequestMessage _ _ m params) = otTracedHandler "Initialize" (show m) $ \sp -> do
    traceWithSpan sp params
    let root = LSP.resRootPath env
    dir <- maybe getCurrentDirectory return root
    dbLoc <- getHieDbLoc dir
    let initConfig = parseConfiguration params
    logWith recorder Info $ LogRegisteringIdeConfig initConfig
    dbMVar <- newEmptyMVar


    let handleServerException (Left e) = do
            logWith recorder Error $ LogReactorThreadException e
            exitClientMsg
        handleServerException (Right _) = pure ()

        exceptionInHandler e = do
            logWith recorder Error $ LogReactorMessageActionException e

        checkCancelled _id act k =
            flip finally (clearReqId _id) $
                catch (do
                    -- We could optimize this by first checking if the id
                    -- is in the cancelled set. However, this is unlikely to be a
                    -- bottleneck and the additional check might hide
                    -- issues with async exceptions that need to be fixed.
                    cancelOrRes <- race (waitForCancel _id) act
                    case cancelOrRes of
                        Left () -> do
                            logWith recorder Debug $ LogCancelledRequest _id
                            k $ ResponseError (InL LSPErrorCodes_RequestCancelled) "" Nothing
                        Right res -> pure res
                ) $ \(e :: SomeException) -> do
                    exceptionInHandler e
                    k $ ResponseError (InR ErrorCodes_InternalError) (T.pack $ show e) Nothing
    _ <- flip forkFinally handleServerException $ do
        untilMVar lifetime $ runWithDb (cmapWithPrio LogSession recorder) dbLoc $ \withHieDb' hieChan' -> do
            putMVar dbMVar (WithHieDbShield withHieDb',hieChan')
            forever $ do
                msg <- readChan clientMsgChan
                -- We dispatch notifications synchronously and requests asynchronously
                -- This is to ensure that all file edits and config changes are applied before a request is handled
                case msg of
                    ReactorNotification act -> handle exceptionInHandler act
                    ReactorRequest _id act k -> void $ async $ checkCancelled _id act k
        logWith recorder Info LogReactorThreadStopped

    (WithHieDbShield withHieDb,hieChan) <- takeMVar dbMVar
    ide <- getIdeState env root withHieDb hieChan
    registerIdeConfiguration (shakeExtras ide) initConfig
    pure $ Right (env,ide)


-- | Runs the action until it ends or until the given MVar is put.
--   Rethrows any exceptions.
untilMVar :: MonadUnliftIO m => MVar () -> m () -> m ()
untilMVar mvar io = void $
    waitAnyCancel =<< traverse async [ io , readMVar mvar ]

cancelHandler :: (SomeLspId -> IO ()) -> LSP.Handlers (ServerM c)
cancelHandler cancelRequest = LSP.notificationHandler SMethod_CancelRequest $ \TNotificationMessage{_params=CancelParams{_id}} ->
  liftIO $ cancelRequest (SomeLspId (toLspId _id))
  where toLspId :: (Int32 |? T.Text) -> LspId a
        toLspId (InL x) = IdInt x
        toLspId (InR y) = IdString y

shutdownHandler :: Recorder (WithPriority Log) -> IO () -> LSP.Handlers (ServerM c)
shutdownHandler recorder stopReactor = LSP.requestHandler SMethod_Shutdown $ \_ resp -> do
    (_, ide) <- ask
    liftIO $ logWith recorder Debug LogServerShutdownMessage
    -- stop the reactor to free up the hiedb connection
    liftIO stopReactor
    -- flush out the Shake session to record a Shake profile if applicable
    liftIO $ shakeShut ide
    resp $ Right Null

exitHandler :: IO () -> LSP.Handlers (ServerM c)
exitHandler exit = LSP.notificationHandler SMethod_Exit $ const $ liftIO exit

modifyOptions :: LSP.Options -> LSP.Options
modifyOptions x = x{ LSP.optTextDocumentSync   = Just $ tweakTDS origTDS
                   }
    where
        tweakTDS tds = tds{_openClose=Just True, _change=Just TextDocumentSyncKind_Incremental, _save=Just $ InR $ SaveOptions Nothing}
        origTDS = fromMaybe tdsDefault $ LSP.optTextDocumentSync x
        tdsDefault = TextDocumentSyncOptions Nothing Nothing Nothing Nothing Nothing

