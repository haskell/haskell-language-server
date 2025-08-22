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
    , ThreadQueue
    , runWithWorkerThreads
    , Setup (..)
    , InitParameters (..)
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
import           Control.Concurrent.Extra              (newBarrier,
                                                        signalBarrier,
                                                        waitBarrier)
import           Control.Monad.IO.Unlift               (MonadUnliftIO)
import           Control.Monad.Trans.Cont              (evalContT)
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.Service          (shutdown)
import           Development.IDE.Core.Shake            hiding (Log)
import           Development.IDE.Core.Tracing
import           Development.IDE.Core.WorkerThread     (withWorkerQueue)
import qualified Development.IDE.Session               as Session
import           Development.IDE.Types.Shake           (WithHieDb,
                                                        WithHieDbShield (..))
import           Ide.Logger
import           Language.LSP.Server                   (LanguageContextEnv,
                                                        LspServerLog,
                                                        type (<~>))
import           System.Timeout                        (timeout)
data Log
  = LogRegisteringIdeConfig !IdeConfiguration
  | LogReactorThreadException !SomeException
  | LogReactorMessageActionException !SomeException
  | LogReactorThreadStopped
  | LogCancelledRequest !SomeLspId
  | LogSession Session.Log
  | LogLspServer LspServerLog
  | LogServerShutdownMessage
  | LogShutDownTimeout Int
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShutDownTimeout seconds ->
        "Shutdown timeout, the server will exit now after waiting for" <+> pretty seconds <+> "milliseconds"
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

-- | Parameters for initializing the LSP language server.
-- This record encapsulates all the configuration and callback functions
-- needed to set up and run the language server initialization process.
data InitParameters config = InitParameters
  { initRecorder :: Recorder (WithPriority Log)
    -- ^ Logger for recording server events and diagnostics
  , initDefaultRoot :: FilePath
    -- ^ Default root directory for the workspace, see Note [Root Directory]
  , initGetHieDbLoc :: FilePath -> IO FilePath
    -- ^ Function to determine the HIE database location for a given root path
  , initGetIdeState :: LSP.LanguageContextEnv config -> FilePath -> WithHieDb -> ThreadQueue -> IO IdeState
    -- ^ Function to create and initialize the IDE state with the given environment
  , initLifetime :: (MVar (), IO ())
    -- ^ Lifetime control: MVar to signal shutdown and confirmation action
  , initForceShutdown :: IO ()
    -- ^ Action to forcefully exit the server when exception occurs
  , initClearReqId :: SomeLspId -> IO ()
    -- ^ Function to clear/cancel a request by its ID
  , initWaitForCancel :: SomeLspId -> IO ()
    -- ^ Function to wait for a request cancellation by its ID
  , initClientMsgChan :: Chan ReactorMessage
    -- ^ Channel for communicating with the reactor message loop
  }

data Setup config m a
  = MkSetup
  { doInitialize :: LSP.LanguageContextEnv config -> TRequestMessage Method_Initialize -> IO (Either (TResponseError Method_Initialize) (LSP.LanguageContextEnv config, a))
  -- ^ the callback invoked when the language server receives the 'Method_Initialize' request
  , staticHandlers :: LSP.Handlers m
  -- ^ the statically known handlers of the lsp server
  , interpretHandler :: (LanguageContextEnv config, a) -> m <~> IO
  -- ^ how to interpret @m@ to 'IO' and how to lift 'IO' into @m@
  , onExit :: [IO ()]
  -- ^ a list of 'IO' actions that clean up resources and must be run when the server shuts down
  }

runLanguageServer
    :: forall config a m. (Show config)
    => Recorder (WithPriority Log)
    -> LSP.Options
    -> Handle -- input
    -> Handle -- output
    -> config
    -> (config -> Value -> Either T.Text config)
    -> (config -> m ())
    -> (MVar () -> IO (Setup config m a))
    -> IO ()
runLanguageServer recorder options inH outH defaultConfig parseConfig onConfigChange setup = do
    -- This MVar becomes full when the server thread exits or we receive exit message from client.
    -- LSP server will be canceled when it's full.
    clientMsgVar <- newEmptyMVar

    MkSetup
      { doInitialize, staticHandlers, interpretHandler, onExit } <- setup clientMsgVar

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

    let lspCologAction :: forall io. MonadIO io => Colog.LogAction io (Colog.WithSeverity LspServerLog)
        lspCologAction = toCologActionWithPrio (cmapWithPrio LogLspServer recorder)

    let runServer =
          LSP.runServerWithHandles
            lspCologAction
            lspCologAction
            inH
            outH
            serverDefinition

    void $ runServer `finally` sequence_ onExit

setupLSP ::
     forall config.
     Recorder (WithPriority Log)
  -> FilePath -- ^ root directory, see Note [Root Directory]
  -> (FilePath -> IO FilePath) -- ^ Map root paths to the location of the hiedb for the project
  -> LSP.Handlers (ServerM config)
  -> (LSP.LanguageContextEnv config -> FilePath -> WithHieDb -> ThreadQueue -> IO IdeState)
  -> MVar ()
  -> IO (Setup config (ServerM config) IdeState)
setupLSP recorder defaultRoot getHieDbLoc userHandlers getIdeState clientMsgVar = do
  -- Send everything over a channel, since you need to wait until after initialise before
  -- LspFuncs is available
  clientMsgChan :: Chan ReactorMessage <- newChan

  -- An MVar to control the lifetime of the reactor loop.
  -- The loop will be stopped and resources freed when it's full
  reactorLifetime <- newEmptyMVar
  reactorLifetimeConfirmBarrier <- newBarrier
  let stopReactorLoopConfirm =
        signalBarrier reactorLifetimeConfirmBarrier ()
  let stopReactorLoop = do
        _ <- tryPutMVar reactorLifetime ()
        let timeOutSeconds = 3 * 1_000_000
        timeout timeOutSeconds (waitBarrier reactorLifetimeConfirmBarrier) >>= \case
            Just () -> pure ()
            -- If we don't get confirmation within 3 seconds, we log a warning and shutdown anyway.
            -- This is to avoid deadlocks in case the client does not respond to shutdown requests.
            Nothing -> logWith recorder Warning $ LogShutDownTimeout timeOutSeconds

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

  let staticHandlers = mconcat
        [ userHandlers
        , cancelHandler cancelRequest
        , shutdownHandler recorder stopReactorLoop
        ]
        -- Cancel requests are special since they need to be handled
        -- out of order to be useful. Existing handlers are run afterwards.

  let initParams = InitParameters
        { initRecorder = recorder
        , initDefaultRoot = defaultRoot
        , initGetHieDbLoc = getHieDbLoc
        , initGetIdeState = getIdeState
        , initLifetime = (reactorLifetime, stopReactorLoopConfirm)
        , initForceShutdown = exit
        , initClearReqId = clearReqId
        , initWaitForCancel = waitForCancel
        , initClientMsgChan = clientMsgChan
        }

  let doInitialize = handleInit initParams

  let interpretHandler (env,  st) = LSP.Iso (LSP.runLspT env . flip (runReaderT . unServerM) (clientMsgChan,st)) liftIO

  let onExit = [stopReactorLoop, exit]

  pure MkSetup {doInitialize, staticHandlers, interpretHandler, onExit}


handleInit
    :: InitParameters config
    -> LSP.LanguageContextEnv config -> TRequestMessage Method_Initialize -> IO (Either err (LSP.LanguageContextEnv config, IdeState))
handleInit initParams env (TRequestMessage _ _ m params) = otTracedHandler "Initialize" (show m) $ \sp -> do
    traceWithSpan sp params
    -- only shift if lsp root is different from the rootDir
    -- see Note [Root Directory]
    let recorder = initRecorder initParams
        defaultRoot = initDefaultRoot initParams
        (lifetime, lifetimeConfirm) = initLifetime initParams
    root <- case LSP.resRootPath env of
        Just lspRoot | lspRoot /= defaultRoot -> setCurrentDirectory lspRoot >> return lspRoot
        _ -> pure defaultRoot
    dbLoc <- initGetHieDbLoc initParams root
    let initConfig = parseConfiguration params
    logWith recorder Info $ LogRegisteringIdeConfig initConfig
    ideMVar <- newEmptyMVar

    let handleServerExceptionOrShutDown me = do
            -- try to shutdown shake
            tryReadMVar ideMVar >>= \case
                Nothing -> return ()
                Just ide -> shutdown ide
            lifetimeConfirm
            case me of
                Left e -> do
                    logWith recorder Error $ LogReactorThreadException e
                    initForceShutdown initParams
                _ -> return ()

        exceptionInHandler e = do
            logWith recorder Error $ LogReactorMessageActionException e

        checkCancelled :: forall m . LspId m -> IO () -> (TResponseError m -> IO ()) -> IO ()
        checkCancelled _id act k =
            let sid = SomeLspId _id
            in flip finally (initClearReqId initParams sid) $
                catch (do
                    -- We could optimize this by first checking if the id
                    -- is in the cancelled set. However, this is unlikely to be a
                    -- bottleneck and the additional check might hide
                    -- issues with async exceptions that need to be fixed.
                    cancelOrRes <- race (initWaitForCancel initParams sid) act
                    case cancelOrRes of
                        Left () -> do
                            logWith recorder Debug $ LogCancelledRequest sid
                            k $ TResponseError (InL LSPErrorCodes_RequestCancelled) "" Nothing
                        Right res -> pure res
                ) $ \(e :: SomeException) -> do
                    exceptionInHandler e
                    k $ TResponseError (InR ErrorCodes_InternalError) (T.pack $ show e) Nothing
    _ <- flip forkFinally handleServerExceptionOrShutDown $ do
        untilMVar lifetime $ runWithWorkerThreads (cmapWithPrio LogSession recorder) dbLoc $ \withHieDb' threadQueue' -> do
            ide <- initGetIdeState initParams env root withHieDb' threadQueue'
            putMVar ideMVar ide
            forever $ do
                msg <- readChan $ initClientMsgChan initParams
                -- We dispatch notifications synchronously and requests asynchronously
                -- This is to ensure that all file edits and config changes are applied before a request is handled
                case msg of
                    ReactorNotification act -> handle exceptionInHandler act
                    ReactorRequest _id act k -> void $ async $ checkCancelled _id act k
        logWith recorder Info LogReactorThreadStopped

    ide <- readMVar ideMVar
    registerIdeConfiguration (shakeExtras ide) initConfig
    pure $ Right (env,ide)


-- | runWithWorkerThreads
-- create several threads to run the session, db and session loader
-- see Note [Serializing runs in separate thread]
runWithWorkerThreads :: Recorder (WithPriority Session.Log) -> FilePath -> (WithHieDb -> ThreadQueue -> IO ()) -> IO ()
runWithWorkerThreads recorder dbLoc f = evalContT $ do
            sessionRestartTQueue <- withWorkerQueue id
            sessionLoaderTQueue <- withWorkerQueue id
            (WithHieDbShield hiedb, threadQueue) <- runWithDb recorder dbLoc
            liftIO $ f hiedb (ThreadQueue threadQueue sessionRestartTQueue sessionLoaderTQueue)

-- | Runs the action until it ends or until the given MVar is put.
--   It is important, that the thread that puts the 'MVar' is not dropped before it puts the 'MVar' i.e. it should
--   occur as the final action in a 'finally' or 'bracket', because otherwise this thread will finish early (as soon
--   as the thread receives the BlockedIndefinitelyOnMVar exception)
--   Rethrows any exceptions.
untilMVar :: MonadUnliftIO m => MVar () -> m a -> m ()
untilMVar mvar io = race_ (readMVar mvar) io

cancelHandler :: (SomeLspId -> IO ()) -> LSP.Handlers (ServerM c)
cancelHandler cancelRequest = LSP.notificationHandler SMethod_CancelRequest $ \TNotificationMessage{_params=CancelParams{_id}} ->
  liftIO $ cancelRequest (SomeLspId (toLspId _id))
  where toLspId :: (Int32 |? T.Text) -> LspId a
        toLspId (InL x) = IdInt x
        toLspId (InR y) = IdString y

shutdownHandler :: Recorder (WithPriority Log) -> IO () -> LSP.Handlers (ServerM c)
shutdownHandler recorder stopReactor = LSP.requestHandler SMethod_Shutdown $ \_ resp -> do
    liftIO $ logWith recorder Debug LogServerShutdownMessage
    -- stop the reactor to free up the hiedb connection and shut down shake
    liftIO stopReactor
    resp $ Right Null

modifyOptions :: LSP.Options -> LSP.Options
modifyOptions x = x{ LSP.optTextDocumentSync   = Just $ tweakTDS origTDS
                   }
    where
        tweakTDS tds = tds{_openClose=Just True, _change=Just TextDocumentSyncKind_Incremental, _save=Just $ InR $ SaveOptions Nothing}
        origTDS = fromMaybe tdsDefault $ LSP.optTextDocumentSync x
        tdsDefault = TextDocumentSyncOptions Nothing Nothing Nothing Nothing Nothing

