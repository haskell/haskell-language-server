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
    , InitializationContext (..)
    , untilMVar'
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
  | LogReactorThreadStopped Int
  | LogCancelledRequest !SomeLspId
  | LogSession Session.Log
  | LogLspServer LspServerLog
  | LogReactorShutdownRequested Bool
  | LogShutDownTimeout Int
  | LogServerExitWith (Either () Int)
  | LogReactorShutdownConfirmed !T.Text
  deriving Show

instance Pretty Log where
  pretty = \case
    LogReactorShutdownRequested b ->
      "Requested reactor shutdown; stop signal posted: " <+> pretty b
    LogReactorShutdownConfirmed msg ->
        "Reactor shutdown confirmed: " <+> pretty msg
    LogServerExitWith (Right 0) ->
      "Server exited successfully"
    LogServerExitWith (Right code) ->
      "Server exited with failure code" <+> pretty code
    LogServerExitWith (Left _) ->
      "Server forcefully exited due to exception in reactor thread"
    LogShutDownTimeout seconds ->
        "Shutdown timeout, the server will exit now after waiting for" <+> pretty seconds  <+> "seconds"
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
    LogReactorThreadStopped i ->
      "Reactor thread stopped" <+> pretty i
    LogCancelledRequest requestId ->
      "Cancelled request" <+> viaShow requestId
    LogSession msg -> pretty msg
    LogLspServer msg -> pretty msg

-- | Context for initializing the LSP language server.
-- This record encapsulates all the configuration and callback functions
-- needed to set up and run the language server initialization process.
data InitializationContext config = InitializationContext
  { ctxRecorder :: Recorder (WithPriority Log)
    -- ^ Logger for recording server events and diagnostics
  , ctxDefaultRoot :: FilePath
    -- ^ Default root directory for the workspace, see Note [Root Directory]
  , ctxGetHieDbLoc :: FilePath -> IO FilePath
    -- ^ Function to determine the HIE database location for a given root path
  , ctxGetIdeState :: LSP.LanguageContextEnv config -> FilePath -> WithHieDb -> ThreadQueue -> IO IdeState
    -- ^ Function to create and initialize the IDE state with the given environment
  , ctxUntilReactorStopSignal :: IO () -> IO ()
    -- ^ Lifetime control: MVar to signal reactor shutdown
  , ctxconfirmReactorShutdown :: T.Text -> IO ()
    -- ^ Callback to log/confirm reactor shutdown with a reason
  , ctxForceShutdown :: IO ()
    -- ^ Action to forcefully exit the server when exception occurs
  , ctxClearReqId :: SomeLspId -> IO ()
    -- ^ Function to clear/cancel a request by its ID
  , ctxWaitForCancel :: SomeLspId -> IO ()
    -- ^ Function to wait for a request cancellation by its ID
  , ctxClientMsgChan :: Chan ReactorMessage
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

    untilMVar' clientMsgVar runServer `finally` sequence_ onExit
        >>= logWith recorder Info . LogServerExitWith

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
  reactorStopSignal <- newEmptyMVar
  reactorConfirmBarrier <- newBarrier
  let
    untilReactorStopSignal = untilMVar reactorStopSignal
    confirmReactorShutdown reason = do
      logWith recorder Debug $ LogReactorShutdownConfirmed reason
      signalBarrier reactorConfirmBarrier ()
    requestReactorShutdown = do
      k <- tryPutMVar reactorStopSignal ()
      logWith recorder Info $ LogReactorShutdownRequested k
      let timeOutSeconds = 2
      timeout (timeOutSeconds * 1_000_000) (waitBarrier reactorConfirmBarrier) >>= \case
        Just () -> pure ()
        -- If we don't get confirmation within 2 seconds, we log a warning and shutdown anyway.
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
        , shutdownHandler recorder requestReactorShutdown
        ]
        -- Cancel requests are special since they need to be handled
        -- out of order to be useful. Existing handlers are run afterwards.

  let initParams = InitializationContext
        { ctxRecorder = recorder
        , ctxDefaultRoot = defaultRoot
        , ctxGetHieDbLoc = getHieDbLoc
        , ctxGetIdeState = getIdeState
        , ctxUntilReactorStopSignal = untilReactorStopSignal
        , ctxconfirmReactorShutdown = confirmReactorShutdown
        , ctxForceShutdown = exit
        , ctxClearReqId = clearReqId
        , ctxWaitForCancel = waitForCancel
        , ctxClientMsgChan = clientMsgChan
        }

  let doInitialize = handleInit initParams

  let interpretHandler (env,  st) = LSP.Iso (LSP.runLspT env . flip (runReaderT . unServerM) (clientMsgChan,st)) liftIO
  let onExit = [void $ tryPutMVar reactorStopSignal ()]

  pure MkSetup {doInitialize, staticHandlers, interpretHandler, onExit}


handleInit
    :: InitializationContext config
    -> LSP.LanguageContextEnv config -> TRequestMessage Method_Initialize -> IO (Either err (LSP.LanguageContextEnv config, IdeState))
handleInit initParams env (TRequestMessage _ _ m params) = otTracedHandler "Initialize" (show m) $ \sp -> do
    traceWithSpan sp params
  -- only shift if lsp root is different from the rootDir
  -- see Note [Root Directory]
    let
      recorder = ctxRecorder initParams
      defaultRoot = ctxDefaultRoot initParams
      untilReactorStopSignal = ctxUntilReactorStopSignal initParams
      lifetimeConfirm = ctxconfirmReactorShutdown initParams
    root <- case LSP.resRootPath env of
      Just lspRoot | lspRoot /= defaultRoot -> setCurrentDirectory lspRoot >> return lspRoot
      _ -> pure defaultRoot
    dbLoc <- ctxGetHieDbLoc initParams root
    let initConfig = parseConfiguration params
    logWith recorder Info $ LogRegisteringIdeConfig initConfig
    ideMVar <- newEmptyMVar

    let handleServerExceptionOrShutDown me = do
            -- shutdown shake
            readMVar ideMVar >>= \case
                ide -> shutdown ide
            case me of
                Left e -> do
                    lifetimeConfirm "due to exception in reactor thread or shutdown message"
                    logWith recorder Error $ LogReactorThreadException e
                    ctxForceShutdown initParams
                _ -> do
                    lifetimeConfirm "due to shutdown message"
                    return ()

        exceptionInHandler e = do
            logWith recorder Error $ LogReactorMessageActionException e

        checkCancelled :: forall m . LspId m -> IO () -> (TResponseError m -> IO ()) -> IO ()
        checkCancelled _id act k =
            let sid = SomeLspId _id
            in flip finally (ctxClearReqId initParams sid) $
                catch (do
                    -- We could optimize this by first checking if the id
                    -- is in the cancelled set. However, this is unlikely to be a
                    -- bottleneck and the additional check might hide
                    -- issues with async exceptions that need to be fixed.
                    cancelOrRes <- race (ctxWaitForCancel initParams sid) act
                    case cancelOrRes of
                        Left () -> do
                            logWith recorder Debug $ LogCancelledRequest sid
                            k $ TResponseError (InL LSPErrorCodes_RequestCancelled) "" Nothing
                        Right res -> pure res
                ) $ \(e :: SomeException) -> do
                    exceptionInHandler e
                    k $ TResponseError (InR ErrorCodes_InternalError) (T.pack $ show e) Nothing
    _ <- flip forkFinally handleServerExceptionOrShutDown $ do
            runWithWorkerThreads (cmapWithPrio LogSession recorder) dbLoc $ \withHieDb' threadQueue' ->
                do
                ide <- ctxGetIdeState initParams env root withHieDb' threadQueue'
                putMVar ideMVar ide
                -- We might be blocked indefinitly at initialization if reactorStop is signaled
                -- before we putMVar.
                untilReactorStopSignal $ forever $ do
                    msg <- readChan $ ctxClientMsgChan initParams
                    -- We dispatch notifications synchronously and requests asynchronously
                    -- This is to ensure that all file edits and config changes are applied before a request is handled
                    case msg of
                        ReactorNotification act -> handle exceptionInHandler act
                        ReactorRequest _id act k -> void $ async $ checkCancelled _id act k
                logWith recorder Info $ LogReactorThreadStopped 1
            logWith recorder Info $ LogReactorThreadStopped 2

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

untilMVar' :: MonadUnliftIO m => MVar a -> m b -> m (Either a b)
untilMVar' mvar io = race (readMVar mvar) io

cancelHandler :: (SomeLspId -> IO ()) -> LSP.Handlers (ServerM c)
cancelHandler cancelRequest = LSP.notificationHandler SMethod_CancelRequest $ \TNotificationMessage{_params=CancelParams{_id}} ->
  liftIO $ cancelRequest (SomeLspId (toLspId _id))
  where toLspId :: (Int32 |? T.Text) -> LspId a
        toLspId (InL x) = IdInt x
        toLspId (InR y) = IdString y

shutdownHandler :: Recorder (WithPriority Log) -> IO () -> LSP.Handlers (ServerM c)
shutdownHandler _recorder requestReactorShutdown = LSP.requestHandler SMethod_Shutdown $ \_ resp -> do
    -- stop the reactor to free up the hiedb connection and shut down shake
    liftIO requestReactorShutdown
    resp $ Right Null

modifyOptions :: LSP.Options -> LSP.Options
modifyOptions x = x{ LSP.optTextDocumentSync   = Just $ tweakTDS origTDS
                   }
    where
        tweakTDS tds = tds{_openClose=Just True, _change=Just TextDocumentSyncKind_Incremental, _save=Just $ InR $ SaveOptions Nothing}
        origTDS = fromMaybe tdsDefault $ LSP.optTextDocumentSync x
        tdsDefault = TextDocumentSyncOptions Nothing Nothing Nothing Nothing Nothing

