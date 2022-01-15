{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module WrapperLspMain where

import           Control.Concurrent.STM                (retry)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Reader
import           Data.Aeson
import           Data.Default
import           Data.Maybe
import qualified Data.Set                              as Set
import qualified Data.Text                             as T
import           Development.IDE.Core.IdeConfiguration (parseConfiguration)
import           Development.IDE.LSP.Server            (ReactorChan,
                                                        ReactorMessage (ReactorNotification, ReactorRequest))
import qualified Development.IDE.Main                  as Main
import           Development.IDE.Types.Logger
import qualified Development.IDE.Types.Logger          as G
import qualified Development.IDE.Types.Options         as Ghcide
import qualified Ide.Arguments                         as IdeArgs
import           Ide.Logger
import           Ide.Plugin.Config
import           Language.LSP.Server
import qualified Language.LSP.Server                   as LSP
import           Language.LSP.Types
import qualified System.Directory                      as IO
import           System.IO
import qualified System.Log                            as L
import           UnliftIO
import           UnliftIO.Concurrent

type ServerM c = ReaderT ReactorChan (LspM c)

lspMain :: IdeArgs.GhcideArguments -> T.Text -> IO ()
lspMain ghcideArgs@IdeArgs.GhcideArguments{..} msg = do
    whenJust argsCwd IO.setCurrentDirectory
    dir <- IO.getCurrentDirectory
    LSP.setupLogger argsLogFile ["hls"]
      $ if argsDebugOn then L.DEBUG else L.INFO

    when (Main.isLSP argsCommand) $ do
        hPutStrLn stderr "Starting (haskell-language-server-wrapper)LSP server..."
        hPutStrLn stderr $ "  with arguments: " <> show ghcideArgs
        hPutStrLn stderr $ "  in directory: " <> dir

    let realArguments = def
          { Main.argCommand = argsCommand
          , Main.argsLogger = pure hlsWrapperLogger
          , Main.argsThreads = if argsThreads == 0 then Nothing else Just $ fromIntegral argsThreads
          , Main.argsIdeOptions = \_config sessionLoader ->
            let defOptions = Ghcide.defaultIdeOptions sessionLoader
            in defOptions
                { Ghcide.optShakeProfiling = argsShakeProfiling
                , Ghcide.optTesting = Ghcide.IdeTesting argsTesting
                }
          }


    inH <- Main.argsHandleIn realArguments
    outH <- Main.argsHandleOut realArguments

    runLanguageServer (Main.argsLspOptions realArguments) inH outH (Main.argsDefaultHlsConfig realArguments) getConfigFromNotification $ \env exitFun -> do
        -- Send a message to the client to tell about our problems
        void $ LSP.runLspT env $ LSP.sendRequest SWindowShowMessageRequest
            (ShowMessageRequestParams MtError msg (Just [MessageActionItem restartTitle])) $ \case
                Right (Just (MessageActionItem title))
                    | title == restartTitle -> liftIO exitFun
                _ -> pure ()
    where
        restartTitle = "Try to restart"

-- LSP Helper functions
runLanguageServer
    :: LSP.Options
    -> Handle -- input
    -> Handle -- output
    -> Config
    -> (Config -> Value -> Either T.Text Config)
    -> (LSP.LanguageContextEnv Config -> IO () -> IO ())
    -> IO ()
runLanguageServer options inH outH defaultConfig onConfigurationChange onRun = do

    -- This MVar becomes full when the server thread exits or we receive exit message from client.
    -- LSP server will be canceled when it's full.
    clientMsgVar <- newEmptyMVar
    -- Forcefully exit
    let exit = void $ tryPutMVar clientMsgVar ()

    -- An MVar to control the lifetime of the reactor loop.
    -- The loop will be stopped and resources freed when it's full
    reactorLifetime <- newEmptyMVar
    let stopReactorLoop = void $ tryPutMVar reactorLifetime ()

    -- The set of requests ids that we have received but not finished processing
    pendingRequests <- newTVarIO Set.empty
    -- The set of requests that have been cancelled and are also in pendingRequests
    cancelledRequests <- newTVarIO Set.empty

    let cancelRequest reqId = atomically $ do
            queued <- readTVar pendingRequests
            -- We want to avoid that the list of cancelled requests
            -- keeps growing if we receive cancellations for requests
            -- that do not exist or have already been processed.
            when (reqId `elem` queued) $
                modifyTVar cancelledRequests (Set.insert reqId)
    let clearReqId reqId = atomically $ do
            modifyTVar pendingRequests (Set.delete reqId)
            modifyTVar cancelledRequests (Set.delete reqId)
        -- We implement request cancellation by racing waitForCancel against
        -- the actual request handler.
    let waitForCancel reqId = atomically $ do
            cancelled <- readTVar cancelledRequests
            unless (reqId `Set.member` cancelled) retry

    -- Send everything over a channel, since you need to wait until after initialise before
    -- LspFuncs is available
    clientMsgChan :: Chan ReactorMessage <- newChan

    let asyncHandlers = mconcat
          [ cancelHandler cancelRequest
          , exitHandler exit
          , shutdownHandler stopReactorLoop
          ]
          -- Cancel requests are special since they need to be handled
          -- out of order to be useful. Existing handlers are run afterwards.


    let serverDefinition = LSP.ServerDefinition
            { LSP.onConfigurationChange = onConfigurationChange
            , LSP.defaultConfig = defaultConfig
            , LSP.doInitialize = handleInit reactorLifetime exit clearReqId waitForCancel clientMsgChan
            , LSP.staticHandlers = asyncHandlers
            , LSP.interpretHandler = \env -> LSP.Iso (LSP.runLspT env . flip runReaderT clientMsgChan) liftIO
            , LSP.options = modifyOptions options
            }

    void $ untilMVar clientMsgVar $
          void $ LSP.runServerWithHandles
            inH
            outH
            serverDefinition

    where
        handleInit
          :: MVar () -> IO () -> (SomeLspId -> IO ()) -> (SomeLspId -> IO ()) -> Chan ReactorMessage
          -> LSP.LanguageContextEnv Config -> RequestMessage Initialize -> IO (Either err (LSP.LanguageContextEnv Config))
        handleInit lifetime exitClientMsg clearReqId waitForCancel clientMsgChan env (RequestMessage _ _ _ params) = do
            let initConfig = parseConfiguration params
            logInfo hlsWrapperLogger $ T.pack $ "Registering ide configuration: " <> show initConfig

            let handleServerException (Left e) = do
                    logError hlsWrapperLogger $
                        T.pack $ "Fatal error in server thread: " <> show e
                    sendErrorMessage e
                    exitClientMsg
                handleServerException (Right _) = pure ()

                sendErrorMessage (e :: SomeException) = do
                    LSP.runLspT env $ LSP.sendNotification SWindowShowMessage $
                        ShowMessageParams MtError $ T.unlines
                        [ "Unhandled exception, please [report](" <> issueTrackerUrl <> "): "
                        , T.pack(show e)
                        ]

                exceptionInHandler e = do
                    logError hlsWrapperLogger $ T.pack $
                        "Unexpected exception, please report!\n" ++
                        "Exception: " ++ show e
                    sendErrorMessage e

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
                                    logDebug hlsWrapperLogger $ T.pack $ "Cancelled request " <> show _id
                                    k $ ResponseError RequestCancelled "" Nothing
                                Right res -> pure res
                        ) $ \(e :: SomeException) -> do
                            exceptionInHandler e
                            k $ ResponseError InternalError (T.pack $ show e) Nothing
            _ <- flip forkFinally handleServerException $ do
                untilMVar lifetime $ do
                    forever $ do
                        msg <- readChan clientMsgChan
                        -- We dispatch notifications synchronously and requests asynchronously
                        -- This is to ensure that all file edits and config changes are applied before a request is handled
                        case msg of
                            ReactorNotification act -> handle exceptionInHandler act
                            ReactorRequest _id act k -> void $ async $ checkCancelled _id act k
                logInfo hlsWrapperLogger "Reactor thread stopped"

            onRun env exitClientMsg
            pure $ Right env

-- | Runs the action until it ends or until the given MVar is put.
--   Rethrows any exceptions.
untilMVar :: MonadUnliftIO m => MVar () -> m () -> m ()
untilMVar mvar io = void $
    waitAnyCancel =<< traverse async [ io , readMVar mvar ]

cancelHandler :: (SomeLspId -> IO ()) -> LSP.Handlers (ServerM c)
cancelHandler cancelRequest = LSP.notificationHandler SCancelRequest $ \NotificationMessage{_params=CancelParams{_id}} ->
  liftIO $ cancelRequest (SomeLspId _id)

shutdownHandler :: IO () -> LSP.Handlers (ServerM c)
shutdownHandler stopReactor = LSP.requestHandler SShutdown $ \_ resp -> do
    liftIO $ logDebug hlsWrapperLogger "Received shutdown message"
    -- stop the reactor to free up the hiedb connection
    liftIO stopReactor
    -- flush out the Shake session to record a Shake profile if applicable
    resp $ Right Empty

exitHandler :: IO () -> LSP.Handlers (ServerM c)
exitHandler exit = LSP.notificationHandler SExit $ const $ liftIO exit

modifyOptions :: LSP.Options -> LSP.Options
modifyOptions x = x{ LSP.textDocumentSync   = Just $ tweakTDS origTDS
                   }
    where
        tweakTDS tds = tds{_openClose=Just True, _change=Just TdSyncIncremental, _save=Just $ InR $ SaveOptions Nothing}
        origTDS = fromMaybe tdsDefault $ LSP.textDocumentSync x
        tdsDefault = TextDocumentSyncOptions Nothing Nothing Nothing Nothing Nothing

issueTrackerUrl :: T.Text
issueTrackerUrl = "https://github.com/haskell/haskell-language-server/issues"

hlsWrapperLogger :: G.Logger
hlsWrapperLogger = G.Logger $ \pri txt ->
    case pri of
      G.Telemetry -> logm     (T.unpack txt)
      G.Debug     -> debugm   (T.unpack txt)
      G.Info      -> logm     (T.unpack txt)
      G.Warning   -> warningm (T.unpack txt)
      G.Error     -> errorm   (T.unpack txt)
