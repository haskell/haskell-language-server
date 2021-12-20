      -- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}

-- WARNING: A copy of DA.Daml.LanguageServer, try to keep them in sync
-- This version removes the daml: handling
module Development.IDE.LSP.LanguageServer
    ( runLanguageServer
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
import qualified Language.LSP.Server                   as LSP
import           Language.LSP.Types
import           System.IO
import           UnliftIO.Async
import           UnliftIO.Concurrent
import           UnliftIO.Directory
import           UnliftIO.Exception

import           Development.IDE.Core.FileStore
import           Development.IDE.Core.IdeConfiguration
import           Development.IDE.Core.Shake
import           Development.IDE.Core.Tracing
import           Development.IDE.LSP.HoverDefinition
import           Development.IDE.Types.Logger

import           Control.Monad.IO.Unlift               (MonadUnliftIO)
import           System.IO.Unsafe                      (unsafeInterleaveIO)

issueTrackerUrl :: T.Text
issueTrackerUrl = "https://github.com/haskell/haskell-language-server/issues"

runLanguageServer
    :: forall config. (Show config)
    => LSP.Options
    -> Handle -- input
    -> Handle -- output
    -> (FilePath -> IO FilePath) -- ^ Map root paths to the location of the hiedb for the project
    -> config
    -> (config -> Value -> Either T.Text config)
    -> LSP.Handlers (ServerM config)
    -> (LSP.LanguageContextEnv config -> VFSHandle -> Maybe FilePath -> HieDb -> IndexQueue -> IO IdeState)
    -> IO ()
runLanguageServer options inH outH getHieDbLoc defaultConfig onConfigurationChange userHandlers getIdeState = do

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

    let ideHandlers = mconcat
          [ setIdeHandlers
          , userHandlers
          ]

    -- Send everything over a channel, since you need to wait until after initialise before
    -- LspFuncs is available
    clientMsgChan :: Chan ReactorMessage <- newChan

    let asyncHandlers = mconcat
          [ ideHandlers
          , cancelHandler cancelRequest
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
            , LSP.interpretHandler = \(env, st) -> LSP.Iso (LSP.runLspT env . flip runReaderT (clientMsgChan,st)) liftIO
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
          -> LSP.LanguageContextEnv config -> RequestMessage Initialize -> IO (Either err (LSP.LanguageContextEnv config, IdeState))
        handleInit lifetime exitClientMsg clearReqId waitForCancel clientMsgChan env (RequestMessage _ _ m params) = otTracedHandler "Initialize" (show m) $ \sp -> do
            traceWithSpan sp params
            let root = LSP.resRootPath env
            dir <- maybe getCurrentDirectory return root
            dbLoc <- getHieDbLoc dir

            -- The database needs to be open for the duration of the reactor thread, but we need to pass in a reference
            -- to 'getIdeState', so we use this dirty trick
            dbMVar <- newEmptyMVar
            ~(hiedb,hieChan) <- unsafeInterleaveIO $ takeMVar dbMVar

            ide <- getIdeState env (makeLSPVFSHandle env) root hiedb hieChan

            let initConfig = parseConfiguration params
            logInfo (ideLogger ide) $ T.pack $ "Registering ide configuration: " <> show initConfig
            registerIdeConfiguration (shakeExtras ide) initConfig

            let handleServerException (Left e) = do
                    logError logger $
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
                    logError logger $ T.pack $
                        "Unexpected exception, please report!\n" ++
                        "Exception: " ++ show e
                    sendErrorMessage e

                logger = ideLogger ide

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
                                    logDebug (ideLogger ide) $ T.pack $ "Cancelled request " <> show _id
                                    k $ ResponseError RequestCancelled "" Nothing
                                Right res -> pure res
                        ) $ \(e :: SomeException) -> do
                            exceptionInHandler e
                            k $ ResponseError InternalError (T.pack $ show e) Nothing
            _ <- flip forkFinally handleServerException $ do
                untilMVar lifetime $ runWithDb logger dbLoc $ \hiedb hieChan -> do
                    putMVar dbMVar (hiedb,hieChan)
                    forever $ do
                        msg <- readChan clientMsgChan
                        -- We dispatch notifications synchronously and requests asynchronously
                        -- This is to ensure that all file edits and config changes are applied before a request is handled
                        case msg of
                            ReactorNotification act -> handle exceptionInHandler act
                            ReactorRequest _id act k -> void $ async $ checkCancelled _id act k
                logInfo logger "Reactor thread stopped"
            pure $ Right (env,ide)


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
    (_, ide) <- ask
    liftIO $ logDebug (ideLogger ide) "Received shutdown message"
    -- stop the reactor to free up the hiedb connection
    liftIO stopReactor
    -- flush out the Shake session to record a Shake profile if applicable
    liftIO $ shakeShut ide
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

