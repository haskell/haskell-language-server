      -- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

-- WARNING: A copy of DA.Daml.LanguageServer, try to keep them in sync
-- This version removes the daml: handling
module Development.IDE.LSP.LanguageServer
    ( runLanguageServer
    ) where

import           Language.LSP.Types
import           Development.IDE.LSP.Server
import qualified Development.IDE.GHC.Util as Ghcide
import qualified Language.LSP.Server as LSP
import Control.Concurrent.Extra (newBarrier, signalBarrier, waitBarrier)
import Control.Concurrent.STM
import Data.Maybe
import Data.Aeson (Value)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.IO.Handle (hDuplicate)
import System.IO
import Control.Monad.Extra
import UnliftIO.Exception
import UnliftIO.Async
import UnliftIO.Concurrent
import UnliftIO.Directory
import Control.Monad.IO.Class
import Control.Monad.Reader
import Ide.Types (traceWithSpan)
import Development.IDE.Session (runWithDb)

import Development.IDE.Core.IdeConfiguration
import Development.IDE.Core.Shake
import Development.IDE.LSP.HoverDefinition
import Development.IDE.LSP.Notifications
import Development.IDE.Types.Logger
import Development.IDE.Core.FileStore
import Development.IDE.Core.Tracing

import System.IO.Unsafe (unsafeInterleaveIO)

runLanguageServer
    :: forall config. (Show config)
    => LSP.Options
    -> (FilePath -> IO FilePath) -- ^ Map root paths to the location of the hiedb for the project
    -> (IdeState -> Value -> IO (Either T.Text config))
    -> LSP.Handlers (ServerM config)
    -> (LSP.LanguageContextEnv config -> VFSHandle -> Maybe FilePath -> HieDb -> IndexQueue -> IO IdeState)
    -> IO ()
runLanguageServer options getHieDbLoc onConfigurationChange userHandlers getIdeState = do
    -- Move stdout to another file descriptor and duplicate stderr
    -- to stdout. This guards against stray prints from corrupting the JSON-RPC
    -- message stream.
    newStdout <- hDuplicate stdout
    stderr `Ghcide.hDuplicateTo'` stdout
    hSetBuffering stderr NoBuffering
    hSetBuffering stdout NoBuffering

    -- Print out a single space to assert that the above redirection works.
    -- This is interleaved with the logger, hence we just print a space here in
    -- order not to mess up the output too much. Verified that this breaks
    -- the language server tests without the redirection.
    putStr " " >> hFlush stdout

    -- These barriers are signaled when the threads reading from these chans exit.
    -- This should not happen but if it does, we will make sure that the whole server
    -- dies and can be restarted instead of losing threads silently.
    clientMsgBarrier <- newBarrier
    -- Forcefully exit
    let exit = signalBarrier clientMsgBarrier ()

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
          , setHandlersNotifications -- absolutely critical, join them with user notifications
          ]

    -- Send everything over a channel, since you need to wait until after initialise before
    -- LspFuncs is available
    clientMsgChan :: Chan ReactorMessage <- newChan

    let asyncHandlers = mconcat
          [ ideHandlers
          , cancelHandler cancelRequest
          , exitHandler exit
          ]
          -- Cancel requests are special since they need to be handled
          -- out of order to be useful. Existing handlers are run afterwards.


    let serverDefinition = LSP.ServerDefinition
            { LSP.onConfigurationChange = \v -> do
                (_chan, ide) <- ask
                liftIO $ onConfigurationChange ide v
            , LSP.doInitialize = handleInit exit clearReqId waitForCancel clientMsgChan
            , LSP.staticHandlers = asyncHandlers
            , LSP.interpretHandler = \(env, st) -> LSP.Iso (LSP.runLspT env . flip runReaderT (clientMsgChan,st)) liftIO
            , LSP.options = modifyOptions options
            }

    void $ waitAnyCancel =<< traverse async
        [ void $ LSP.runServerWithHandles
            stdin
            newStdout
            serverDefinition
        , void $ waitBarrier clientMsgBarrier
        ]

    where
        handleInit
          :: IO () -> (SomeLspId -> IO ()) -> (SomeLspId -> IO ()) -> Chan ReactorMessage
          -> LSP.LanguageContextEnv config -> RequestMessage Initialize -> IO (Either err (LSP.LanguageContextEnv config, IdeState))
        handleInit exitClientMsg clearReqId waitForCancel clientMsgChan env (RequestMessage _ _ m params) = otTracedHandler "Initialize" (show m) $ \sp -> do
            traceWithSpan sp params
            let root = LSP.resRootPath env

            dir <- getCurrentDirectory
            dbLoc <- getHieDbLoc dir

            -- The database needs to be open for the duration of the reactor thread, but we need to pass in a reference
            -- to 'getIdeState', so we use this dirty trick
            dbMVar <- newEmptyMVar
            ~(hiedb,hieChan) <- unsafeInterleaveIO $ takeMVar dbMVar

            ide <- getIdeState env (makeLSPVFSHandle env) root hiedb hieChan

            let initConfig = parseConfiguration params
            logInfo (ideLogger ide) $ T.pack $ "Registering ide configuration: " <> show initConfig
            registerIdeConfiguration (shakeExtras ide) initConfig

            _ <- flip forkFinally (const exitClientMsg) $ runWithDb dbLoc $ \hiedb hieChan -> do
              putMVar dbMVar (hiedb,hieChan)
              forever $ do
                msg <- readChan clientMsgChan
                -- We dispatch notifications synchronously and requests asynchronously
                -- This is to ensure that all file edits and config changes are applied before a request is handled
                case msg of
                    ReactorNotification act -> do
                      catch act $ \(e :: SomeException) ->
                        logError (ideLogger ide) $ T.pack $
                          "Unexpected exception on notification, please report!\n" ++
                          "Exception: " ++ show e
                    ReactorRequest _id act k -> void $ async $
                      checkCancelled ide clearReqId waitForCancel _id act k
            pure $ Right (env,ide)

        checkCancelled
          :: IdeState -> (SomeLspId -> IO ()) -> (SomeLspId -> IO ()) -> SomeLspId
          -> IO () -> (ResponseError -> IO ()) -> IO ()
        checkCancelled ide clearReqId waitForCancel _id act k =
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
                    logError (ideLogger ide) $ T.pack $
                        "Unexpected exception on request, please report!\n" ++
                        "Exception: " ++ show e
                    k $ ResponseError InternalError (T.pack $ show e) Nothing


cancelHandler :: (SomeLspId -> IO ()) -> LSP.Handlers (ServerM c)
cancelHandler cancelRequest = LSP.notificationHandler SCancelRequest $ \NotificationMessage{_params=CancelParams{_id}} ->
  liftIO $ cancelRequest (SomeLspId _id)

exitHandler :: IO () -> LSP.Handlers (ServerM c)
exitHandler exit = LSP.notificationHandler SExit (const $ liftIO exit)

modifyOptions :: LSP.Options -> LSP.Options
modifyOptions x = x{ LSP.textDocumentSync   = Just $ tweakTDS origTDS
                   }
    where
        tweakTDS tds = tds{_openClose=Just True, _change=Just TdSyncIncremental, _save=Just $ InR $ SaveOptions Nothing}
        origTDS = fromMaybe tdsDefault $ LSP.textDocumentSync x
        tdsDefault = TextDocumentSyncOptions Nothing Nothing Nothing Nothing Nothing

