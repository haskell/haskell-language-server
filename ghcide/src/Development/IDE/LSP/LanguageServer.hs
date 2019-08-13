-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE ExistentialQuantification  #-}

-- WARNING: A copy of DA.Daml.LanguageServer, try to keep them in sync
-- This version removes the daml: handling
module Development.IDE.LSP.LanguageServer
    ( runLanguageServer
    ) where

import           Language.Haskell.LSP.Types
import           Language.Haskell.LSP.Types.Capabilities
import           Development.IDE.LSP.Server
import qualified Language.Haskell.LSP.Control as LSP
import qualified Language.Haskell.LSP.Core as LSP
import Control.Concurrent.Chan
import Control.Concurrent.Extra
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe
import Data.Default
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Text as T
import           GHC.IO.Handle                    (hDuplicate, hDuplicateTo)
import System.IO
import Control.Monad.Extra

import Development.IDE.LSP.Definition
import Development.IDE.LSP.Hover
import Development.IDE.LSP.CodeAction
import Development.IDE.LSP.Notifications
import Development.IDE.Core.Service
import Development.IDE.Types.Logger
import Development.IDE.Core.FileStore
import Language.Haskell.LSP.Core (LspFuncs(..))
import Language.Haskell.LSP.Messages


runLanguageServer
    :: LSP.Options
    -> PartialHandlers
    -> ((FromServerMessage -> IO ()) -> VFSHandle -> ClientCapabilities -> IO IdeState)
    -> IO ()
runLanguageServer options userHandlers getIdeState = do
    -- Move stdout to another file descriptor and duplicate stderr
    -- to stdout. This guards against stray prints from corrupting the JSON-RPC
    -- message stream.
    newStdout <- hDuplicate stdout
    stderr `hDuplicateTo` stdout
    hSetBuffering stderr NoBuffering
    hSetBuffering stdout NoBuffering

    -- Print out a single space to assert that the above redirection works.
    -- This is interleaved with the logger, hence we just print a space here in
    -- order not to mess up the output too much. Verified that this breaks
    -- the language server tests without the redirection.
    putStr " " >> hFlush stdout

    -- Send everything over a channel, since you need to wait until after initialise before
    -- LspFuncs is available
    clientMsgChan :: Chan Message <- newChan

    -- These barriers are signaled when the threads reading from these chans exit.
    -- This should not happen but if it does, we will make sure that the whole server
    -- dies and can be restarted instead of losing threads silently.
    clientMsgBarrier <- newBarrier

    -- The set of requests ids that we have received but not finished processing
    pendingRequests <- newTVarIO Set.empty
    -- The set of requests that have been cancelled and are also in pendingRequests
    cancelledRequests <- newTVarIO Set.empty

    let withResponse wrap f = Just $ \r@RequestMessage{_id} -> do
            atomically $ modifyTVar pendingRequests (Set.insert _id)
            writeChan clientMsgChan $ Response r wrap f
    let withNotification old f = Just $ \r -> writeChan clientMsgChan $ Notification r (\lsp ide x -> f lsp ide x >> whenJust old ($ r))
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
    let PartialHandlers parts =
            setHandlersIgnore <> -- least important
            setHandlersDefinition <> setHandlersHover <> setHandlersCodeAction <> -- useful features someone may override
            userHandlers <>
            setHandlersNotifications <> -- absolutely critical, join them with user notifications
            cancelHandler cancelRequest
            -- Cancel requests are special since they need to be handled
            -- out of order to be useful. Existing handlers are run afterwards.
    handlers <- parts WithMessage{withResponse, withNotification} def

    let initializeCallbacks = LSP.InitializeCallbacks
            { LSP.onInitialConfiguration = const $ Right ()
            , LSP.onConfigurationChange = const $ Right ()
            , LSP.onStartup = handleInit (signalBarrier clientMsgBarrier ()) clearReqId waitForCancel clientMsgChan
            }

    void $ waitAnyCancel =<< traverse async
        [ void $ LSP.runWithHandles
            stdin
            newStdout
            initializeCallbacks
            handlers
            (modifyOptions options)
            Nothing
        , void $ waitBarrier clientMsgBarrier
        ]
    where
        handleInit :: IO () -> (LspId -> IO ()) -> (LspId -> IO ()) -> Chan Message -> LSP.LspFuncs () -> IO (Maybe err)
        handleInit exitClientMsg clearReqId waitForCancel clientMsgChan lspFuncs@LSP.LspFuncs{..} = do
            ide <- getIdeState sendFunc (makeLSPVFSHandle lspFuncs) clientCapabilities
            _ <- flip forkFinally (const exitClientMsg) $ forever $ do
                msg <- readChan clientMsgChan
                case msg of
                    Notification x@NotificationMessage{_params} act -> do
                        catch (act lspFuncs ide _params) $ \(e :: SomeException) ->
                            logError (ideLogger ide) $ T.pack $
                                "Unexpected exception on notification, please report!\n" ++
                                "Message: " ++ show x ++ "\n" ++
                                "Exception: " ++ show e
                    Response x@RequestMessage{_id, _params} wrap act ->
                        flip finally (clearReqId _id) $
                        catch (do
                            -- We could optimize this by first checking if the id
                            -- is in the cancelled set. However, this is unlikely to be a
                            -- bottleneck and the additional check might hide
                            -- issues with async exceptions that need to be fixed.
                            cancelOrRes <- race (waitForCancel _id) $ act lspFuncs ide _params
                            case cancelOrRes of
                                Left () -> do
                                    logDebug (ideLogger ide) $ T.pack $
                                        "Cancelled request " <> show _id
                                    sendFunc $ wrap $ ResponseMessage "2.0" (responseId _id) Nothing $
                                        Just $ ResponseError RequestCancelled "" Nothing
                                Right res ->
                                    sendFunc $ wrap $ ResponseMessage "2.0" (responseId _id) (Just res) Nothing
                        ) $ \(e :: SomeException) -> do
                            logError (ideLogger ide) $ T.pack $
                                "Unexpected exception on request, please report!\n" ++
                                "Message: " ++ show x ++ "\n" ++
                                "Exception: " ++ show e
                            sendFunc $ wrap $ ResponseMessage "2.0" (responseId _id) Nothing $
                                Just $ ResponseError InternalError (T.pack $ show e) Nothing
            pure Nothing


-- | Things that get sent to us, but we don't deal with.
--   Set them to avoid a warning in VS Code output.
setHandlersIgnore :: PartialHandlers
setHandlersIgnore = PartialHandlers $ \_ x -> return x
    {LSP.initializedHandler = none
    }
    where none = Just $ const $ return ()

cancelHandler :: (LspId -> IO ()) -> PartialHandlers
cancelHandler cancelRequest = PartialHandlers $ \_ x -> return x
    {LSP.cancelNotificationHandler = Just $ \msg@NotificationMessage {_params = CancelParams {_id}} -> do
            cancelRequest _id
            whenJust (LSP.cancelNotificationHandler x) ($ msg)
    }


-- | A message that we need to deal with - the pieces are split up with existentials to gain additional type safety
--   and defer precise processing until later (allows us to keep at a higher level of abstraction slightly longer)
data Message
    = forall m req resp . (Show m, Show req) => Response (RequestMessage m req resp) (ResponseMessage resp -> FromServerMessage) (LSP.LspFuncs () -> IdeState -> req -> IO resp)
    | forall m req . (Show m, Show req) => Notification (NotificationMessage m req) (LSP.LspFuncs () -> IdeState -> req -> IO ())


modifyOptions :: LSP.Options -> LSP.Options
modifyOptions x = x{LSP.textDocumentSync = Just $ tweak orig}
    where
        tweak x = x{_openClose=Just True, _change=Just TdSyncIncremental, _save=Just $ SaveOptions Nothing}
        orig = fromMaybe tdsDefault $ LSP.textDocumentSync x
        tdsDefault = TextDocumentSyncOptions Nothing Nothing Nothing Nothing Nothing
