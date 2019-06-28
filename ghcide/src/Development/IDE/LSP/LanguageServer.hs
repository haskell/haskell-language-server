-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ExistentialQuantification  #-}

-- WARNING: A copy of DA.Service.Daml.LanguageServer, try to keep them in sync
-- This version removes the daml: handling
module Development.IDE.LSP.LanguageServer
    ( runLanguageServer
    ) where

import           Language.Haskell.LSP.Types
import           Development.IDE.LSP.Server
import qualified Language.Haskell.LSP.Control as LSP
import qualified Language.Haskell.LSP.Core as LSP
import Control.Concurrent.Chan
import Control.Concurrent.Extra
import Control.Concurrent.Async
import Control.Exception.Safe
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import           GHC.IO.Handle                    (hDuplicate, hDuplicateTo)
import System.IO
import Control.Monad.Extra

import Development.IDE.LSP.Definition
import Development.IDE.LSP.Hover
import Development.IDE.LSP.Notifications
import Development.IDE.Core.Service
import Development.IDE.Types.Logger
import Development.IDE.Core.FileStore
import Language.Haskell.LSP.Core (LspFuncs(..))
import Language.Haskell.LSP.Messages


runLanguageServer
    :: LSP.Options
    -> PartialHandlers
    -> ((FromServerMessage -> IO ()) -> VFSHandle -> IO IdeState)
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

    let withResponse wrap f = Just $ \r -> writeChan clientMsgChan $ Response r wrap f
    let withNotification old f = Just $ \r -> writeChan clientMsgChan $ Notification r (\ide x -> f ide x >> whenJust old ($ r))
    let PartialHandlers parts =
            setHandlersIgnore <> -- least important
            setHandlersDefinition <> setHandlersHover <> -- useful features someone may override
            userHandlers <>
            setHandlersNotifications -- absolutely critical, join them with user notifications
    handlers <- parts WithMessage{withResponse, withNotification} def

    void $ waitAnyCancel =<< traverse async
        [ void $ LSP.runWithHandles
            stdin
            newStdout
            ( const $ Right ()
            , handleInit (signalBarrier clientMsgBarrier ()) clientMsgChan
            )
            handlers
            (modifyOptions options)
            Nothing
        , void $ waitBarrier clientMsgBarrier
        ]
    where
        handleInit :: IO () -> Chan Message -> LSP.LspFuncs () -> IO (Maybe err)
        handleInit exitClientMsg clientMsgChan lspFuncs@LSP.LspFuncs{..} = do
            ide <- getIdeState sendFunc (makeLSPVFSHandle lspFuncs)
            _ <- flip forkFinally (const exitClientMsg) $ forever $ do
                msg <- readChan clientMsgChan
                case msg of
                    Notification x@NotificationMessage{_params} act -> do
                        catch (act ide _params) $ \(e :: SomeException) ->
                            logError (ideLogger ide) $ T.pack $
                                "Unexpected exception on notification, please report!\n" ++
                                "Message: " ++ show x ++ "\n" ++
                                "Exception: " ++ show e
                    Response x@RequestMessage{_id, _params} wrap act ->
                        catch (do
                            res <- act ide _params
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
    {LSP.cancelNotificationHandler = none
    ,LSP.initializedHandler = none
    }
    where none = Just $ const $ return ()


-- | A message that we need to deal with - the pieces are split up with existentials to gain additional type safety
--   and defer precise processing until later (allows us to keep at a higher level of abstraction slightly longer)
data Message
    = forall m req resp . (Show m, Show req) => Response (RequestMessage m req resp) (ResponseMessage resp -> FromServerMessage) (IdeState -> req -> IO resp)
    | forall m req . (Show m, Show req) => Notification (NotificationMessage m req) (IdeState -> req -> IO ())


modifyOptions :: LSP.Options -> LSP.Options
modifyOptions x = x{LSP.textDocumentSync = Just $ tweak orig}
    where
        tweak x = x{_openClose=Just True, _change=Just TdSyncIncremental, _save=Just $ SaveOptions Nothing}
        orig = fromMaybe tdsDefault $ LSP.textDocumentSync x
        tdsDefault = TextDocumentSyncOptions Nothing Nothing Nothing Nothing Nothing
