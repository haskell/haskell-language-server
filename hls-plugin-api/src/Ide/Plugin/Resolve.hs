{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-| This module currently includes helper functions to provide fallback support
to code actions that use resolve in HLS. The difference between the two
functions for code actions that don't support resolve is that
mkCodeActionHandlerWithResolve will immediately resolve your code action before
sending it on to the client, while  mkCodeActionWithResolveAndCommand will turn
your resolve into a command.

General support for resolve in HLS can be used with mkResolveHandler from
Ide.Types. Resolve theoretically should allow us to delay computation of parts
of the request till the client needs it, allowing us to answer requests faster
and with less resource usage.
-}
module Ide.Plugin.Resolve
(mkCodeActionHandlerWithResolve,
mkCodeActionWithResolveAndCommand) where

import           Control.Lens                  (_Just, (&), (.~), (?~), (^.),
                                                (^?))
import           Control.Monad.Error.Class     (MonadError (throwError))
import           Control.Monad.Trans.Class     (lift)
import           Control.Monad.Trans.Except    (ExceptT (..))

import qualified Data.Aeson                    as A
import           Data.Maybe                    (catMaybes)
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)
import           Ide.Logger
import           Ide.Plugin.Error
import           Ide.Types
import qualified Language.LSP.Protocol.Lens    as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server           (LspT, getClientCapabilities,
                                                sendRequest)

data Log
    = DoesNotSupportResolve T.Text
    | forall m . A.ToJSON (ErrorData m) => ApplyWorkspaceEditFailed (TResponseError m)
instance Pretty Log where
    pretty = \case
        DoesNotSupportResolve fallback->
            "Client does not support resolve," <+> pretty fallback
        ApplyWorkspaceEditFailed err ->
            "ApplyWorkspaceEditFailed:" <+> pretty err

-- |When provided with both a codeAction provider and an affiliated codeAction
-- resolve provider, this function creates a handler that automatically uses
-- your resolve provider to fill out you original codeAction if the client doesn't
-- have codeAction resolve support. This means you don't have to check whether
-- the client supports resolve and act accordingly in your own providers.
mkCodeActionHandlerWithResolve
  :: forall ideState a. (A.FromJSON a) =>
  Recorder (WithPriority Log)
  -> PluginMethodHandler ideState 'Method_TextDocumentCodeAction
  -> ResolveFunction ideState a 'Method_CodeActionResolve
  -> PluginHandlers ideState
mkCodeActionHandlerWithResolve recorder codeActionMethod codeResolveMethod =
  let newCodeActionMethod ideState pid params =
        do codeActionReturn <- codeActionMethod ideState pid params
           caps <- lift getClientCapabilities
           case codeActionReturn of
             r@(InR Null) -> pure r
             (InL ls) | -- We don't need to do anything if the client supports
                        -- resolve
                        supportsCodeActionResolve caps -> pure $ InL ls
                        --This is the actual part where we call resolveCodeAction which fills in the edit data for the client
                      | otherwise -> do
                        logWith recorder Debug (DoesNotSupportResolve "filling in the code action")
                        InL <$> traverse (resolveCodeAction (params ^. L.textDocument . L.uri) ideState pid) ls
  in (mkPluginHandler SMethod_TextDocumentCodeAction newCodeActionMethod
  <> mkResolveHandler SMethod_CodeActionResolve codeResolveMethod)
  where dropData :: CodeAction -> CodeAction
        dropData ca = ca & L.data_ .~ Nothing
        resolveCodeAction :: Uri -> ideState -> PluginId -> (Command |? CodeAction) -> ExceptT PluginError (LspT Config IO) (Command |? CodeAction)
        resolveCodeAction _uri _ideState _plId c@(InL _) = pure c
        resolveCodeAction uri ideState pid (InR codeAction@CodeAction{_data_=Just value}) = do
          case A.fromJSON value of
            A.Error err -> throwError $ parseError (Just value) (T.pack err)
            A.Success innerValueDecoded -> do
              resolveResult <- codeResolveMethod ideState pid codeAction uri innerValueDecoded
              case resolveResult of
                CodeAction {_edit = Just _ } -> do
                  pure $ InR $ dropData resolveResult
                _ -> throwError $ invalidParamsError "Returned CodeAction has no data field"
        resolveCodeAction _ _ _ (InR CodeAction{_data_=Nothing}) = throwError $ invalidParamsError "CodeAction has no data field"


-- |When provided with both a codeAction provider with a data field and a resolve
--  provider, this function creates a handler that creates a command that uses
-- your resolve if the client doesn't have code action resolve support. This means
-- you don't have to check whether the client supports resolve and act
-- accordingly in your own providers. see Note [Code action resolve fallback to commands]
-- Also: This helper only works with workspace edits, not commands. Any command set
-- either in the original code action or in the resolve will be ignored.
mkCodeActionWithResolveAndCommand
  :: forall ideState a. (A.FromJSON a) =>
  Recorder (WithPriority Log)
  -> PluginId
  -> PluginMethodHandler ideState 'Method_TextDocumentCodeAction
  -> ResolveFunction ideState a 'Method_CodeActionResolve
  -> ([PluginCommand ideState], PluginHandlers ideState)
mkCodeActionWithResolveAndCommand recorder plId codeActionMethod codeResolveMethod =
  let newCodeActionMethod ideState pid params =
        do codeActionReturn <- codeActionMethod ideState pid params
           caps <- lift getClientCapabilities
           case codeActionReturn of
             r@(InR Null) -> pure r
             (InL ls) | -- We don't need to do anything if the client supports
                        -- resolve
                        supportsCodeActionResolve caps -> pure $ InL ls
                        -- If they do not we will drop the data field, in addition we will populate the command
                        -- field with our command to execute the resolve, with the whole code action as it's argument.
                      | otherwise -> do
                        logWith recorder Debug (DoesNotSupportResolve "rewriting the code action to use commands")
                        pure $ InL $ moveDataToCommand (params ^. L.textDocument . L.uri) <$> ls
  in ([PluginCommand "codeActionResolve" "Executes resolve for code action" (executeResolveCmd codeResolveMethod)],
  mkPluginHandler SMethod_TextDocumentCodeAction newCodeActionMethod
  <> mkResolveHandler SMethod_CodeActionResolve codeResolveMethod)
  where moveDataToCommand :: Uri -> Command |? CodeAction -> Command |? CodeAction
        moveDataToCommand uri ca =
          let dat = A.toJSON . wrapWithURI uri <$> ca ^? _R -- We need to take the whole codeAction
              -- And put it in the argument for the Command, that way we can later
              -- pass it to the resolve handler (which expects a whole code action)
              -- It should be noted that mkLspCommand already specifies the command
              -- to the plugin, so we don't need to do that here.
              cmd = mkLspCommand plId (CommandId "codeActionResolve") "Execute Code Action" (pure <$> dat)
          in ca
              & _R . L.data_ .~ Nothing -- Set the data field to nothing
              & _R . L.command ?~ cmd -- And set the command to our previously created command
        wrapWithURI ::  Uri -> CodeAction -> CodeAction
        wrapWithURI  uri codeAction =
          codeAction & L.data_ .~  (A.toJSON .WithURI uri <$> data_)
          where data_ = codeAction ^? L.data_ . _Just
        executeResolveCmd :: ResolveFunction ideState a 'Method_CodeActionResolve -> CommandFunction ideState CodeAction
        executeResolveCmd resolveProvider ideState _token ca@CodeAction{_data_=Just value} = do
          case A.fromJSON value of
            A.Error err -> throwError $ parseError (Just value) (T.pack err)
            A.Success (WithURI uri innerValue) -> do
              case A.fromJSON innerValue of
                A.Error err -> throwError $ parseError (Just value) (T.pack err)
                A.Success innerValueDecoded -> do
                  resolveResult <- resolveProvider ideState plId ca uri innerValueDecoded
                  case resolveResult of
                    ca2@CodeAction {_edit = Just wedits } | diffCodeActions ca ca2 == ["edit"] -> do
                        _ <- ExceptT $ Right <$> sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedits) handleWEditCallback
                        pure $ InR Null
                    ca2@CodeAction {_edit = Just _ }  ->
                      throwError $ internalError $
                          "The resolve provider unexpectedly returned a code action with the following differing fields: "
                          <> (T.pack $ show $  diffCodeActions ca ca2)
                    _ -> throwError $ internalError "The resolve provider unexpectedly returned a result with no data field"
        executeResolveCmd _ _ _ CodeAction{_data_= value} = throwError $ invalidParamsError ("The code action to resolve has an illegal data field: " <> (T.pack $ show value))
        handleWEditCallback (Left err ) = do
            logWith recorder Warning (ApplyWorkspaceEditFailed err)
            pure ()
        handleWEditCallback _ = pure ()

-- TODO: Remove once provided by lsp-types
-- |Compares two CodeActions and returns a list of fields that are not equal
diffCodeActions :: CodeAction -> CodeAction -> [T.Text]
diffCodeActions ca ca2 =
  let titleDiff = if ca ^. L.title == ca2 ^. L.title then Nothing else Just "title"
      kindDiff = if ca ^. L.kind == ca2 ^. L.kind then Nothing else Just "kind"
      diagnosticsDiff = if ca ^. L.diagnostics == ca2 ^. L.diagnostics then Nothing else Just "diagnostics"
      commandDiff = if ca ^. L.command == ca2 ^. L.command then Nothing else Just "diagnostics"
      isPreferredDiff = if ca ^. L.isPreferred == ca2 ^. L.isPreferred then Nothing else Just "isPreferred"
      dataDiff = if ca ^. L.data_ == ca2 ^. L.data_ then Nothing else Just "data"
      disabledDiff = if ca ^. L.disabled == ca2 ^. L.disabled then Nothing else Just "disabled"
      editDiff = if ca ^. L.edit == ca2 ^. L.edit then Nothing else Just "edit"
  in catMaybes [titleDiff, kindDiff, diagnosticsDiff, commandDiff, isPreferredDiff, dataDiff, disabledDiff, editDiff]

-- |To execute the resolve provider as a command, we need to additionally store
-- the URI that was provided to the original code action.
data WithURI = WithURI {
 _uri    :: Uri
, _value :: A.Value
} deriving (Generic, Show)
instance A.ToJSON WithURI
instance A.FromJSON WithURI

-- |Checks if the the client supports resolve for code action. We currently only check
--  whether resolve for the edit field is supported, because that's the only one we care
-- about at the moment.
supportsCodeActionResolve :: ClientCapabilities -> Bool
supportsCodeActionResolve caps =
    caps ^? L.textDocument . _Just . L.codeAction . _Just . L.dataSupport . _Just == Just True
    && case caps ^? L.textDocument . _Just . L.codeAction . _Just . L.resolveSupport . _Just of
        Just ClientCodeActionResolveOptions{_properties} -> "edit" `elem` _properties
        _        -> False

internalError :: T.Text -> PluginError
internalError msg = PluginInternalError ("Ide.Plugin.Resolve: " <> msg)

invalidParamsError :: T.Text -> PluginError
invalidParamsError msg = PluginInvalidParams ("Ide.Plugin.Resolve: : " <> msg)

parseError :: Maybe A.Value -> T.Text -> PluginError
parseError value errMsg = PluginInternalError ("Ide.Plugin.Resolve: Error parsing value:"<> (T.pack $ show value) <> " Error: "<> errMsg)

{- Note [Code action resolve fallback to commands]
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  To make supporting code action resolve easy for plugins, we want to let them
  provide one implementation that can be used both when clients support
  resolve, and when they don't.
  The way we do this is to have them always implement a resolve handler.
  Then, if the client doesn't support resolve, we instead install the resolve
  handler as a _command_ handler, passing the code action literal itself
  as the command argument. This allows the command handler to have
  the same interface as the resolve handler!
  -}
