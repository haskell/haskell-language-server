{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Ide.Plugin.Resolve (mkCodeActionHandlerWithResolve,
mkCodeActionWithResolveAndCommand) where

import           Control.Lens                  (_Just, (&), (.~), (?~), (^?))
import           Control.Monad.Trans.Class     (MonadTrans (lift))
import           Control.Monad.Trans.Except    (ExceptT (..), runExceptT)
import           Data.Aeson                    (ToJSON (toJSON))
import qualified Data.Aeson
import           Data.Row                      ((.!))
import           Ide.Types
import qualified Language.LSP.Protocol.Lens    as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server           (LspM, LspT,
                                                ProgressCancellable (Cancellable),
                                                getClientCapabilities,
                                                sendRequest,
                                                withIndefiniteProgress)

  -- |When provided with both a codeAction provider and an affiliated codeAction
-- resolve provider, this function creates a handler that automatically uses
-- your resolve provider to fill out you original codeAction if the client doesn't
-- have codeAction resolve support. This means you don't have to check whether
-- the client supports resolve and act accordingly in your own providers.
mkCodeActionHandlerWithResolve
  :: forall ideState. (ideState -> PluginId -> CodeActionParams -> LspM Config (Either ResponseError ([Command |? CodeAction] |? Null)))
  -> (ideState -> PluginId -> CodeAction -> LspM Config (Either ResponseError CodeAction))
  -> PluginHandlers ideState
mkCodeActionHandlerWithResolve codeActionMethod codeResolveMethod =
    let newCodeActionMethod ideState pid params = runExceptT $
            do codeActionReturn <- ExceptT $ codeActionMethod ideState pid params
               caps <- lift getClientCapabilities
               case codeActionReturn of
                r@(InR Null) -> pure r
                (InL ls) | -- If the client supports resolve, we will wrap the resolve data in a owned
                           -- resolve data type to allow the server to know who to send the resolve request to
                           supportsCodeActionResolve caps -> pure $ InL ls
                           --This is the actual part where we call resolveCodeAction which fills in the edit data for the client
                         | otherwise -> InL <$> traverse (resolveCodeAction ideState pid) ls
    in mkPluginHandler SMethod_TextDocumentCodeAction newCodeActionMethod
    <> mkPluginHandler SMethod_CodeActionResolve codeResolveMethod
    where
        dropData :: CodeAction -> CodeAction
        dropData ca = ca & L.data_ .~ Nothing
        resolveCodeAction :: ideState -> PluginId -> (Command |? CodeAction) -> ExceptT ResponseError (LspT Config IO) (Command |? CodeAction)
        resolveCodeAction _ideState _pid c@(InL _) = pure c
        resolveCodeAction ideState pid (InR codeAction) =
            fmap (InR . dropData) $ ExceptT $ codeResolveMethod ideState pid codeAction

-- |When provided with both a codeAction provider that includes both a command
-- and a data field and a resolve provider, this function creates a handler that
-- defaults to using your command if the client doesn't have code action resolve
-- support. This means you don't have to check whether the client supports resolve
-- and act accordingly in your own providers.
mkCodeActionWithResolveAndCommand
  :: forall ideState.
  PluginId
  -> (ideState -> PluginId -> CodeActionParams -> LspM Config (Either ResponseError ([Command |? CodeAction] |? Null)))
  -> (ideState -> PluginId -> CodeAction -> LspM Config (Either ResponseError CodeAction))
  -> ([PluginCommand ideState], PluginHandlers ideState)
mkCodeActionWithResolveAndCommand plId codeActionMethod codeResolveMethod =
    let newCodeActionMethod ideState pid params = runExceptT $
            do codeActionReturn <- ExceptT $ codeActionMethod ideState pid params
               caps <- lift getClientCapabilities
               case codeActionReturn of
                r@(InR Null) -> pure r
                (InL ls) | -- If the client supports resolve, we will wrap the resolve data in a owned
                           -- resolve data type to allow the server to know who to send the resolve request to
                           supportsCodeActionResolve caps ->
                            pure $ InL ls
                           -- If they do not we will drop the data field, in addition we will populate the command
                           -- field with our command to execute the resolve, with the whole code action as it's argument.
                         | otherwise -> pure $ InL $ moveDataToCommand <$> ls
    in ([PluginCommand "codeActionResolve" "Executes resolve for code action" (executeResolveCmd plId codeResolveMethod)],
    mkPluginHandler SMethod_TextDocumentCodeAction newCodeActionMethod
    <> mkPluginHandler SMethod_CodeActionResolve codeResolveMethod)
  where moveDataToCommand :: Command |? CodeAction -> Command |? CodeAction
        moveDataToCommand ca =
          let dat = toJSON <$> ca ^? _R -- We need to take the whole codeAction
              -- And put it in the argument for the Command, that way we can later
              -- pas it to the resolve handler (which expects a whole code action)
              cmd = mkLspCommand plId (CommandId "codeActionResolve") "Execute Code Action" (pure <$> dat)
          in ca
              & _R . L.data_ .~ Nothing -- Set the data field to nothing
              & _R . L.command ?~ cmd -- And set the command to our previously created command
        executeResolveCmd :: PluginId -> PluginMethodHandler ideState Method_CodeActionResolve -> CommandFunction ideState CodeAction
        executeResolveCmd pluginId resolveProvider ideState ca =  do
          withIndefiniteProgress "Executing code action..." Cancellable $ do
            resolveResult <- resolveProvider ideState pluginId ca
            case resolveResult of
              Right CodeAction {_edit = Just wedits } -> do
                  _ <- sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedits) (\_ -> pure ())
                  pure $ Right Data.Aeson.Null
              Right _ -> pure $ Left $ responseError "No edit in CodeAction"
              Left err -> pure $ Left err

supportsCodeActionResolve :: ClientCapabilities -> Bool
supportsCodeActionResolve caps =
    caps ^? L.textDocument . _Just . L.codeAction . _Just . L.dataSupport . _Just == Just True
    && case caps ^? L.textDocument . _Just . L.codeAction . _Just . L.resolveSupport . _Just of
        Just row -> "edit" `elem` row .! #properties
        _        -> False
