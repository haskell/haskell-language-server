{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE ScopedTypeVariables      #-}

module Ide.Plugin.Resolve
(mkCodeActionHandlerWithResolve,
mkCodeActionWithResolveAndCommand) where

import           Control.Lens                  (_Just, (&), (.~), (?~), (^.),
                                                (^?))
import           Control.Monad.Trans.Class     (MonadTrans (lift))
import           Control.Monad.Trans.Except    (ExceptT (..), runExceptT,
                                                throwE)
import qualified Data.Aeson                    as A
import           Data.Row                      ((.!))
import qualified Data.Text                     as T
import           GHC.Generics                  (Generic)
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
  :: forall ideState a. (A.FromJSON a) =>
   (ideState -> PluginId -> CodeActionParams -> LspM Config (Either ResponseError ([Command |? CodeAction] |? Null)))
  -> (ideState -> PluginId -> CodeAction -> Uri -> a -> LspM Config (Either ResponseError CodeAction))
  -> (PluginHandlers ideState, PluginResolveHandlers ideState)
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
                         | otherwise -> InL <$> traverse (resolveCodeAction (params ^. L.textDocument . L.uri) ideState pid) ls
    in (mkPluginHandler SMethod_TextDocumentCodeAction newCodeActionMethod
    , mkResolveHandler SMethod_CodeActionResolve codeResolveMethod)
    where
        dropData :: CodeAction -> CodeAction
        dropData ca = ca & L.data_ .~ Nothing
        resolveCodeAction :: Uri -> ideState -> PluginId -> (Command |? CodeAction) -> ExceptT ResponseError (LspT Config IO) (Command |? CodeAction)
        resolveCodeAction _uri _ideState _plId c@(InL _) = pure c
        resolveCodeAction uri ideState pid (InR codeAction@CodeAction{_data_=Just value}) = do
            case A.fromJSON value of
              A.Error err -> throwE $ parseError (Just value) (T.pack err)
              A.Success innerValueDecoded -> do
                resolveResult <- ExceptT $ codeResolveMethod ideState pid codeAction uri innerValueDecoded
                case resolveResult of
                  CodeAction {_edit = Just _ } -> do
                      pure $ InR $ dropData resolveResult
                  _ -> throwE $ invalidParamsError "Returned CodeAction has no data field"
        resolveCodeAction _ _ _ (InR CodeAction{_data_=Nothing}) = throwE $ invalidParamsError "CodeAction has no data field"

-- |When provided with both a codeAction provider that includes both a command
-- and a data field and a resolve provider, this function creates a handler that
-- defaults to using your command if the client doesn't have code action resolve
-- support. This means you don't have to check whether the client supports resolve
-- and act accordingly in your own providers.
mkCodeActionWithResolveAndCommand
  :: forall ideState a. (A.FromJSON a) =>
  PluginId
  -> (ideState -> PluginId -> CodeActionParams -> LspM Config (Either ResponseError ([Command |? CodeAction] |? Null)))
  -> (ideState -> PluginId -> CodeAction -> Uri -> a -> LspM Config (Either ResponseError CodeAction))
  -> ([PluginCommand ideState], PluginHandlers ideState, PluginResolveHandlers ideState)
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
                         | otherwise -> pure $ InL $ moveDataToCommand (params ^. L.textDocument . L.uri) <$> ls
    in ([PluginCommand "codeActionResolve" "Executes resolve for code action" (executeResolveCmd (codeResolveMethod))],
    mkPluginHandler SMethod_TextDocumentCodeAction newCodeActionMethod,
    mkResolveHandler SMethod_CodeActionResolve codeResolveMethod)
  where moveDataToCommand :: Uri -> Command |? CodeAction -> Command |? CodeAction
        moveDataToCommand uri ca =
          let dat = A.toJSON . wrapWithURI uri <$> ca ^? _R -- We need to take the whole codeAction
              -- And put it in the argument for the Command, that way we can later
              -- pas it to the resolve handler (which expects a whole code action)
              cmd = mkLspCommand plId (CommandId "codeActionResolve") "Execute Code Action" (pure <$> dat)
          in ca
              & _R . L.data_ .~ Nothing -- Set the data field to nothing
              & _R . L.command ?~ cmd -- And set the command to our previously created command
        wrapWithURI ::  Uri -> CodeAction -> CodeAction
        wrapWithURI  uri codeAction =
          codeAction & L.data_ .~  (A.toJSON .WithURI uri <$> data_)
          where data_ = codeAction ^? L.data_ . _Just
        executeResolveCmd :: (ideState -> PluginId -> CodeAction -> Uri -> a -> LspM Config (Either ResponseError CodeAction))-> CommandFunction ideState CodeAction
        executeResolveCmd resolveProvider ideState ca@CodeAction{_data_=Just value} =  do
          withIndefiniteProgress "Executing code action..." Cancellable $ do
            case A.fromJSON value of
              A.Error err -> pure $ Left $ parseError (Just value) (T.pack err)
              A.Success (WithURI uri innerValue) -> do
                case A.fromJSON innerValue of
                  A.Error err -> pure $ Left $ parseError (Just value) (T.pack err)
                  A.Success innerValueDecoded -> do
                    resolveResult <- resolveProvider ideState plId ca uri innerValueDecoded
                    case resolveResult of
                      Right CodeAction {_edit = Just wedits } -> do
                          _ <- sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing wedits) (\_ -> pure ())
                          pure $ Right A.Null
                      Right _ -> pure $ Left $ invalidParamsError "Returned CodeAction has no data field"
                      Left err -> pure $ Left err
        executeResolveCmd _ _ CodeAction{_data_= value} = pure $ Left $ invalidParamsError ("CodeAction data field empty: " <> (T.pack $ show value))


-- |To execute the resolve provider as a command, we need to additionally store
-- the URI that was provided to the original code action.
data WithURI = WithURI {
 _uri    :: Uri
, _value :: A.Value
} deriving (Generic, Show)
instance A.ToJSON WithURI
instance A.FromJSON WithURI

supportsCodeActionResolve :: ClientCapabilities -> Bool
supportsCodeActionResolve caps =
    caps ^? L.textDocument . _Just . L.codeAction . _Just . L.dataSupport . _Just == Just True
    && case caps ^? L.textDocument . _Just . L.codeAction . _Just . L.resolveSupport . _Just of
        Just row -> "edit" `elem` row .! #properties
        _        -> False

invalidParamsError :: T.Text -> ResponseError
invalidParamsError msg = ResponseError (InR ErrorCodes_InternalError) ("Ide.Plugin.Resolve: " <> msg) Nothing

parseError :: Maybe A.Value -> T.Text -> ResponseError
parseError value errMsg = ResponseError (InR ErrorCodes_InternalError) ("Ide.Plugin.Resolve: Error parsing value:"<> (T.pack $ show value) <> " Error: "<> errMsg) Nothing
