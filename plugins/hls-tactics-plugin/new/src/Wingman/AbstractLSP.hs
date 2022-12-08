{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards     #-}

{-# LANGUAGE NoMonoLocalBinds    #-}

{-# OPTIONS_GHC -Wno-orphans     #-}

module Wingman.AbstractLSP (installInteractions) where

import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import qualified Data.Aeson as A
import           Data.Coerce
import           Data.Foldable (traverse_)
import           Data.Monoid (Last (..))
import qualified Data.Text as T
import           Data.Traversable (for)
import           Data.Tuple.Extra (uncurry3)
import           Development.IDE (IdeState)
import           Development.IDE.Core.UseStale
import           Development.IDE.GHC.ExactPrint (GetAnnotatedParsedSource(GetAnnotatedParsedSource))
import qualified Ide.Plugin.Config as Plugin
import           Ide.Types
import           Language.LSP.Server (LspM, sendRequest, getClientCapabilities)
import qualified Language.LSP.Types as LSP
import           Language.LSP.Types hiding (CodeLens, CodeAction)
import           Wingman.AbstractLSP.Types
import           Wingman.EmptyCase (fromMaybeT)
import           Wingman.LanguageServer (runIde, getTacticConfigAction, getIdeDynflags, mkWorkspaceEdits, runStaleIde, showLspMessage, mkShowMessageParams)
import           Wingman.StaticPlugin (enableQuasiQuotes)
import           Wingman.Types


------------------------------------------------------------------------------
-- | Attach the 'Interaction's to a 'PluginDescriptor'. Interactions are
-- self-contained request/response pairs that abstract over the LSP, and
-- provide a unified interface for doing interesting things, without needing to
-- dive into the underlying API too directly.
installInteractions
    :: [Interaction]
    -> PluginDescriptor IdeState
    -> PluginDescriptor IdeState
installInteractions is desc =
  let plId = pluginId desc
   in desc
        { pluginCommands = pluginCommands desc <> fmap (buildCommand plId) is
        , pluginHandlers = pluginHandlers desc <> buildHandlers is
        }


------------------------------------------------------------------------------
-- | Extract 'PluginHandlers' from 'Interaction's.
buildHandlers
    :: [Interaction]
    -> PluginHandlers IdeState
buildHandlers cs =
  flip foldMap cs $ \(Interaction (c :: Continuation sort target b)) ->
    case c_makeCommand c of
      SynthesizeCodeAction k ->
        mkPluginHandler STextDocumentCodeAction $ codeActionProvider @target (c_sort c) k
      SynthesizeCodeLens k ->
        mkPluginHandler STextDocumentCodeLens   $ codeLensProvider   @target (c_sort c) k


------------------------------------------------------------------------------
-- | Extract a 'PluginCommand' from an 'Interaction'.
buildCommand
  :: PluginId
  -> Interaction
  -> PluginCommand IdeState
buildCommand plId (Interaction (c :: Continuation sort target b)) =
  PluginCommand
    { commandId = toCommandId $ c_sort c
    , commandDesc = T.pack ""
    , commandFunc = runContinuation plId c
    }


------------------------------------------------------------------------------
-- | Boilerplate for running a 'Continuation' as part of an LSP command.
runContinuation
    :: forall sort a b
     . IsTarget a
    => PluginId
    -> Continuation sort a b
    -> CommandFunction IdeState (FileContext, b)
runContinuation plId cont state (fc, b) = do
  fromMaybeT
    (Left $ ResponseError
              { _code = InternalError
              , _message = T.pack "TODO(sandy)"
              , _xdata =  Nothing
              } ) $ do
      env@LspEnv{..} <- buildEnv state plId fc
      nfp <- getNfp $ fc_uri le_fileContext
      let stale a = runStaleIde "runContinuation" state nfp a
      args <- fetchTargetArgs @a env
      res <- c_runCommand cont env args fc b

      -- This block returns a maybe error.
      fmap (maybe (Right A.Null) Left . coerce . foldMap Last) $
        for res $ \case
          ErrorMessages errs -> do
            traverse_ showUserFacingMessage errs
            pure Nothing
          RawEdit edits -> do
            sendEdits edits
            pure Nothing
          GraftEdit gr -> do
            ccs <- lift getClientCapabilities
            TrackedStale pm _ <- mapMaybeT liftIO $ stale GetAnnotatedParsedSource
            case mkWorkspaceEdits (enableQuasiQuotes le_dflags) ccs (fc_uri le_fileContext) (unTrack pm) gr of
              Left errs ->
                pure $ Just $ ResponseError
                  { _code    = InternalError
                  , _message = T.pack $ show errs
                  , _xdata   = Nothing
                  }
              Right edits -> do
                sendEdits edits
                pure Nothing


------------------------------------------------------------------------------
-- | Push a 'WorkspaceEdit' to the client.
sendEdits :: WorkspaceEdit -> MaybeT (LspM Plugin.Config) ()
sendEdits edits =
  void $ lift $
    sendRequest
      SWorkspaceApplyEdit
      (ApplyWorkspaceEditParams Nothing edits)
      (const $ pure ())


------------------------------------------------------------------------------
-- | Push a 'UserFacingMessage' to the client.
showUserFacingMessage
    :: UserFacingMessage
    -> MaybeT (LspM Plugin.Config) ()
showUserFacingMessage ufm =
  void $ lift $ showLspMessage $ mkShowMessageParams ufm


------------------------------------------------------------------------------
-- | Build an 'LspEnv', which contains the majority of things we need to know
-- in a 'Continuation'.
buildEnv
    :: IdeState
    -> PluginId
    -> FileContext
    -> MaybeT (LspM Plugin.Config) LspEnv
buildEnv state plId fc = do
  cfg <- liftIO $ runIde "plugin" "config" state $ getTacticConfigAction plId
  nfp <- getNfp $ fc_uri fc
  dflags <- mapMaybeT liftIO $ getIdeDynflags state nfp
  pure $ LspEnv
    { le_ideState = state
    , le_pluginId = plId
    , le_dflags   = dflags
    , le_config   = cfg
    , le_fileContext = fc
    }


------------------------------------------------------------------------------
-- | Lift a 'Continuation' into an LSP CodeAction.
codeActionProvider
    :: forall target sort b
     . (IsContinuationSort sort, A.ToJSON b, IsTarget target)
    => sort
    -> ( LspEnv
     -> TargetArgs target
     -> MaybeT (LspM Plugin.Config) [(Metadata, b)]
       )
    -> PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider sort k state plId
                   (CodeActionParams _ _ (TextDocumentIdentifier uri) range _) = do
  fromMaybeT (Right $ List []) $ do
    let fc = FileContext
                { fc_uri   = uri
                , fc_range = Just $ unsafeMkCurrent range
                }
    env <- buildEnv state plId fc
    args <- fetchTargetArgs @target env
    actions <- k env args
    pure
      $ Right
      $ List
      $ fmap (InR . uncurry (makeCodeAction plId fc sort)) actions


------------------------------------------------------------------------------
-- | Lift a 'Continuation' into an LSP CodeLens.
codeLensProvider
    :: forall target sort b
     . (IsContinuationSort sort, A.ToJSON b, IsTarget target)
    => sort
    -> ( LspEnv
     -> TargetArgs target
     -> MaybeT (LspM Plugin.Config) [(Range, Metadata, b)]
      )
    -> PluginMethodHandler IdeState TextDocumentCodeLens
codeLensProvider sort k state plId
                 (CodeLensParams _ _ (TextDocumentIdentifier uri)) = do
      fromMaybeT (Right $ List []) $ do
        let fc = FileContext
                   { fc_uri   = uri
                   , fc_range = Nothing
                   }
        env <- buildEnv state plId fc
        args <- fetchTargetArgs @target env
        actions <- k env args
        pure
          $ Right
          $ List
          $ fmap (uncurry3 $ makeCodeLens plId sort fc) actions


------------------------------------------------------------------------------
-- | Build a 'LSP.CodeAction'.
makeCodeAction
    :: (A.ToJSON b, IsContinuationSort sort)
    => PluginId
    -> FileContext
    -> sort
    -> Metadata
    -> b
    -> LSP.CodeAction
makeCodeAction plId fc sort (Metadata title kind preferred) b =
  let cmd_id = toCommandId sort
      cmd = mkLspCommand plId cmd_id title $ Just [A.toJSON (fc, b)]
   in LSP.CodeAction
        { _title       = title
        , _kind        = Just kind
        , _diagnostics = Nothing
        , _isPreferred = Just preferred
        , _disabled    = Nothing
        , _edit        = Nothing
        , _command     = Just cmd
        , _xdata       = Nothing
        }


------------------------------------------------------------------------------
-- | Build a 'LSP.CodeLens'.
makeCodeLens
    :: (A.ToJSON b, IsContinuationSort sort)
    => PluginId
    -> sort
    -> FileContext
    -> Range
    -> Metadata
    -> b
    -> LSP.CodeLens
makeCodeLens plId sort fc range (Metadata title _ _) b =
  let fc' = fc { fc_range = Just $ unsafeMkCurrent range }
      cmd_id = toCommandId sort
      cmd = mkLspCommand plId cmd_id title $ Just [A.toJSON (fc', b)]
   in LSP.CodeLens
        { _range = range
        , _command = Just cmd
        , _xdata = Nothing
        }

