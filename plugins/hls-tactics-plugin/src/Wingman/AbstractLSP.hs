{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE RecordWildCards        #-}

module Wingman.AbstractLSP where

import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe (MaybeT, mapMaybeT)
import qualified Data.Aeson as A
import           Data.Foldable (traverse_)
import qualified Data.Text as T
import           Data.Tuple.Extra (uncurry3)
import           Development.IDE (IdeState)
import           Development.IDE.Core.UseStale
import qualified Ide.Plugin.Config as Plugin
import           Ide.Types
import           Language.LSP.Server (LspM, sendRequest)
import qualified Language.LSP.Types as LSP
import           Language.LSP.Types hiding (CodeLens, CodeAction)
import           Wingman.AbstractLSP.Types
import           Wingman.EmptyCase (fromMaybeT)
import           Wingman.LanguageServer (getTacticConfig, getIdeDynflags)
import           Wingman.Types


buildHandlers
    :: forall target sort b
     . (Show sort, IsTarget target, A.ToJSON b )
    => [Continuation sort target b]
    -> PluginHandlers IdeState
buildHandlers cs =
  flip foldMap cs $ \c ->
    case c_makeCommand c of
      SynthesizeCodeAction k ->
        mkPluginHandler STextDocumentCodeAction $ codeActionProvider @target (c_sort c) k
      SynthesizeCodeLens k ->
        mkPluginHandler STextDocumentCodeLens   $ codeLensProvider   @target (c_sort c) k


runCodeAction
    :: forall sort a b
     . IsTarget a
    => PluginId
    -> Continuation sort a b
    -> CommandFunction IdeState (FileContext, b)
runCodeAction plId cont state (fc, b) =
  fromMaybeT (Left undefined) $ do
    env <- buildEnv state plId fc
    args <- fetchTargetArgs @a env
    c_runCommand cont env args fc b >>= \case
      Left errs ->
        traverse_ showUserFacingMessage errs
      Right edits ->
        void $ lift $
          sendRequest
            SWorkspaceApplyEdit
            (ApplyWorkspaceEditParams Nothing edits)
            (const $ pure ())
    pure $ Right A.Null


showUserFacingMessage :: UserFacingMessage -> MaybeT (LspM Plugin.Config) ()
showUserFacingMessage = error "not implemented"


buildEnv
    :: IdeState
    -> PluginId
    -> FileContext
    -> MaybeT (LspM Plugin.Config) LspEnv
buildEnv state plId fc = do
  cfg <- lift $ getTacticConfig plId
  dflags <- mapMaybeT liftIO $ getIdeDynflags state $ fc_nfp fc
  pure $ LspEnv
    { le_ideState = state
    , le_pluginId = plId
    , le_dflags   = dflags
    , le_config   = cfg
    , le_fileContext = fc
    }


codeActionProvider
    :: forall target sort b
     . (Show sort, A.ToJSON b, IsTarget target)
    => sort
    -> ( LspEnv
     -> TargetArgs target
     -> MaybeT (LspM Plugin.Config) [(Metadata, b)]
       )
    -> PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider
    sort k state plId
    (CodeActionParams _ _ (TextDocumentIdentifier uri) range _)
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      fromMaybeT (Right $ List []) $ do
        let fc = FileContext
                   { fc_uri   = uri
                   , fc_nfp   = nfp
                   , fc_range = Just $ unsafeMkCurrent range
                   }
        env <- buildEnv state plId fc
        args <- fetchTargetArgs @target env
        actions <- k env args
        pure
          $ Right
          $ List
          $ fmap (InR . uncurry (makeCodeAction plId sort)) actions
codeActionProvider _ _ _ _ _ = pure $ Right $ List []


codeLensProvider
    :: forall target sort b
     . (Show sort, A.ToJSON b, IsTarget target)
    => sort
    -> ( LspEnv
     -> TargetArgs target
     -> MaybeT (LspM Plugin.Config) [(Range, Metadata, b)]
      )
    -> PluginMethodHandler IdeState TextDocumentCodeLens
codeLensProvider
    sort k state plId
    (CodeLensParams _ _ (TextDocumentIdentifier uri))
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      fromMaybeT (Right $ List []) $ do
        let fc = FileContext
                   { fc_uri   = uri
                   , fc_nfp   = nfp
                   , fc_range = Nothing
                   }
        env <- buildEnv state plId fc
        args <- fetchTargetArgs @target env
        actions <- k env args
        pure
          $ Right
          $ List
          $ fmap (uncurry3 $ makeCodeLens plId sort) actions
codeLensProvider _ _ _ _ _ = pure $ Right $ List []


makeCodeAction
    :: (A.ToJSON b, Show sort)
    => PluginId
    -> sort
    -> Metadata
    -> b
    -> LSP.CodeAction
makeCodeAction plId sort (Metadata title kind preferred) b =
  let cmd_id = CommandId $ T.pack $ show sort
      cmd = mkLspCommand plId cmd_id title $ Just [A.toJSON b]
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

makeCodeLens
    :: (A.ToJSON b, Show sort)
    => PluginId
    -> sort
    -> Range
    -> Metadata
    -> b
    -> LSP.CodeLens
makeCodeLens plId sort range (Metadata title _ _) b =
  let cmd_id = CommandId $ T.pack $ show sort
      cmd = mkLspCommand plId cmd_id title $ Just [A.toJSON b]
   in LSP.CodeLens
        { _range = range
        , _command = Just cmd
        , _xdata = Nothing
        }

-- makeTacticCodeAction
--     :: TacticCommand
--     -> Continuation 'HoleTarget b
-- makeTacticCodeAction cmd =
--   Continuation CodeAction
--     (\LspEnv{..} hj -> do
--       let FileContext{..} = le_fileContext
--       case fc_range of
--         Nothing -> do
--           traceM "Tried to run makeTacticCodeAction but no range was given"
--           pure []
--         Just range -> do
--           undefined
--           lift $ liftIO $ commandProvider cmd $
--             -- TODO(sandy): this is stupid. just use the same env
--             TacticProviderData
--               { tpd_dflags    = le_dflags
--               , tpd_config    = le_config
--               , tpd_plid      = le_pluginId
--               , tpd_uri       = fc_uri
--               , tpd_range     = range
--               , tpd_jdg       = hj_jdg hj
--               , tpd_hole_sort = hj_hole_sort hj
--               }
--     ) undefined

