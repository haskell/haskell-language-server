{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE QuantifiedConstraints  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- | A plugin that uses tactics to synthesize code
module Wingman.AbstractLSP where

import           Control.Monad (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe (MaybeT (MaybeT), mapMaybeT)
import qualified Data.Aeson as A
import           Data.Foldable (traverse_)
import           Data.Text (Text)
import           Development.IDE (IdeState)
import           Development.IDE.Core.UseStale
import           Development.IDE.GHC.Compat hiding (Target)
import           GHC.Generics (Generic)
import qualified Ide.Plugin.Config as Plugin
import           Ide.Types
import           Language.LSP.Server (LspM, sendRequest)
import qualified Language.LSP.Types as LSP
import           Language.LSP.Types hiding (CodeLens, CodeAction)
import           Wingman.EmptyCase (fromMaybeT)
import           Wingman.LanguageServer (judgementForHole, getTacticConfig, getIdeDynflags)
import           Wingman.Types
import qualified Data.Text as T

-- STILL TO DO:
--
-- generalize c_makeCommand so that it produces a 'b' and a 'Metadata'
-- or maybe attach metadata directly to the continuation
--
-- implement code lenses
--
-- and then wire it all up!


data Metadata
  = CodeActionMetadata
      { md_title     :: Text
      , md_kind      :: CodeActionKind
      , md_preferred :: Bool
      }
  | CodeLensMetadata
      { md_title     :: Text
      }
  deriving stock (Eq, Show)


data InteractionSort
  = CodeAction
  | CodeLens
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- TODO(sandy): a is the data we want to fetch on both sides
-- b is the data we share when synthesizing commands to running them
data Continuation sort (a :: Target) b = Continuation
  { c_sort :: sort
  , c_interactionSort :: InteractionSort
  , c_makeCommand
        :: LspEnv
        -> TargetArgs a
           -- TODO(sandy): wrong type. should be more structured, and then call
           -- a high-level function to actually build the command
           --
           -- should produce a 'b'
        -> MaybeT (LspM Plugin.Config) [(Metadata, b)]
  , c_runCommand
        :: LspEnv
        -> TargetArgs a
        -> FileContext
        -> b
        -> MaybeT (LspM Plugin.Config)
                  (Either [UserFacingMessage] WorkspaceEdit)
  }

data Target = HoleTarget | EmptyCaseTarget
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data FileContext = FileContext
  { fc_uri      :: Uri
  , fc_nfp      :: NormalizedFilePath
  , fc_range    :: Maybe (Tracked 'Current Range)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (A.ToJSON, A.FromJSON)

deriving anyclass instance A.ToJSON NormalizedFilePath
deriving anyclass instance A.FromJSON NormalizedFilePath
deriving anyclass instance A.ToJSON NormalizedUri
deriving anyclass instance A.FromJSON NormalizedUri

data LspEnv = LspEnv
  { le_ideState    :: IdeState
  , le_pluginId    :: PluginId
  , le_dflags      :: DynFlags
  , le_config      :: Config
  , le_fileContext :: FileContext
  }

class IsTarget (t :: Target) where
  type TargetArgs t
  fetchTargetArgs
      :: LspEnv
      -> MaybeT (LspM Plugin.Config) (TargetArgs t)

contToCommand :: Continuation sort a b -> PluginCommand IdeState
contToCommand = undefined

buildHandlers
    :: (Show sort, IsTarget a, A.ToJSON b )
    => [Continuation sort a b]
    -> PluginHandlers IdeState
buildHandlers cs =
  flip foldMap cs $ \c ->
    case c_interactionSort c of
      CodeAction -> mkPluginHandler STextDocumentCodeAction $ codeActionProvider c
      CodeLens   -> mkPluginHandler STextDocumentCodeLens $ undefined

instance IsTarget 'HoleTarget where
  type TargetArgs 'HoleTarget = HoleJudgment
  fetchTargetArgs LspEnv{..} = do
    let FileContext{..} = le_fileContext
    range <- MaybeT $ pure fc_range
    mapMaybeT liftIO $ judgementForHole le_ideState fc_nfp range le_config


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
    :: forall sort target b
     . (Show sort, A.ToJSON b, IsTarget target)
    => Continuation sort target b
    -> PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider
    c state plId
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
        actions <- c_makeCommand c env args
        pure $ Right $ List $ fmap (uncurry $ makeCommands plId $ c_sort c) actions
codeActionProvider _ _ _ _ = pure $ Right $ List []


makeCommands
    :: (A.ToJSON b, Show sort)
    => PluginId
    -> sort
    -> Metadata
    -> b
    -> Command |? LSP.CodeAction
makeCommands plId sort (CodeActionMetadata title kind preferred) b =
  let cmd_id = CommandId $ T.pack $ show sort
      cmd = mkLspCommand plId cmd_id title $ Just [A.toJSON b]
   in InR
    $ LSP.CodeAction
        { _title       = title
        , _kind        = Just kind
        , _diagnostics = Nothing
        , _isPreferred = Just preferred
        , _disabled    = Nothing
        , _edit        = Nothing
        , _command     = Just cmd
        , _xdata       = Nothing
        }
makeCommands plId sort (CodeLensMetadata title) b =
  let cmd_id = undefined
      cmd = mkLspCommand plId cmd_id title $ Just [A.toJSON b]
      range = undefined
   -- TODO(sandy): omfg LSP is such an asshole
   in undefined -- InR $ LSP.CodeLens range (Just cmd) Nothing


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

