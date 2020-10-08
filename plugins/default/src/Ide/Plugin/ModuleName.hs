{-# OPTIONS_GHC -Wwarn -fno-warn-type-defaults -fno-warn-unused-binds -fno-warn-unused-imports #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- |Keep the module name in sync with its file path.
module Ide.Plugin.ModuleName

  ( descriptor
  )
where

import           Control.Monad              (join)
import           Control.Monad.IO.Class     (MonadIO (liftIO))
import           Control.Monad.Trans.Maybe  ()
import           Data.Aeson                 (ToJSON (toJSON), Value (Null))
import qualified Data.HashMap.Strict        as Map
import           Data.List                  (isPrefixOf)
import           Data.List.Extra            (replace)
import           Data.Maybe                 (listToMaybe)
import           Data.String                (IsString)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Development.IDE            (GetParsedModule (GetParsedModule),
                                             GhcSession (GhcSession),
                                             HscEnvEq (hscEnv), IdeState,
                                             List (..), NormalizedFilePath,
                                             Position (Position), Range (Range),
                                             evalGhcEnv, realSrcSpanToRange,
                                             runAction, toNormalizedUri,
                                             uriToFilePath', use, use_)
import           Development.IDE.Plugin     (getPid)
import           GHC                        (DynFlags (importPaths),
                                             GenLocated (L),
                                             GhcMonad (getSession),
                                             HsModule (hsmodName),
                                             ParsedModule (pm_parsed_source),
                                             SrcSpan (RealSrcSpan), unLoc)
import           GhcPlugins                 (HscEnv (hsc_IC),
                                             InteractiveContext (ic_dflags))
import           Ide.Types                  (CommandFunction, CommandId (..),
                                             PluginCommand (..),
                                             PluginDescriptor (..),
                                             PluginId (..),
                                             defaultPluginDescriptor)
import           Language.Haskell.LSP.Core  (LspFuncs, getVirtualFileFunc)
import           Language.Haskell.LSP.Types (ApplyWorkspaceEditParams (..),
                                             CAResult (CACodeAction),
                                             CodeAction (CodeAction),
                                             CodeActionKind (CodeActionQuickFix),
                                             CodeLens (CodeLens),
                                             CodeLensParams (CodeLensParams),
                                             Command (Command),
                                             ServerMethod (..),
                                             TextDocumentIdentifier (TextDocumentIdentifier),
                                             TextEdit (TextEdit), Uri,
                                             WorkspaceEdit (..),
                                             uriToNormalizedFilePath)
import           Language.Haskell.LSP.VFS   (virtualFileText)
import           System.FilePath            (dropExtension)

-- |Plugin descriptor
descriptor :: PluginId -> PluginDescriptor
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginId = plId,
        pluginCodeLensProvider = Just codeLens
       ,pluginCommands = [PluginCommand editCommandName editCommandName editCmd]
        -- pluginCodeActionProvider = Just codeAction
    }

-- | Generate code lenses
codeLens :: LspFuncs c -> IdeState -> PluginId -> CodeLensParams -> IO (Either a2 (List CodeLens))
codeLens lsp state pluginId (CodeLensParams (TextDocumentIdentifier uri) _) = do
  pid <- getPid
  actions (asCodeLens (mkLspCmdId pid pluginId editCommandName)) lsp state uri

-- | Generate code actions.
-- NOTE: Not invoked on an empty module (but codeLens is, why?)
codeAction :: LspFuncs c -> IdeState -> p1 -> TextDocumentIdentifier -> p2 -> p3 -> IO (Either a (List CAResult))
codeAction lsp state _plId (TextDocumentIdentifier uri) _range _ = actions asCodeAction lsp state uri

-- Copied from "Ide.Plugin"
mkLspCmdId :: T.Text -> PluginId -> CommandId -> T.Text
mkLspCmdId pid (PluginId plid) (CommandId cid)
  = pid <> ":" <> plid <> ":" <> cid

editCommandName :: IsString p => p
editCommandName = "edit"

-- | Generic command to apply a group of edits
editCmd :: CommandFunction WorkspaceEdit
editCmd _lf _ide workspaceEdits = return (Right Null, Just $ (WorkspaceApplyEdit,ApplyWorkspaceEditParams workspaceEdits))

-- | Required actions (actually, at most one) that can be converted to either CodeLenses or CodeActions
actions :: Show a1 => (Action -> a1) -> LspFuncs c -> IdeState -> Uri -> IO (Either a2 (List a1))
actions convert lsp state uri = do
    let Just nfp = uriToNormalizedFilePath $ toNormalizedUri uri
    let Just fp = uriToFilePath' uri

    contents <- liftIO $ getVirtualFileFunc lsp $ toNormalizedUri uri
    let emptyModule = maybe True ((==0) . T.length . T.strip . virtualFileText) contents

    correctNameMaybe <- pathModuleName state nfp fp
    statedNameMaybe <- codeModuleName state nfp

    let act = Action uri
    let actions = case (correctNameMaybe,statedNameMaybe) of
            (Just correctName,Just (nameRange,statedName)) | correctName /= statedName -> [convert $ act nameRange ("Set module name to " <> correctName)  correctName]
            (Just correctName,_) | emptyModule -> let code = T.unwords ["module",correctName,"where\n"] in [convert $ act (Range (Position 0 0) (Position 0 0)) code code]
            _ -> []

    out ["actions",show actions]
    pure . Right . List $ actions

-- | The module name, as derived by the position of the module in its source directory
pathModuleName :: IdeState -> NormalizedFilePath -> String -> IO (Maybe Text)
pathModuleName state nfp fp =  do
    session :: HscEnvEq <- runAction "ModuleName.ghcSession" state $ use_ GhcSession nfp
    paths <- evalGhcEnv (hscEnv session) $ do
        env <- getSession
        let df = ic_dflags . hsc_IC  $ env
        return $ importPaths df
    out ["import paths",show paths]
    let maybePrefix = listToMaybe .  filter (`isPrefixOf` fp) $ paths
    out ["prefix",show maybePrefix]
    let maybeMdlName = (\prefix -> replace "/" "."  . drop (length prefix+1) $ dropExtension fp) <$> maybePrefix
    out ["mdlName",show maybeMdlName]
    return $ T.pack <$> maybeMdlName

-- | The module name, as stated in the module
codeModuleName :: IdeState -> NormalizedFilePath -> IO (Maybe (Range, Text))
codeModuleName state nfp = ((\(L (RealSrcSpan l) m) -> (realSrcSpanToRange l,T.pack . show $ m)) <$>) . join . (hsmodName . unLoc . pm_parsed_source <$>) <$> runAction "ModuleName.GetParsedModule" state (use GetParsedModule nfp)

-- | A source code change
data Action = Action {aUri::Uri,aRange::Range,aTitle::Text,aCode::Text} deriving Show

-- | Convert an Action to a CodeLens
asCodeLens :: Text -> Action -> CodeLens
asCodeLens cid act@Action{..} = CodeLens aRange (Just $ Command aTitle cid (Just (List [toJSON $ asEdit act]))) Nothing

-- | Convert an Action to a CodeAction
asCodeAction :: Action -> CAResult
asCodeAction act@Action{..} = CACodeAction $ CodeAction aTitle (Just CodeActionQuickFix) (Just $ List []) (Just $ asEdit act) Nothing
-- -- [TextDocumentEdit (VersionedTextDocumentIdentifier testUri (Just 0)) expectedTextEdits]

asEdit :: Action -> WorkspaceEdit
asEdit act@Action{..} = WorkspaceEdit (Just $ Map.singleton aUri $ List (asTextEdits act) ) Nothing

asTextEdits :: Action -> [TextEdit]
asTextEdits Action{..} = [TextEdit aRange aCode]

out :: [String] -> IO ()
out = print . unwords . ("Plugin ModuleName " :)
-- out _ = return ()
