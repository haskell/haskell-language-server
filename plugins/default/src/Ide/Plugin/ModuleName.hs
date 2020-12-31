{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wall -Wwarn -fno-warn-type-defaults -fno-warn-unused-binds -fno-warn-unused-imports #-}

{- | Keep the module name in sync with its file path.

Provide CodeLenses to:
* Add a module header ("module /moduleName/ where") to empty Haskell files
* Fix the module name if incorrect
-}
module Ide.Plugin.ModuleName (
    descriptor,
) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (
    ToJSON (toJSON),
    Value (Null),
 )
import Data.Char (isLower)
import qualified Data.HashMap.Strict as Map
import Data.List (find, intercalate, isPrefixOf)
import Data.Maybe (maybeToList)
import Data.String (IsString)
import Data.Text (Text, pack)
import qualified Data.Text as T
-- import Debug.Trace (trace)
import Development.IDE (
    GetParsedModule (
        GetParsedModule
    ),
    GhcSession (GhcSession),
    HscEnvEq,
    IdeState,
    List (..),
    NormalizedFilePath,
    Position (Position),
    Range (Range),
    evalGhcEnv,
    hscEnvWithImportPaths,
    realSrcSpanToRange,
    runAction,
    toNormalizedUri,
    uriToFilePath',
    use,
    use_,
 )
import GHC (
    DynFlags (importPaths),
    GenLocated (L),
    HsModule (hsmodName),
    ParsedModule (pm_parsed_source),
    SrcSpan (RealSrcSpan),
    getSessionDynFlags,
    unLoc,
 )
import Ide.PluginUtils (mkLspCmdId, getProcessID)
import Ide.Types (
    CommandFunction,
    PluginCommand (..),
    PluginDescriptor (..),
    PluginId (..),
    defaultPluginDescriptor,
 )
import Language.Haskell.LSP.Core (
    LspFuncs,
    getVirtualFileFunc,
 )
import Language.Haskell.LSP.Types (
    ApplyWorkspaceEditParams (..),
    CodeLens (CodeLens),
    CodeLensParams (CodeLensParams),
    Command (Command),
    ServerMethod (..),
    TextDocumentIdentifier (
        TextDocumentIdentifier
    ),
    TextEdit (TextEdit),
    Uri,
    WorkspaceEdit (..),
    uriToNormalizedFilePath,
 )
import Language.Haskell.LSP.VFS (virtualFileText)
import System.Directory (canonicalizePath)
import System.FilePath (
    dropExtension,
    splitDirectories,
    takeFileName,
 )

-- |Plugin descriptor
descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
    (defaultPluginDescriptor plId)
        { pluginCodeLensProvider = Just codeLens
        , pluginCommands = [PluginCommand editCommandName editCommandName command]
        }

editCommandName :: IsString p => p
editCommandName = "edit"

asCodeLens :: Text -> Action -> CodeLens
asCodeLens cid Replace{..} =
    CodeLens
        aRange
        (Just $ Command aTitle cid (Just (List [toJSON aUri])))
        Nothing

-- | Generate code lenses
codeLens ::
    LspFuncs c ->
    IdeState ->
    PluginId ->
    CodeLensParams ->
    IO (Either a2 (List CodeLens))
codeLens lsp state pluginId (CodeLensParams (TextDocumentIdentifier uri) _) =
    do
        pid <- pack . show <$> getProcessID
        Right . List . maybeToList . (asCodeLens (mkLspCmdId pid pluginId editCommandName) <$>) <$> action lsp state uri

-- | (Quasi) Idempotent command execution: recalculate action to execute on command request
command :: CommandFunction IdeState Uri
command lsp state uri = do
    actMaybe <- action lsp state uri
    return
        ( Right Null
        , (\act -> (WorkspaceApplyEdit, ApplyWorkspaceEditParams $ asEdit act)) <$> actMaybe
        )

-- | A source code change
data Action = Replace {aUri :: Uri, aRange :: Range, aTitle :: Text, aCode :: Text} deriving (Show)

-- | Convert an Action to the corresponding edit operation
asEdit :: Action -> WorkspaceEdit
asEdit act@Replace{..} =
    WorkspaceEdit (Just $ Map.singleton aUri $ List (asTextEdits act)) Nothing

asTextEdits :: Action -> [TextEdit]
asTextEdits Replace{..} = [TextEdit aRange aCode]

-- | Required action (that can be converted to either CodeLenses or CodeActions)
action :: LspFuncs c -> IdeState -> Uri -> IO (Maybe Action)
action lsp state uri =
    traceAs "action" <$> do
        let Just nfp = uriToNormalizedFilePath $ toNormalizedUri uri
        let Just fp = uriToFilePath' uri

        contents <- liftIO $ getVirtualFileFunc lsp $ toNormalizedUri uri
        let emptyModule = maybe True (T.null . T.strip . virtualFileText) contents

        correctNameMaybe <- traceAs "correctName" <$> pathModuleName state nfp fp
        statedNameMaybe <- traceAs "statedName" <$> codeModuleName state nfp

        let act = Replace uri
        let todo = case (correctNameMaybe, statedNameMaybe) of
                (Just correctName, Just (nameRange, statedName))
                    | correctName /= statedName ->
                        Just $
                            act
                                nameRange
                                ("Set module name to " <> correctName)
                                correctName
                (Just correctName, _)
                    | emptyModule ->
                        let code = T.unwords ["module", correctName, "where\n"]
                         in Just $ act (Range (Position 0 0) (Position 0 0)) code code
                _ -> Nothing
        return todo

-- | The module name, as derived by the position of the module in its source directory
pathModuleName :: IdeState -> NormalizedFilePath -> String -> IO (Maybe Text)
pathModuleName state normFilePath filePath
    | isLower (head $ takeFileName filePath) = return $ Just "Main"
    | otherwise = do
        session :: HscEnvEq <- runAction "ModuleName.ghcSession" state $ use_ GhcSession normFilePath
        srcPaths <- evalGhcEnv (hscEnvWithImportPaths session) $ importPaths <$> getSessionDynFlags
        paths <- mapM canonicalizePath srcPaths
        mdlPath <- canonicalizePath filePath
        let maybePrefix = find (`isPrefixOf` mdlPath) paths

        let maybeMdlName =
                ( \prefix ->
                    intercalate "."
                        . splitDirectories
                        . drop (length prefix + 1)
                        $ dropExtension mdlPath
                )
                    <$> maybePrefix
        return $ T.pack <$> maybeMdlName

-- | The module name, as stated in the module
codeModuleName :: IdeState -> NormalizedFilePath -> IO (Maybe (Range, Text))
codeModuleName state nfp =
    ((\(L (RealSrcSpan l) m) -> (realSrcSpanToRange l, T.pack . show $ m)) <$>)
        . ((hsmodName . unLoc . pm_parsed_source) =<<)
        <$> runAction "ModuleName.GetParsedModule" state (use GetParsedModule nfp)

-- traceAs :: Show a => String -> a -> a
-- traceAs lbl a = trace (lbl ++ " = " ++ show a) a

traceAs :: b -> a -> a
traceAs _ a = a
