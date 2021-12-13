{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wall -Wwarn -fno-warn-type-defaults #-}

{- | Keep the module name in sync with its file path.

Provide CodeLenses to:
* Add a module header ("module /moduleName/ where") to empty Haskell files
* Fix the module name if incorrect
-}
module Ide.Plugin.ModuleName (
    descriptor,
) where

import           Control.Monad              (forM_, void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Maybe
import           Data.Aeson                 (Value (Null), toJSON)
import           Data.Char                  (isLower)
import qualified Data.HashMap.Strict        as HashMap
import           Data.List                  (intercalate, isPrefixOf, minimumBy)
import           Data.Maybe                 (maybeToList)
import           Data.Ord                   (comparing)
import           Data.String                (IsString)
import qualified Data.Text                  as T
import           Development.IDE            (GetParsedModule (GetParsedModule),
                                             GhcSession (GhcSession), IdeState,
                                             evalGhcEnv, hscEnvWithImportPaths,
                                             realSrcSpanToRange, runAction,
                                             uriToFilePath', use, use_)
import           Development.IDE.GHC.Compat (GenLocated (L), getSessionDynFlags,
                                             hsmodName, importPaths,
                                             pattern RealSrcSpan,
                                             pm_parsed_source, unLoc)
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types         hiding
                                            (SemanticTokenAbsolute (length, line),
                                             SemanticTokenRelative (length),
                                             SemanticTokensEdit (_start))
import           Language.LSP.VFS           (virtualFileText)
import           System.Directory           (makeAbsolute)
import           System.FilePath            (dropExtension, splitDirectories,
                                             takeFileName)

-- |Plugin descriptor
descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
    (defaultPluginDescriptor plId)
        { pluginHandlers = mkPluginHandler STextDocumentCodeLens codeLens
        , pluginCommands = [PluginCommand updateModuleNameCommand "set name of module to match with file path" command]
        }

updateModuleNameCommand :: IsString p => p
updateModuleNameCommand = "updateModuleName"

-- | Generate code lenses
codeLens :: PluginMethodHandler IdeState 'TextDocumentCodeLens
codeLens state pluginId CodeLensParams{_textDocument=TextDocumentIdentifier uri} =
  Right . List . maybeToList . (asCodeLens <$>) <$> action state uri
  where
    asCodeLens :: Action -> CodeLens
    asCodeLens Replace{..} = CodeLens aRange (Just cmd) Nothing
      where
        cmd = mkLspCommand pluginId updateModuleNameCommand aTitle (Just [toJSON aUri])

-- | (Quasi) Idempotent command execution: recalculate action to execute on command request
command :: CommandFunction IdeState Uri
command state uri = do
  actMaybe <- action state uri
  forM_ actMaybe $ \Replace{..} ->
    let
      -- | Convert an Action to the corresponding edit operation
      edit = WorkspaceEdit (Just . HashMap.singleton aUri $ List [TextEdit aRange aCode]) Nothing Nothing
    in
      void $ sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (const (pure ()))
  pure $ Right Null

-- | A source code change
data Action = Replace
  { aUri   :: Uri
  , aRange :: Range
  , aTitle :: T.Text
  , aCode  :: T.Text
  }
  deriving (Show)

-- | Required action (that can be converted to either CodeLenses or CodeActions)
action :: IdeState -> Uri -> LspM c (Maybe Action)
action state uri =
  traceAs "action" <$> runMaybeT $ do
    nfp <- MaybeT . pure . uriToNormalizedFilePath $ toNormalizedUri uri
    fp <- MaybeT . pure $ uriToFilePath' uri

    contents <- lift . getVirtualFile $ toNormalizedUri uri
    let emptyModule = maybe True (T.null . T.strip . virtualFileText) contents

    correctNames <- liftIO $ traceAs "correctNames" <$> pathModuleNames state nfp fp
    let bestName = minimumBy (comparing T.length) correctNames

    statedNameMaybe <- liftIO $ traceAs "statedName" <$> codeModuleName state nfp
    case statedNameMaybe of
      Just (nameRange, statedName)
        | statedName `notElem` correctNames ->
            pure $ Replace uri nameRange ("Set module name to " <> bestName) bestName
      Nothing
        | emptyModule ->
            let code = "module " <> bestName <> " where\n"
            in pure $ Replace uri (Range (Position 0 0) (Position 0 0)) code code
      _ -> MaybeT $ pure Nothing

-- | Possible module names, as derived by the position of the module in the
-- source directories.  There may be more than one possible name, if the source
-- directories are nested inside each other.
pathModuleNames :: IdeState -> NormalizedFilePath -> String -> IO [T.Text]
pathModuleNames state normFilePath filePath
  | isLower . head $ takeFileName filePath = return ["Main"]
  | otherwise = do
      session <- runAction "ModuleName.ghcSession" state $ use_ GhcSession normFilePath
      srcPaths <- evalGhcEnv (hscEnvWithImportPaths session) $ importPaths <$> getSessionDynFlags
      paths <- mapM makeAbsolute srcPaths
      mdlPath <- makeAbsolute filePath
      let prefixes = filter (`isPrefixOf` mdlPath) paths
      pure (map (moduleNameFrom mdlPath) prefixes)
  where
    moduleNameFrom mdlPath prefix =
      T.pack
        . intercalate "."
        . splitDirectories
        . drop (length prefix + 1)
        $ dropExtension mdlPath

-- | The module name, as stated in the module
codeModuleName :: IdeState -> NormalizedFilePath -> IO (Maybe (Range, T.Text))
codeModuleName state nfp = runMaybeT $ do
  pm <- MaybeT . runAction "ModuleName.GetParsedModule" state $ use GetParsedModule nfp
  L (RealSrcSpan l _) m <- MaybeT . pure . hsmodName . unLoc $ pm_parsed_source pm
  pure (realSrcSpanToRange l, T.pack $ show m)

-- traceAs :: Show a => String -> a -> a
-- traceAs lbl a = trace (lbl ++ " = " ++ show a) a

traceAs :: b -> a -> a
traceAs _ a = a
