{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wall -Wwarn -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

{- | Keep the module name in sync with its file path.

Provide CodeLenses to:
* Add a module header ("module /moduleName/ where") to empty Haskell files
* Fix the module name if incorrect
-}
module Ide.Plugin.ModuleName (
    descriptor,
    Log,
) where

import           Control.Monad                (forM_, void)
import           Control.Monad.IO.Class       (liftIO)
import           Control.Monad.Trans.Class    (lift)
import           Control.Monad.Trans.Maybe
import           Data.Aeson                   (Value (Null), toJSON)
import           Data.Char                    (isLower)
import qualified Data.HashMap.Strict          as HashMap
import           Data.List                    (intercalate, isPrefixOf,
                                               minimumBy)
import qualified Data.List.NonEmpty           as NE
import           Data.Maybe                   (maybeToList)
import           Data.Ord                     (comparing)
import           Data.String                  (IsString)
import qualified Data.Text                    as T
import           Development.IDE              (GetParsedModule (GetParsedModule),
                                               GhcSession (GhcSession),
                                               IdeState, Pretty,
                                               Priority (Debug), Recorder,
                                               WithPriority, colon, evalGhcEnv,
                                               hscEnvWithImportPaths, logWith,
                                               realSrcSpanToRange, runAction,
                                               uriToFilePath', use, use_, (<+>))
import           Development.IDE.GHC.Compat   (GenLocated (L),
                                               getSessionDynFlags, hsmodName,
                                               importPaths, locA,
                                               moduleNameString,
                                               pattern RealSrcSpan,
                                               pm_parsed_source, unLoc)
import           Development.IDE.Types.Logger (Pretty (..))
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types           hiding
                                              (SemanticTokenAbsolute (length, line),
                                               SemanticTokenRelative (length),
                                               SemanticTokensEdit (_start))
import           Language.LSP.VFS             (virtualFileText)
import           System.Directory             (makeAbsolute)
import           System.FilePath              (dropExtension, normalise,
                                               pathSeparator, splitDirectories,
                                               takeFileName)

-- |Plugin descriptor
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId)
        { pluginHandlers = mkPluginHandler STextDocumentCodeLens (codeLens recorder)
        , pluginCommands = [PluginCommand updateModuleNameCommand "set name of module to match with file path" (command recorder)]
        }

updateModuleNameCommand :: IsString p => p
updateModuleNameCommand = "updateModuleName"

-- | Generate code lenses
codeLens :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'TextDocumentCodeLens
codeLens recorder state pluginId CodeLensParams{_textDocument=TextDocumentIdentifier uri} =
  Right . List . maybeToList . (asCodeLens <$>) <$> action recorder state uri
  where
    asCodeLens :: Action -> CodeLens
    asCodeLens Replace{..} = CodeLens aRange (Just cmd) Nothing
      where
        cmd = mkLspCommand pluginId updateModuleNameCommand aTitle (Just [toJSON aUri])

-- | (Quasi) Idempotent command execution: recalculate action to execute on command request
command :: Recorder (WithPriority Log) -> CommandFunction IdeState Uri
command recorder state uri = do
  actMaybe <- action recorder state uri
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
action :: Recorder (WithPriority Log) -> IdeState -> Uri -> LspM c (Maybe Action)
action recorder state uri =
  runMaybeT $ do
    nfp <- MaybeT . pure . uriToNormalizedFilePath $ toNormalizedUri uri
    fp <- MaybeT . pure $ uriToFilePath' uri

    contents <- lift . getVirtualFile $ toNormalizedUri uri
    let emptyModule = maybe True (T.null . T.strip . virtualFileText) contents

    correctNames <- liftIO $ pathModuleNames recorder state nfp fp
    logWith recorder Debug (CorrectNames correctNames)
    bestName <- minimumBy (comparing T.length) <$> (MaybeT . pure $ NE.nonEmpty correctNames)
    logWith recorder Debug (BestName bestName)

    statedNameMaybe <- liftIO $ codeModuleName state nfp
    logWith recorder Debug (ModuleName $ snd <$> statedNameMaybe)
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
pathModuleNames :: Recorder (WithPriority Log) -> IdeState -> NormalizedFilePath -> FilePath -> IO [T.Text]
pathModuleNames recorder state normFilePath filePath
  | isLower . head $ takeFileName filePath = return ["Main"]
  | otherwise = do
      session <- runAction "ModuleName.ghcSession" state $ use_ GhcSession normFilePath
      srcPaths <- evalGhcEnv (hscEnvWithImportPaths session) $ importPaths <$> getSessionDynFlags
      logWith recorder Debug (SrcPaths srcPaths)

      -- Append a `pathSeparator` to make the path looks like a directory,
      --   and then we can drop it uniformly.
      -- See https://github.com/haskell/haskell-language-server/pull/3092 for details.
      let paths = map (normalise . (<> pure pathSeparator)) srcPaths
      logWith recorder Debug (NormalisedPaths paths)

      mdlPath <- makeAbsolute filePath
      logWith recorder Debug (AbsoluteFilePath mdlPath)

      let prefixes = filter (`isPrefixOf` mdlPath) paths
      pure (map (moduleNameFrom mdlPath) prefixes)
  where
    moduleNameFrom mdlPath prefix =
      T.pack
        . intercalate "."
        . splitDirectories
        . drop (length prefix)
        $ dropExtension mdlPath

-- | The module name, as stated in the module
codeModuleName :: IdeState -> NormalizedFilePath -> IO (Maybe (Range, T.Text))
codeModuleName state nfp = runMaybeT $ do
  pm <- MaybeT . runAction "ModuleName.GetParsedModule" state $ use GetParsedModule nfp
  L (locA -> (RealSrcSpan l _)) m <- MaybeT . pure . hsmodName . unLoc $ pm_parsed_source pm
  pure (realSrcSpanToRange l, T.pack $ moduleNameString m)

data Log =
    CorrectNames [T.Text]
  | BestName T.Text
  | ModuleName (Maybe T.Text)
  | SrcPaths [FilePath]
  | NormalisedPaths [FilePath]
  | AbsoluteFilePath FilePath
  deriving Show

instance Pretty Log where
  pretty log = "ModuleName." <> case log of
    CorrectNames log     -> "CorrectNames" <> colon <+> pretty log
    BestName log         -> "BestName" <> colon <+> pretty log
    ModuleName log       -> "StatedNameMaybe" <> colon <+> pretty log
    SrcPaths log         -> "SrcPaths" <> colon <+> pretty log
    NormalisedPaths log  -> "NormalisedPaths" <> colon <+> pretty log
    AbsoluteFilePath log -> "AbsoluteFilePath" <> colon <+> pretty log
