{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.Rename.ModuleRename (renameModuleDeclaration, applyRenameToImports, Log(..)) where

import           Control.Lens                                 (re)
import           Control.Lens.Getter                          ((^.))
import           Control.Monad                                (guard, (<=<))
import qualified Control.Monad.Extra                          as Maybe
import           Control.Monad.Trans
import           Control.Monad.Trans.Except                   (ExceptT)
import           Control.Monad.Trans.Maybe
import qualified Data.Maybe                                   as Maybe
import qualified Data.Text                                    as T
import           Development.IDE                              (NormalizedFilePath)
import           Development.IDE.Core.FileStore               (getVersionedTextDocForNormalizedFilePath)
import           Development.IDE.Core.PluginUtils             (runActionE)
import           Development.IDE.Core.PositionMapping         (toCurrentRange)
import           Development.IDE.Core.Rules                   hiding (Log)
import qualified Development.IDE.Core.Rules                   as Shake
import           Development.IDE.Core.RuleTypes               (GetModuleGraph (..))
import qualified Development.IDE.Core.Shake                   as Shake
import qualified Development.IDE.GHC.Compat                   as GHC
import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Error                    (realSrcSpanToRange,
                                                               srcSpanToRange)
import           Development.IDE.Import.DependencyInformation (immediateReverseDependencies)
import           Ide.Logger
import           Ide.Plugin.Error
import           Language.LSP.Protocol.Types                  (Range,
                                                               TextDocumentEdit (..),
                                                               TextEdit (..),
                                                               VersionedTextDocumentIdentifier,
                                                               WorkspaceEdit (..),
                                                               _versionedTextDocumentIdentifier,
                                                               fromNormalizedFilePath,
                                                               type (|?) (InL))

data Log
  = CorrectNames [T.Text]
  | LogRenameDependencies T.Text [NormalizedFilePath]
  | NoModuleName NormalizedFilePath
  | LogRenameModuleDeclaration NormalizedFilePath
  deriving (Show)

instance Pretty Log where
  pretty log =
    "ModuleRename." <> case log of
      CorrectNames log -> "CorrectNames" <> colon <+> pretty log
      LogRenameDependencies oldName fps -> "Rename of" <+> pretty oldName <+> "in files:" <+> (pretty $ map fromNormalizedFilePath fps)
      NoModuleName nfp -> "Could not execute rename of" <+> pretty (fromNormalizedFilePath nfp) <+> "as no module path could be determined."
      LogRenameModuleDeclaration nfp -> "Renaming module declaration for file" <+> pretty (fromNormalizedFilePath nfp)

-- | Apply rename to the given module's declaration
--
-- Rename the module in the given file's module declaration.
-- Fails if .
renameModuleDeclaration :: (MonadIO m) => Recorder (WithPriority Log) -> IdeState -> NormalizedFilePath -> T.Text -> ExceptT PluginError m WorkspaceEdit
renameModuleDeclaration recorder ideState oldHaskellFilePath newModulePath = do
  logWith recorder Info $ LogRenameModuleDeclaration oldHaskellFilePath
  verTextDocId <- runActionE "cabal-plugin.getUriContents" ideState $ lift $ getVersionedTextDocForNormalizedFilePath $ oldHaskellFilePath
  rangeToRename <-
    maybeToExceptT PluginStaleResolve $
      MaybeT $
        liftIO $
          moduleNameRange ideState $
            oldHaskellFilePath
  let
    edit = mkTextEditInRange newModulePath verTextDocId rangeToRename
  pure $ WorkspaceEdit Nothing (Just [InL edit]) Nothing

-- | Apply rename to all imports of the given module
--
-- Replaces all imports of the given old module name with the given new module name.
applyRenameToImports ::
  (MonadIO m) =>
  Recorder (WithPriority Log) ->
  IdeState ->
  -- | The module name before the rename.
  T.Text ->
  -- | The new module name after the rename.
  T.Text ->
  -- | The old path to the renamed haskell file.
  NormalizedFilePath ->
  ExceptT e m WorkspaceEdit
applyRenameToImports recorder ideState oldModulePath newModulePath oldHaskellFilePath = do
  moduleGraph <- runActionE "applyRenameToImports" ideState $ lift $ Shake.useNoFile_ GetModuleGraph
  let
    invertedDepsM = immediateReverseDependencies oldHaskellFilePath moduleGraph
  case invertedDepsM of
    Just depFilePaths -> do
      logWith recorder Debug $ LogRenameDependencies oldModulePath depFilePaths
      modImportRanges <- liftIO $ Maybe.mapMaybeM (getRangesForModuleImports ideState oldModulePath) depFilePaths
      let
        textEdits = concatMap (\(verTextDocId, ranges) -> map (mkTextEditInRange newModulePath verTextDocId) ranges) modImportRanges
      pure $ WorkspaceEdit Nothing (Just $ map InL textEdits) Nothing
    Nothing -> pure $ WorkspaceEdit Nothing Nothing Nothing

-- | The module declaration range of the given file path

-- | Determines all ranges in the given file where the module name is imported
--
-- Returns the identifier of the file and a list of ranges of the imports if none of the rule applications fail.
-- Otherwise will return Nothing.
getRangesForModuleImports :: IdeState -> T.Text -> NormalizedFilePath -> IO (Maybe (VersionedTextDocumentIdentifier, [Range]))
getRangesForModuleImports state moduleName nfp = runMaybeT $ do
  verTextDocId <- MaybeT . fmap Just $ runAction "cabal-plugin.getUriContents" state $ getVersionedTextDocForNormalizedFilePath nfp
  (pm, mp) <- MaybeT . runAction "ModuleName.GetParsedModule" state $ Shake.useWithStale GetParsedModule nfp
  let
    allImports = hsmodImports . unLoc $ GHC.pm_parsed_source pm
    modNameImports =
      Maybe.mapMaybe
        (\imp -> GHC.getLoc (ideclName $ GHC.unLoc imp) <$ guard ((== (mkModuleName $ T.unpack moduleName)) . GHC.unLoc . ideclName $ GHC.unLoc imp))
        allImports
  pure $ (verTextDocId, Maybe.mapMaybe (toCurrentRange mp <=< srcSpanToRange) modNameImports)

--
-- Inspired by `codeModuleName` in the hls-module-name-plugin.
moduleNameRange :: Shake.IdeState -> NormalizedFilePath -> IO (Maybe Range)
moduleNameRange state nfp = runMaybeT $ do
  (pm, mp) <- MaybeT . runAction "ModuleName.GetParsedModule" state $ Shake.useWithStale GetParsedModule nfp
  L (locA -> (RealSrcSpan l _)) _ <- MaybeT . pure . hsmodName . unLoc $ GHC.pm_parsed_source pm
  range <- MaybeT . pure $ toCurrentRange mp (realSrcSpanToRange l)
  pure range

mkTextEditInRange :: T.Text -> VersionedTextDocumentIdentifier -> Range -> TextDocumentEdit
mkTextEditInRange newText verTextDocId range =
  TextDocumentEdit (verTextDocId ^. re _versionedTextDocumentIdentifier) $ fmap InL [TextEdit range newText]
