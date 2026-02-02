{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}


module Ide.Plugin.Cabal.CabalAdd.Rename (
  renameHandler,
  Log,
)
where
import           Control.Applicative
import           Control.Lens                                  (re)
import           Control.Lens.Getter                           ((^.))
import           Control.Monad                                 (guard, (<=<))
import qualified Control.Monad.Extra                           as Maybe
import           Control.Monad.Trans
import           Control.Monad.Trans.Except                    (ExceptT, throwE)
import           Control.Monad.Trans.Maybe
import           Data.ByteString                               (ByteString)
import qualified Data.Map.Strict                               as Map
import qualified Data.Maybe                                    as Maybe
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as T
import           Debug.Trace                                   (traceShowId)
import           Development.IDE.Core.FileStore                (getVersionedTextDocForNormalizedFilePath)
import           Development.IDE.Core.PluginUtils              (runActionE)
import           Development.IDE.Core.PositionMapping          (toCurrentRange)
import           Development.IDE.Core.Rules                    hiding (Log)
import           Development.IDE.Core.RuleTypes                (GetModuleGraph (..))
import qualified Development.IDE.Core.Shake                    as Shake
import qualified Development.IDE.GHC.Compat                    as GHC
import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Error                     (realSrcSpanToRange,
                                                                srcSpanToRange)
import           Development.IDE.Import.DependencyInformation  (immediateReverseDependencies)
import qualified Distribution.Client.Add                       as Add
import           Distribution.Client.Rename                    (RenameConfig (..),
                                                                executeRenameConfig)
import           Distribution.Fields                           (Field)
import qualified Distribution.ModuleName                       as Cabal
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Parsec.Position                  (Position)
import           Distribution.Simple.BuildTarget               (buildTargetComponentName)
import           Ide.Logger
import           Ide.Plugin.Cabal.CabalAdd.CodeAction          (buildInfoToHsSourceDirs,
                                                                getBuildTargets,
                                                                mkRelativeModulePathM)
import           Ide.Plugin.Cabal.Definition                   (lookupBuildTargetPackageDescription)
import           Ide.Plugin.Error
import           Ide.PluginUtils                               (WithDeletions (IncludeDeletions),
                                                                diffText)
import           Language.LSP.Protocol.Types                   (ClientCapabilities,
                                                                NormalizedFilePath,
                                                                Range,
                                                                TextDocumentEdit (..),
                                                                TextEdit (..),
                                                                VersionedTextDocumentIdentifier,
                                                                WorkspaceEdit (WorkspaceEdit),
                                                                _versionedTextDocumentIdentifier,
                                                                fromNormalizedFilePath,
                                                                toNormalizedFilePath,
                                                                type (|?) (InL))

data Log
  = LogDidRename FilePath FilePath
  | LogRenameDependencies T.Text [NormalizedFilePath]
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogDidRename oldFp newFp -> "Received rename info from:" <+> pretty oldFp <+> "to:" <+> pretty newFp
    LogRenameDependencies oldName fps -> "Rename of" <+> pretty oldName <+> "in" <+> (pretty $ map fromNormalizedFilePath fps)

--------------------------------------------
-- Rename module in cabal file
--------------------------------------------

renameHandler ::
  forall m.
  (MonadIO m) =>
  Recorder (WithPriority Log) ->
  Shake.IdeState ->
  (ClientCapabilities, VersionedTextDocumentIdentifier) ->
  -- | the old file path, before the rename
  FilePath ->
  -- | the new file path, after the rename
  FilePath ->
  -- | the path to the cabal file, responsible for the renamed module
  FilePath ->
  -- | the responsible cabal file's contents
  ByteString ->
  -- | the responsible cabal file's fields
  [Field Position] ->
  GenericPackageDescription ->
  ExceptT PluginError m WorkspaceEdit
renameHandler recorder ideState (caps, verTxtDocId) oldHaskellFilePath newHaskellFilePath cabalFilePath cnfOrigContents fields gpd = do
  logWith recorder Info $ LogDidRename oldHaskellFilePath newHaskellFilePath
  let pd = flattenPackageDescription gpd
  compName <- resolveFileTargetE pd cabalFilePath oldHaskellFilePath
  buildInfo <- resolveBuildInfoE pd compName
  newModulePath <- toRelativeModulePathE (buildInfoToHsSourceDirs buildInfo) cabalFilePath newHaskellFilePath
  oldModulePath <- toRelativeModulePathE (buildInfoToHsSourceDirs buildInfo) cabalFilePath oldHaskellFilePath

  cabalFileEdit <- applyModuleRenameToCabalFile recorder (caps, verTxtDocId) oldHaskellFilePath newHaskellFilePath cabalFilePath cnfOrigContents fields gpd
  modDeclEdit <- renameModuleDeclaration recorder ideState oldHaskellFilePath newModulePath
  importEdits <- applyRenameToImports recorder ideState oldModulePath newModulePath $ toNormalizedFilePath oldHaskellFilePath
  pure $ traceShowId $ combineTextEdits importEdits $ combineTextEdits (traceShowId cabalFileEdit) $ traceShowId modDeclEdit

-- | Apply rename to the given module's declaration
--
-- Rename the module in the given file's module declaration.
-- Fails if .
renameModuleDeclaration :: MonadIO m => Recorder (WithPriority Log) -> IdeState -> FilePath -> T.Text -> ExceptT PluginError m WorkspaceEdit
renameModuleDeclaration recorder ideState oldHaskellFilePath newModulePath = do
  verTextDocId <- runActionE "cabal-plugin.getUriContents" ideState $ lift $ getVersionedTextDocForNormalizedFilePath $ toNormalizedFilePath oldHaskellFilePath
  rangeToRename <- maybeToExceptT PluginStaleResolve $
      MaybeT $ liftIO $ moduleNameRange ideState $ toNormalizedFilePath oldHaskellFilePath
  let edit = mkTextEditInRange newModulePath verTextDocId rangeToRename
  pure $ WorkspaceEdit Nothing (Just [InL edit]) Nothing

-- | Apply rename to the given cabal file
--
-- Replaces the module name corresponding to the old file path with the
-- module name corresponding to the new file path in the given cabal file.
-- Fails if the cabal file cannot be parsed, the file paths cannot be parsed to module names
-- or no occurence of the module can be found in the cabal file.
applyModuleRenameToCabalFile ::
  forall m.
  (MonadIO m) =>
  Recorder (WithPriority Log) ->
  (ClientCapabilities, VersionedTextDocumentIdentifier) ->
  -- | the old file path before the rename
  FilePath ->
  -- | the new file path after the rename
  FilePath ->
  -- | the path to the cabal file, responsible for the renamed module
  FilePath ->
  -- | the responsible cabal file's contents
  ByteString ->
  -- | the responsible cabal file's fields
  [Field Position] ->
  GenericPackageDescription ->
  ExceptT PluginError m WorkspaceEdit
applyModuleRenameToCabalFile recorder (caps, verTxtDocId) oldHaskellFilePath newHaskellFilePath cabalFilePath cnfOrigContents fields gpd = do
  let pd = flattenPackageDescription gpd

  compName <- resolveFileTargetE pd cabalFilePath oldHaskellFilePath
  buildInfo <- resolveBuildInfoE pd compName
  newModulePath <- toRelativeModulePathE (buildInfoToHsSourceDirs buildInfo) cabalFilePath newHaskellFilePath
  oldModulePath <- toRelativeModulePathE (buildInfoToHsSourceDirs buildInfo) cabalFilePath oldHaskellFilePath
  targetField <- resolveTargetFieldForComponentE pd oldModulePath compName buildInfo

  newContents <- maybeToExceptT PluginStaleResolve $ hoistMaybe $
    executeRenameConfig (Add.validateChanges gpd) (renameConfig (Right $ compName) targetField oldModulePath newModulePath)
  pure $ diffText caps (verTxtDocId, T.decodeUtf8 cnfOrigContents) (T.decodeUtf8 newContents) IncludeDeletions
  where
    -- define renameConfig to pass to cabal-add
    renameConfig compName targetField from to = RenameConfig
      { cnfOrigContents = cnfOrigContents
      , cnfFields = fields
      , cnfComponent = compName
      , cnfTargetField = targetField
      , cnfRenameFrom = T.encodeUtf8 from
      , cnfRenameTo = T.encodeUtf8 to
      }

-- | Apply rename to all imports of the given module
--
-- Replaces all imports of the given old module name with the given new module name.
applyRenameToImports ::
  MonadIO m =>
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
  let invertedDepsM = immediateReverseDependencies oldHaskellFilePath moduleGraph
  case invertedDepsM of
    Just depFilePaths -> do
      logWith recorder Debug $ LogRenameDependencies oldModulePath depFilePaths
      modImportRanges <- liftIO $ Maybe.mapMaybeM (getRangesForModuleImports ideState oldModulePath) depFilePaths
      let textEdits = concatMap (\(verTextDocId,ranges) -> map (mkTextEditInRange newModulePath verTextDocId) ranges) modImportRanges
      pure $ WorkspaceEdit Nothing (Just $ map InL textEdits) Nothing
    Nothing -> pure $ WorkspaceEdit Nothing Nothing Nothing

mkTextEditInRange :: T.Text ->  VersionedTextDocumentIdentifier -> Range -> TextDocumentEdit
mkTextEditInRange newText verTextDocId range  =
  TextDocumentEdit (verTextDocId  ^. re _versionedTextDocumentIdentifier) $ fmap InL [TextEdit range newText]

-- | Determine the `TargetField` of the given module path
--
-- Takes a module path and which component the module is a part of and returns whether the module is in the
-- exposed- or the other-modules field of the component.
-- If the module is in neither of the fields, returns Nothing.
findFieldForModule :: T.Text -> ComponentName -> PackageDescription -> BuildInfo -> Maybe Add.TargetField
findFieldForModule modulePath compName pd buildInfo =
  let
    exposedMods = case compName of
        CLibName name ->
          case name of
            LMainLibName ->
              Maybe.maybe [] exposedModules $ library pd
            LSubLibName _ ->
              concat $ Maybe.concatMapM (getExposedModulesForLib compName) $ subLibraries pd
        _ -> []
  in
    findInExposedModules exposedMods <|> findInOtherModules (otherModules buildInfo)
  where
    moduleName = Cabal.fromString (T.unpack modulePath)

    findInOtherModules mods =
      Add.OtherModules <$ findInModules mods

    findInExposedModules mods =
      Add.ExposedModules <$ findInModules mods

    findInModules mods =
      guard $ moduleName `elem` mods

    getExposedModulesForLib :: ComponentName -> Library -> Maybe [Cabal.ModuleName]
    getExposedModulesForLib compName library =
      case libName library of
        LSubLibName lName ->
          case componentNameString compName of
            Just unqualCompName -> if lName == unqualCompName then Just $ exposedModules library else Nothing
            _ -> Nothing
        _ -> Nothing

-- | The module declaration range of the given file path
--
-- Inspired by `codeModuleName` in the hls-module-name-plugin.
moduleNameRange :: Shake.IdeState -> NormalizedFilePath -> IO (Maybe Range)
moduleNameRange state nfp = runMaybeT $ do
  (pm, mp) <- MaybeT . runAction "ModuleName.GetParsedModule" state $ Shake.useWithStale GetParsedModule nfp
  L (locA -> (RealSrcSpan l _)) _ <- MaybeT . pure . hsmodName . unLoc $ GHC.pm_parsed_source pm
  range <- MaybeT . pure $ toCurrentRange mp (realSrcSpanToRange l)
  pure range

-- | Determines all ranges in the given file where the module name is imported
--
-- Returns the identifier of the file and a list of ranges of the imports if none of the rule applications fail.
-- Otherwise will return Nothing.
getRangesForModuleImports :: IdeState -> T.Text -> NormalizedFilePath -> IO (Maybe (VersionedTextDocumentIdentifier, [Range]))
getRangesForModuleImports state moduleName nfp = runMaybeT $ do
  verTextDocId <- MaybeT . fmap Just $ runAction "cabal-plugin.getUriContents" state $ getVersionedTextDocForNormalizedFilePath nfp
  (pm, mp) <- MaybeT . runAction "ModuleName.GetParsedModule" state $ Shake.useWithStale GetParsedModule nfp
  let allImports = hsmodImports . unLoc $ GHC.pm_parsed_source pm
      modNameImports = Maybe.mapMaybe
        (\imp -> GHC.getLoc (ideclName $ GHC.unLoc imp) <$ guard ((== (mkModuleName $ T.unpack moduleName)) . GHC.unLoc . ideclName $ GHC.unLoc imp))
        allImports
  pure $ (verTextDocId, Maybe.mapMaybe (toCurrentRange mp <=< srcSpanToRange) modNameImports)

combineTextEdits :: WorkspaceEdit -> WorkspaceEdit -> WorkspaceEdit
combineTextEdits (WorkspaceEdit c1 dc1 ca1) (WorkspaceEdit c2 dc2 ca2) =
    WorkspaceEdit c dc ca
  where
    c = liftA2 (Map.unionWith (<>)) c1 c2 <|> c1 <|> c2
    dc = dc1 <> dc2
    -- We know this might result in information loss due to the monad instance of map,
    -- but we do not expect our use of workspacedit combination to contain two changeAnnotations
    -- for the same edit.
    ca = ca1 <> ca2

---------------------------------------------------------
-- Rule applications with shortcuts to plugin errors
---------------------------------------------------------

-- | Returns the `BuildInfo` for the given component name.
-- If the build info cannot be resolved, throws a PluginError.
resolveBuildInfoE :: Applicative m => PackageDescription -> ComponentName -> ExceptT PluginError m BuildInfo
resolveBuildInfoE pd compName =
  maybeToExceptT PluginStaleResolve $ hoistMaybe $ lookupBuildTargetPackageDescription pd (Just compName)

-- | Determines the `ComponentName` of the given file target.
-- Tries to resolve the file target's component name within the given cabal file.
-- If the component name cannot be uniquely resolved, throws a PluginError,
resolveFileTargetE :: MonadIO m => PackageDescription -> FilePath -> FilePath -> ExceptT PluginError m ComponentName
resolveFileTargetE pd cabalFilePath fileTarget = do
  buildTargets <- liftIO $ getBuildTargets pd cabalFilePath fileTarget
  case buildTargets of
    [buildTarget] -> pure $ buildTargetComponentName buildTarget
    []            -> throwE PluginStaleResolve -- todo maybe handle these two cases differently
    _             -> throwE PluginStaleResolve

-- | Takes a list of source subdirectories, a cabal source path and a haskell filepath
-- and returns a path to the module in exposed module syntax.
--
-- The path will be relative to one of the subdirectories, in case the module is contained within one of them.
-- If no module path can be resolved, throws a PluginError.
toRelativeModulePathE :: Applicative m => [FilePath] -> FilePath -> FilePath -> ExceptT PluginError m T.Text
toRelativeModulePathE sourceDirs cabalFilePath oldHaskellFilePath =
  maybeToExceptT PluginStaleResolve $
    hoistMaybe $ mkRelativeModulePathM sourceDirs cabalFilePath oldHaskellFilePath

-- | Returns the field, a module name is contained in.
--
-- Takes a module name and a component name and returns whether the module name is in exposed- or other-modules of that component.
-- If the module name cannot be found in either field, throws a PluginError.
resolveTargetFieldForComponentE :: Applicative m => PackageDescription -> T.Text -> ComponentName -> BuildInfo -> ExceptT PluginError m Add.TargetField
resolveTargetFieldForComponentE pd oldModulePath compName buildInfo =
  maybeToExceptT PluginStaleResolve $
    hoistMaybe $ findFieldForModule oldModulePath compName pd buildInfo
