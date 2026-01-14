{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}


module Ide.Plugin.Cabal.CabalAdd.Rename (
  renameHandler,
  Log,
)
where
import           Control.Lens                                  (re)
import           Control.Lens.Getter                           ((^.))
import           Control.Monad.IO.Class                        (MonadIO, liftIO)
import           Control.Monad.Trans
import           Control.Monad.Trans.Except                    (ExceptT, throwE)
import           Control.Monad.Trans.Maybe
import           Data.ByteString                               (ByteString)
import qualified Data.Maybe                                    as Maybe
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as T
import           Debug.Trace                                   (traceShowId, traceShowM)
import           Development.IDE.Core.PositionMapping          (toCurrentRange)
import           Development.IDE.Core.Rules                    hiding (Log)
import qualified Development.IDE.Core.Shake                    as Shake
import qualified Development.IDE.GHC.Compat                    as GHC
import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Error                     (realSrcSpanToRange)
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
                                                                VersionedTextDocumentIdentifier,
                                                                WorkspaceEdit (WorkspaceEdit),
                                                                _versionedTextDocumentIdentifier,
                                                                toNormalizedFilePath,
                                                                type (|?) (InL), TextEdit (..))
import Control.Applicative
import qualified Data.Map.Strict as Map
import Development.IDE.Core.FileStore (getVersionedTextDocForNormalizedFilePath)
import Development.IDE.Core.PluginUtils (runActionE)

data Log =
  LogDidRename FilePath FilePath
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogDidRename oldFp newFp -> "Received rename info from:" <+> pretty oldFp <+> "to:" <+> pretty newFp

--------------------------------------------
-- Rename module in cabal file
--------------------------------------------

renameHandler ::
  forall m.
  (MonadIO m) =>
  Recorder (WithPriority Log) ->
  Shake.IdeState ->
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
renameHandler recorder ideState (caps, verTxtDocId) oldHaskellFilePath newHaskellFilePath cabalFilePath cnfOrigContents fields gpd = do
  let pd = flattenPackageDescription gpd
  compName <- resolveFileTargetE pd cabalFilePath oldHaskellFilePath
  buildInfo <- resolveBuildInfoE pd compName
  newModulePath <- toRelativeModulePathE (buildInfoToHsSourceDirs buildInfo) cabalFilePath newHaskellFilePath
  cabalFileEdit <- applyModuleRenameToCabalFile recorder (caps, verTxtDocId) oldHaskellFilePath newHaskellFilePath cabalFilePath cnfOrigContents fields gpd
  modDeclEdit <- renameModuleDeclaration recorder ideState oldHaskellFilePath newHaskellFilePath newModulePath
  pure $ traceShowId $ combineTextEdits (traceShowId cabalFileEdit) $ traceShowId modDeclEdit

renameModuleDeclaration :: MonadIO m => Recorder (WithPriority Log) -> IdeState -> FilePath -> FilePath -> T.Text -> ExceptT PluginError m WorkspaceEdit
renameModuleDeclaration recorder ideState oldHaskellFilePath newHaskellFilePath newModulePath = do
  verTextDocId <- runActionE "cabal-plugin.getUriContents" ideState $ lift $ getVersionedTextDocForNormalizedFilePath $ toNormalizedFilePath oldHaskellFilePath
  rangeToRename <- maybeToExceptT PluginStaleResolve $
      MaybeT $ liftIO $ moduleNameRange ideState $ toNormalizedFilePath oldHaskellFilePath
  let diff = TextEdit rangeToRename newModulePath
      renameEdit = TextDocumentEdit (verTextDocId  ^. re _versionedTextDocumentIdentifier) $ fmap InL [diff]
  pure $ WorkspaceEdit Nothing (Just [InL renameEdit]) Nothing

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
  logWith recorder Info $ LogDidRename oldHaskellFilePath newHaskellFilePath
  let pd = flattenPackageDescription gpd

  compName <- resolveFileTargetE pd cabalFilePath oldHaskellFilePath
  buildInfo <- resolveBuildInfoE pd compName
  newModulePath <- toRelativeModulePathE (buildInfoToHsSourceDirs buildInfo) cabalFilePath newHaskellFilePath
  oldModulePath <- toRelativeModulePathE (buildInfoToHsSourceDirs buildInfo) cabalFilePath oldHaskellFilePath
  targetField <- resolveTargetFieldForComponentE oldModulePath compName pd buildInfo

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
-- The path will be relative to one of the subdirectories, in case the module is contained within one of them.
-- If no module path can be resolved, throws a PluginError.
toRelativeModulePathE :: Applicative m => [FilePath] -> FilePath -> FilePath -> ExceptT PluginError m T.Text
toRelativeModulePathE sourceDirs cabalFilePath oldHaskellFilePath =
  maybeToExceptT PluginStaleResolve $
    hoistMaybe $ mkRelativeModulePathM sourceDirs cabalFilePath oldHaskellFilePath

-- | Returns the field, a module name is contained in.
-- Takes a module name and a component name and returns whether the module name is in exposed- or other-modules of that component.
-- If the module name cannot be found in either field, throws a PluginError.
resolveTargetFieldForComponentE :: Applicative m => T.Text -> ComponentName -> PackageDescription -> BuildInfo -> ExceptT PluginError m Add.TargetField
resolveTargetFieldForComponentE oldModulePath compName pd buildInfo =
  maybeToExceptT PluginStaleResolve $
    hoistMaybe $ findFieldForModule oldModulePath compName pd buildInfo

-- | Determine the `TargetField` of the given module path
--
-- Takes a module path and which component the module is a part of and returns whether the module is in the
-- exposed- or the other-modules field of the component.
-- If the module is in neither of the fields, returns Nothing.
findFieldForModule :: T.Text -> ComponentName -> PackageDescription -> BuildInfo -> Maybe Add.TargetField
findFieldForModule modulePath compName pd buildInfo =
  case compName of
    CLibName name ->
      case name of
        LMainLibName ->
          if moduleName `elem` (Maybe.maybe [] exposedModules $ library pd)
            then Just Add.ExposedModules
            else
              if moduleName `elem` (otherModules buildInfo)
                then Just Add.OtherModules
                else Nothing
        LSubLibName _ ->
          if moduleName `elem` (concat $ Maybe.mapMaybe (getExposedModulesForLib compName) $ subLibraries pd)
            then Just Add.ExposedModules
            else
              if moduleName `elem` (otherModules buildInfo) -- todo deduplicate
                then Just Add.OtherModules
                else Nothing
    _ ->
      if moduleName `elem` (otherModules buildInfo)
        then Just Add.OtherModules
        else Nothing
  where
    moduleName = Cabal.fromString (T.unpack modulePath)
    getExposedModulesForLib :: ComponentName -> Library -> Maybe [Cabal.ModuleName]
    getExposedModulesForLib compName library =
      case libName library of
        LSubLibName lName ->
          case componentNameString compName of
            Just unqualCompName -> if lName == unqualCompName then Just $ exposedModules library else Nothing
            _ -> Nothing
        _ -> Nothing

-- | The module declaration range of the given file path
-- inspired by `codeModuleName` in the hls-module-name-plugin
moduleNameRange :: Shake.IdeState -> NormalizedFilePath -> IO (Maybe Range)
moduleNameRange state nfp = runMaybeT $ do
  (pm, mp) <- MaybeT . runAction "ModuleName.GetParsedModule" state $ Shake.useWithStale GetParsedModule nfp
  L (locA -> (RealSrcSpan l _)) _ <- MaybeT . pure . hsmodName . unLoc $ GHC.pm_parsed_source pm
  range <- MaybeT . pure $ toCurrentRange mp (realSrcSpanToRange l)
  pure range


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
