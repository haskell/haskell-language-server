{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}


module Ide.Plugin.Cabal.CabalAdd.Rename (
  renameHandler,
  Log,
)
where
import           Control.Applicative
import           Control.Monad                                 (guard)
import qualified Control.Monad.Extra                           as Maybe
import           Control.Monad.Trans
import           Control.Monad.Trans.Except                    (ExceptT, throwE)
import           Control.Monad.Trans.Maybe
import           Data.ByteString                               (ByteString)
import qualified Data.Maybe                                    as Maybe
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as T
import qualified Development.IDE.Core.Shake                    as Shake
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
                                                                VersionedTextDocumentIdentifier,
                                                                WorkspaceEdit)
data Log
  = LogDidRename FilePath FilePath
  | CabalRenameLog T.Text T.Text
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogDidRename oldFp newFp -> "Received rename info from:" <+> pretty oldFp <+> "to:" <+> pretty newFp
    CabalRenameLog oldModulePath newModulePath -> "Executing rename of module from" <+> pretty oldModulePath <+> "to" <+> pretty newModulePath <+> "in cabal file."

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
renameHandler recorder _ (caps, verTxtDocId) oldHaskellFilePath newHaskellFilePath cabalFilePath cnfOrigContents fields gpd = do
  logWith recorder Info $ LogDidRename oldHaskellFilePath newHaskellFilePath
  cabalFileEdit <- applyModuleRenameToCabalFile recorder (caps, verTxtDocId) oldHaskellFilePath newHaskellFilePath cabalFilePath cnfOrigContents fields gpd
  pure cabalFileEdit

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
  logWith recorder Info $ CabalRenameLog oldModulePath newModulePath
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
    getExposedModulesForLib compName lib =
      case libName lib of
        LSubLibName lName ->
          case componentNameString compName of
            Just unqualCompName -> if lName == unqualCompName then Just $ exposedModules lib else Nothing
            _ -> Nothing
        _ -> Nothing


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

-- | Returns the field a module name is contained in.
--
-- Takes a module name and a component name and returns whether the module name is in exposed- or other-modules of that component.
-- If the module name cannot be found in either field, throws a PluginError.
resolveTargetFieldForComponentE :: Applicative m => PackageDescription -> T.Text -> ComponentName -> BuildInfo -> ExceptT PluginError m Add.TargetField
resolveTargetFieldForComponentE pd oldModulePath compName buildInfo =
  maybeToExceptT PluginStaleResolve $
    hoistMaybe $ findFieldForModule oldModulePath compName pd buildInfo

-- | Takes a list of source subdirectories, a cabal source path and a haskell filepath
-- and returns a path to the module in exposed module syntax.
--
-- The path will be relative to one of the subdirectories, in case the module is contained within one of them.
-- If no module path can be resolved, throws a PluginError.
toRelativeModulePathE :: Applicative m => [FilePath] -> FilePath -> FilePath -> ExceptT PluginError m T.Text
toRelativeModulePathE sourceDirs cabalFilePath oldHaskellFilePath =
  maybeToExceptT PluginStaleResolve $ hoistMaybe $ mkRelativeModulePathM sourceDirs cabalFilePath oldHaskellFilePath
