{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}

module Ide.Plugin.Cabal.CabalAdd
(  findResponsibleCabalFile
 , hiddenPackageAction
 , hiddenPackageSuggestion
 , cabalAddCommand
 , command
 , Log
)
where

import           Control.Monad                                 (filterM, void)
import           Control.Monad.IO.Class                        (MonadIO, liftIO)
import           Data.String                                   (IsString)
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as T
import           Development.IDE                               (IdeState,
                                                                useWithStale)
import           Distribution.PackageDescription.Quirks        (patchQuirks)
import           Ide.PluginUtils                               (WithDeletions (SkipDeletions),
                                                                diffText,
                                                                mkLspCommand)
import           Ide.Types                                     (CommandFunction,
                                                                CommandId (CommandId),
                                                                HandlerM,
                                                                PluginId,
                                                                pluginGetClientCapabilities,
                                                                pluginSendRequest)
import           Language.LSP.Protocol.Types                   (ApplyWorkspaceEditParams (ApplyWorkspaceEditParams),
                                                                ClientCapabilities,
                                                                CodeAction (CodeAction),
                                                                CodeActionKind (CodeActionKind_QuickFix),
                                                                Diagnostic (..),
                                                                Null (Null),
                                                                VersionedTextDocumentIdentifier,
                                                                WorkspaceEdit,
                                                                toNormalizedFilePath,
                                                                type (|?) (InR))
import           System.Directory                              (doesFileExist,
                                                                listDirectory)

import           Control.Monad.Trans.Class                     (lift)
import           Control.Monad.Trans.Except
import           Data.Aeson.Types                              (FromJSON,
                                                                ToJSON, toJSON)
import           Data.ByteString                               (ByteString)
import qualified Data.ByteString.Char8                         as B
import           Data.List.NonEmpty                            (NonEmpty (..),
                                                                fromList)
import           Data.Text.Encoding                            (encodeUtf8)
import           Development.IDE.Core.Rules                    (runAction)
import           Development.IDE.Core.RuleTypes                (GetFileContents (..))
import           Distribution.Client.Add                       as Add
import           Distribution.Compat.Prelude                   (Generic)
import           Distribution.PackageDescription               (GenericPackageDescription,
                                                                packageDescription,
                                                                specVersion)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Pretty                           (pretty)
import           Distribution.Simple.BuildTarget               (BuildTarget,
                                                                buildTargetComponentName,
                                                                readBuildTargets)
import           Distribution.Simple.Utils                     (safeHead)
import           Distribution.Verbosity                        (silent,
                                                                verboseNoStderr)
import qualified Ide.Logger                                    as Logger
import           Ide.Plugin.Cabal.Completion.Types             (ParseCabalFields (..),
                                                                ParseCabalFile (..))
import           Ide.Plugin.Cabal.Orphans                      ()
import           Ide.Plugin.Error
import           Language.LSP.Protocol.Message                 (SMethod (SMethod_WorkspaceApplyEdit))
import           System.FilePath                               (dropFileName,
                                                                makeRelative,
                                                                splitPath,
                                                                takeExtension,
                                                                (</>))
import           Text.PrettyPrint                              (render)
import           Text.Regex.TDFA

data Log
  = LogFoundResponsibleCabalFile FilePath
  | LogCalledCabalAddCodeAction
  | LogCalledCabalAddCommand CabalAddCommandParams
  | LogCreatedEdit WorkspaceEdit
  | LogExecutedCommand
  deriving (Show)

instance Logger.Pretty Log where
    pretty = \case
      LogFoundResponsibleCabalFile fp -> "Located the responsible cabal file at " Logger.<+> Logger.pretty fp
      LogCalledCabalAddCodeAction -> "The CabalAdd CodeAction is called"
      LogCalledCabalAddCommand params -> "Called CabalAdd command with:\n" Logger.<+> Logger.pretty params
      LogCreatedEdit edit -> "Created inplace edit:\n" Logger.<+> Logger.pretty edit
      LogExecutedCommand -> "Executed CabalAdd command"


-- | Gives a code action that calls the command,
--   if a suggestion for a missing dependency is found.
--   Disabled action if no cabal files given.
--   Conducts IO action on a cabal file to find build targets.
hiddenPackageAction :: Logger.Recorder (Logger.WithPriority Log) -> PluginId -> VersionedTextDocumentIdentifier -> Int -> Diagnostic -> FilePath -> FilePath -> GenericPackageDescription -> IO [CodeAction]
hiddenPackageAction recorder plId verTxtDocId maxCompletions diag haskellFilePath cabalFilePath gpd = do
    buildTargets <- liftIO $ getBuildTargets gpd cabalFilePath haskellFilePath
    Logger.logWith recorder Logger.Info LogCalledCabalAddCodeAction
    case buildTargets of
      [] -> pure $ mkCodeAction cabalFilePath Nothing <$> hiddenPackageSuggestion maxCompletions (_message diag)
      targets -> pure $ concat [mkCodeAction cabalFilePath (Just $ buildTargetToStringRepr target) <$>
                          hiddenPackageSuggestion maxCompletions (_message diag) | target <- targets]
  where
    buildTargetToStringRepr target = render $ pretty $ buildTargetComponentName target

    getBuildTargets :: GenericPackageDescription -> FilePath -> FilePath -> IO [BuildTarget]
    getBuildTargets gpd cabalFilePath haskellFilePath = do
      let haskellFileRelativePath = makeRelative (dropFileName cabalFilePath) haskellFilePath
      readBuildTargets (verboseNoStderr silent) (flattenPackageDescription gpd) [haskellFileRelativePath]

    mkCodeAction :: FilePath -> Maybe String -> (T.Text, T.Text) -> CodeAction
    mkCodeAction cabalFilePath target (suggestedDep, suggestedVersion) =
      let
        versionTitle = if T.null suggestedVersion then T.empty else "  version " <> suggestedVersion
        targetTitle = case target of
          Nothing -> T.empty
          Just t  -> "  target " <> T.pack t
        title = "Add dependency " <> suggestedDep <> versionTitle <> targetTitle
        version = if T.null suggestedVersion then Nothing else Just suggestedVersion

        params = CabalAddCommandParams {cabalPath = cabalFilePath
                                      , verTxtDocId = verTxtDocId
                                      , buildTarget = target
                                      , dependency = suggestedDep
                                      , version=version}
        command = mkLspCommand plId (CommandId cabalAddCommand) "Execute Code Action" (Just [toJSON params])
      in CodeAction title (Just CodeActionKind_QuickFix) (Just []) Nothing Nothing Nothing (Just command) Nothing

-- | Gives a mentioned number of hidden packages given
--   a specific error message
hiddenPackageSuggestion :: Int -> T.Text -> [(T.Text, T.Text)]
hiddenPackageSuggestion maxCompletions msg = take maxCompletions $ getMatch (msg =~ regex)
  where
    regex :: T.Text -- TODO: Support multiple packages suggestion
    regex = "It is a member of the hidden package [\8216']([a-z-]+)(-([0-9\\.]*))?[\8217']"
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [(T.Text, T.Text)]
    getMatch (_, _, _, []) = []
    getMatch (_, _, _, [dependency]) = [(dependency, T.empty)]
    getMatch (_, _, _, [dependency, dashedVersion]) = [(dependency, T.empty)] -- failed to get version
    getMatch (_, _, _, [dependency, dashedVersion, cleanVersion]) = [(dependency, cleanVersion)]
    getMatch (_, _, _, _) = error "Impossible pattern matching case"


cabalAddCommand :: IsString p => p
cabalAddCommand = "cabalAdd"

data CabalAddCommandParams =
     CabalAddCommandParams { cabalPath   :: FilePath
                           , verTxtDocId :: VersionedTextDocumentIdentifier
                           , buildTarget :: Maybe String
                           , dependency  :: T.Text
                           , version     :: Maybe T.Text
                           }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Logger.Pretty CabalAddCommandParams where
  pretty CabalAddCommandParams{..} =
    "CabalAdd parameters:\n" Logger.<+>
    "| cabal path: " Logger.<+> Logger.pretty cabalPath Logger.<+> "\n" Logger.<+>
    "| target: " Logger.<+> Logger.pretty buildTarget Logger.<+> "\n" Logger.<+>
    "| dependendency: " Logger.<+> Logger.pretty dependency Logger.<+> "\n" Logger.<+>
    "| version: " Logger.<+> Logger.pretty version Logger.<+> "\n"

command :: Logger.Recorder (Logger.WithPriority Log) -> CommandFunction IdeState CabalAddCommandParams
command recorder state _ params@(CabalAddCommandParams {cabalPath = path, verTxtDocId = verTxtDocId, buildTarget = target, dependency = dep, version = mbVer}) = do
  Logger.logWith recorder Logger.Info $ LogCalledCabalAddCommand params
  let specifiedDep = case mbVer of
        Nothing  -> dep
        Just ver -> dep <> " ^>=" <> ver
  caps <- lift pluginGetClientCapabilities
  let env = (state, caps, verTxtDocId)
  edit <- getDependencyEdit recorder env path target (fromList [T.unpack specifiedDep])
  void $ lift $ pluginSendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  Logger.logWith recorder Logger.Info LogExecutedCommand
  pure $ InR Null

-- | Constructs prerequisets for the @executeConfig@
--   and runs it, given path to the cabal file and a dependency message.
--   Given the new contents of the cabal file constructs and returns the @edit@.
--   Inspired by @main@ in cabal-add,
--   Distribution.Client.Main
getDependencyEdit :: MonadIO m => Logger.Recorder (Logger.WithPriority Log) -> (IdeState, ClientCapabilities, VersionedTextDocumentIdentifier) ->
    FilePath -> Maybe String -> NonEmpty String -> ExceptT PluginError m WorkspaceEdit
getDependencyEdit recorder env cabalFilePath buildTarget dependency = do
  let (state, caps, verTxtDocId) = env
  (mbCnfOrigContents, mbFields, mbPackDescr) <- liftIO $ runAction "cabal.cabal-add" state $ do
        contents <- Development.IDE.useWithStale GetFileContents $ toNormalizedFilePath cabalFilePath
        inFields <- Development.IDE.useWithStale ParseCabalFields $ toNormalizedFilePath cabalFilePath
        inPackDescr <- Development.IDE.useWithStale ParseCabalFile $ toNormalizedFilePath cabalFilePath
        let mbCnfOrigContents = case snd . fst <$> contents of
                    Just (Just txt) -> Just $ encodeUtf8 txt
                    _               -> Nothing
        let mbFields = fst <$> inFields
        let mbPackDescr :: Maybe GenericPackageDescription = fst <$> inPackDescr
        pure (mbCnfOrigContents, mbFields, mbPackDescr)

  (cnfOrigContents, fields, packDescr) <- do
    cnfOrigContents <- case mbCnfOrigContents of
          (Just cnfOrigContents) -> pure cnfOrigContents
          Nothing                -> readCabalFile cabalFilePath
    (fields, packDescr) <- case (mbFields, mbPackDescr) of
          (Just fields, Just packDescr) -> pure (fields, packDescr)
          (_, _) -> case parseCabalFile cabalFilePath cnfOrigContents of
                        Left err   -> throwE $ PluginInternalError $ T.pack $ err
                        Right (f ,gpd) -> pure (f, gpd)
    pure (cnfOrigContents, fields, packDescr)

  let inputs = do
        let rcnfComponent = buildTarget
        let specVer = specVersion $ packageDescription packDescr
        cmp <- resolveComponent cabalFilePath (fields, packDescr) rcnfComponent
        deps <- traverse (validateDependency specVer) dependency
        pure (fields, packDescr, cmp, deps)

  (cnfFields, origPackDescr, cnfComponent, cnfDependencies) <- case inputs of
    Left err   -> throwE $ PluginInternalError $ T.pack err
    Right pair -> pure pair

  case executeConfig (validateChanges origPackDescr) (Config {..}) of
    Nothing -> throwE $ PluginInternalError $ T.pack $ "Cannot extend build-depends in " ++ cabalFilePath
    Just newContents  -> do
              let edit = diffText caps (verTxtDocId, T.decodeUtf8 cnfOrigContents) (T.decodeUtf8 newContents) SkipDeletions
              Logger.logWith recorder Logger.Info $ LogCreatedEdit edit
              pure edit

-- | Given a path to a haskell file, returns the closest cabal file.
--   If cabal file wasn't found, dives Nothing.
findResponsibleCabalFile :: FilePath -> IO (Maybe FilePath)
findResponsibleCabalFile haskellFilePath = do
  let dirPath = dropFileName haskellFilePath
      allDirPaths = reverse $ scanl1 (</>) (splitPath dirPath) -- sorted from most to least specific
  go allDirPaths
  where
    go [] = pure Nothing
    go (path:ps) = do
      objects <- listDirectory path
      let objectsWithPaths = map (\obj -> path <> obj) objects
          objectsCabalExtension = filter (\c -> takeExtension c == ".cabal") objectsWithPaths
      cabalFiles <- filterM (\c -> doesFileExist c) objectsCabalExtension
      case safeHead cabalFiles of
        Nothing        -> go ps
        Just cabalFile -> pure $ Just cabalFile

-- | Gives cabal file's contents or throws error.
--   Inspired by @readCabalFile@ in cabal-add,
--   Distribution.Client.Main
readCabalFile :: MonadIO m => FilePath -> ExceptT PluginError m ByteString
readCabalFile fileName = do
  cabalFileExists <- liftIO $ doesFileExist fileName
  if cabalFileExists
    then snd . patchQuirks <$> liftIO (B.readFile fileName)
    else throwE $ PluginInternalError $ T.pack ("Failed to read cabal file at " <> fileName)
