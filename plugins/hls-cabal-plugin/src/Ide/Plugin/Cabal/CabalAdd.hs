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
 , addDependencySuggestCodeAction
 , hiddenPackageSuggestion
 , cabalAddCommand
 , command
 , Log
)
where

import           Control.Monad                                 (filterM, void)
import           Control.Monad.IO.Class                        (MonadIO, liftIO)
import           Control.Monad.Trans.Class                     (lift)
import           Control.Monad.Trans.Except
import           Data.Aeson.Types                              (FromJSON,
                                                                ToJSON, toJSON)
import           Data.ByteString                               (ByteString)
import qualified Data.ByteString.Char8                         as B
import           Data.List.NonEmpty                            (NonEmpty (..),
                                                                fromList)
import           Data.String                                   (IsString)
import qualified Data.Text                                     as T
import           Data.Text.Encoding                            (encodeUtf8)
import qualified Data.Text.Encoding                            as T
import           Data.Text.Utf16.Rope.Mixed                    as Rope
import           Development.IDE                               (IdeState,
                                                                getFileContents,
                                                                useWithStale)
import           Development.IDE.Core.Rules                    (runAction)
import           Distribution.Client.Add                       as Add
import           Distribution.Compat.Prelude                   (Generic)
import           Distribution.PackageDescription               (GenericPackageDescription,
                                                                packageDescription,
                                                                specVersion)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.PackageDescription.Quirks        (patchQuirks)
import qualified Distribution.Pretty                           as Pretty
import           Distribution.Simple.BuildTarget               (BuildTarget,
                                                                buildTargetComponentName,
                                                                readBuildTargets)
import           Distribution.Simple.Utils                     (safeHead)
import           Distribution.Verbosity                        (silent,
                                                                verboseNoStderr)
import           Ide.Logger
import           Ide.Plugin.Cabal.Completion.Types             (ParseCabalFields (..),
                                                                ParseCabalFile (..))
import           Ide.Plugin.Cabal.Orphans                      ()
import           Ide.Plugin.Error
import           Ide.PluginUtils                               (WithDeletions (SkipDeletions),
                                                                diffText,
                                                                mkLspCommand)
import           Ide.Types                                     (CommandFunction,
                                                                CommandId (CommandId),
                                                                PluginId,
                                                                pluginGetClientCapabilities,
                                                                pluginSendRequest)
import           Language.LSP.Protocol.Message                 (SMethod (SMethod_WorkspaceApplyEdit))
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
import           System.FilePath                               (dropFileName,
                                                                makeRelative,
                                                                splitPath,
                                                                takeExtension,
                                                                (</>))
import           Text.PrettyPrint                              (render)
import           Text.Regex.TDFA

data Log
  = LogFoundResponsibleCabalFile FilePath
  | LogCalledCabalAddCommand CabalAddCommandParams
  | LogCreatedEdit WorkspaceEdit
  | LogExecutedCommand
  deriving (Show)

instance Pretty Log where
    pretty = \case
      LogFoundResponsibleCabalFile fp -> "Located the responsible cabal file at " <+> pretty fp
      LogCalledCabalAddCommand params -> "Called CabalAdd command with:\n" <+> pretty params
      LogCreatedEdit edit -> "Created inplace edit:\n" <+> pretty edit
      LogExecutedCommand -> "Executed CabalAdd command"

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

instance Pretty CabalAddCommandParams where
  pretty CabalAddCommandParams{..} =
    "CabalAdd parameters:" <+> vcat
      [ "cabal path:" <+> pretty cabalPath
      , "target:" <+> pretty buildTarget
      , "dependendency:" <+> pretty dependency
      , "version:" <+> pretty version
      ]

-- | Creates a code action that calls the `cabalAddCommand`,
--   using dependency-version suggestion pairs as input.
--
--   Returns disabled action if no cabal files given.
--
--   Takes haskell file and cabal file paths to create a relative path
--   to the haskell file, which is used to get a `BuildTarget`.
--
--   In current implementation the dependency is being added to the main found
--   build target, but if there will be a way to get all build targets from a file
--   it will be possible to support addition to a build target of choice.
addDependencySuggestCodeAction
  :: PluginId
  -> VersionedTextDocumentIdentifier -- ^ Cabal's versioned text identifier
  -> [(T.Text, T.Text)] -- ^ A dependency-version suggestion pairs
  -> FilePath -- ^ Path to the haskell file (source of diagnostics)
  -> FilePath -- ^ Path to the cabal file (that will be edited)
  -> GenericPackageDescription
  -> IO [CodeAction]
addDependencySuggestCodeAction plId verTxtDocId suggestions haskellFilePath cabalFilePath gpd = do
    buildTargets <- liftIO $ getBuildTargets gpd cabalFilePath haskellFilePath
    case buildTargets of
      -- If there are no build targets found, run `cabal-add` command with default behaviour
      [] -> pure $ mkCodeAction cabalFilePath Nothing <$> suggestions
      -- Otherwise provide actions for all found targets
      targets -> pure $ concat [mkCodeAction cabalFilePath (Just $ buildTargetToStringRepr target) <$>
                                                            suggestions | target <- targets]
  where
    -- | Note the use of `pretty` function.
    -- It converts the `BuildTarget` to an acceptable string representation.
    -- It will be used in as the input for `cabal-add`'s `executeConfig`.
    buildTargetToStringRepr target = render $ Pretty.pretty $ buildTargetComponentName target

    -- | Gives the build targets that are used in the `CabalAdd`.
    -- Note the unorthodox usage of `readBuildTargets`:
    -- If the relative path to the haskell file is provided,
    -- the `readBuildTargets` will return build targets, where this
    -- module is mentioned (in exposed-modules or other-modules).
    getBuildTargets :: GenericPackageDescription -> FilePath -> FilePath -> IO [BuildTarget]
    getBuildTargets gpd cabalFilePath haskellFilePath = do
      let haskellFileRelativePath = makeRelative (dropFileName cabalFilePath) haskellFilePath
      readBuildTargets (verboseNoStderr silent) (flattenPackageDescription gpd) [haskellFileRelativePath]

    mkCodeAction :: FilePath -> Maybe String -> (T.Text, T.Text) -> CodeAction
    mkCodeAction cabalFilePath target (suggestedDep, suggestedVersion) =
      let
        versionTitle = if T.null suggestedVersion then T.empty else "-" <> suggestedVersion
        targetTitle = case target of
          Nothing -> T.empty
          Just t  -> " at " <> T.pack t
        title = "Add dependency " <> suggestedDep <> versionTitle <> targetTitle
        version = if T.null suggestedVersion then Nothing else Just suggestedVersion

        params = CabalAddCommandParams {cabalPath = cabalFilePath
                                      , verTxtDocId = verTxtDocId
                                      , buildTarget = target
                                      , dependency = suggestedDep
                                      , version=version}
        command = mkLspCommand plId (CommandId cabalAddCommand) "Add missing dependency" (Just [toJSON params])
      in CodeAction title (Just CodeActionKind_QuickFix) (Just []) Nothing Nothing Nothing (Just command) Nothing

-- | Gives a mentioned number of @(dependency, version)@ pairs
-- found in the "hidden package" diagnostic message.
--
-- For example, if a ghc error looks like this:
--
-- > "Could not load module ‘Data.List.Split’
-- > It is a member of the hidden package ‘split-0.2.5’.
-- > Perhaps you need to add ‘split’ to the build-depends in your .cabal file."
--
-- or this if PackageImports extension is used:
--
-- > "Could not find module ‘Data.List.Split’
-- > Perhaps you meant
-- >   Data.List.Split (needs flag -package-id split-0.2.5)"
--
-- It extracts mentioned package names and version numbers.
-- In this example, it will be @[("split", "0.2.5")]@
--
-- Also supports messages without a version.
--
-- > "Perhaps you need to add ‘split’ to the build-depends in your .cabal file."
--
-- Will turn into @[("split", "")]@
hiddenPackageSuggestion :: Diagnostic -> [(T.Text, T.Text)]
hiddenPackageSuggestion diag = getMatch (msg =~ regex)
  where
    msg :: T.Text
    msg = _message diag
    regex :: T.Text -- TODO: Support multiple packages suggestion
    regex =
        let regex' = "([a-zA-Z0-9-]*[a-zA-Z0-9])(-([0-9\\.]*))?"
        in "It is a member of the hidden package [\8216']" <> regex' <> "[\8217']"
           <> "|"
           <> "needs flag -package-id " <> regex'
    -- Have to do this matching because `Regex.TDFA` doesn't(?) support
    -- not-capturing groups like (?:message)
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [(T.Text, T.Text)]
    getMatch (_, _, _, []) = []
    getMatch (_, _, _, [dependency, _, cleanVersion, "", "", ""]) = [(dependency, cleanVersion)]
    getMatch (_, _, _, ["", "", "", dependency, _, cleanVersion]) = [(dependency, cleanVersion)]
    getMatch (_, _, _, _) = []

command :: Recorder (WithPriority Log) -> CommandFunction IdeState CabalAddCommandParams
command recorder state _ params@(CabalAddCommandParams {cabalPath = path, verTxtDocId = verTxtDocId, buildTarget = target, dependency = dep, version = mbVer}) = do
  logWith recorder Debug $ LogCalledCabalAddCommand params
  let specifiedDep = case mbVer of
        Nothing  -> dep
        Just ver -> dep <> " ^>=" <> ver
  caps <- lift pluginGetClientCapabilities
  let env = (state, caps, verTxtDocId)
  edit <- getDependencyEdit recorder env path target (fromList [T.unpack specifiedDep])
  void $ lift $ pluginSendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  logWith recorder Debug LogExecutedCommand
  pure $ InR Null

-- | Constructs prerequisites for the @executeConfig@
--   and runs it, given path to the cabal file and a dependency message.
--   Given the new contents of the cabal file constructs and returns the @edit@.
--   Inspired by @main@ in cabal-add,
--   Distribution.Client.Main
getDependencyEdit :: MonadIO m => Recorder (WithPriority Log) -> (IdeState, ClientCapabilities, VersionedTextDocumentIdentifier) ->
    FilePath -> Maybe String -> NonEmpty String -> ExceptT PluginError m WorkspaceEdit
getDependencyEdit recorder env cabalFilePath buildTarget dependency = do
  let (state, caps, verTxtDocId) = env
  (mbCnfOrigContents, mbFields, mbPackDescr) <- liftIO $ runAction "cabal.cabal-add" state $ do
        contents <- getFileContents $ toNormalizedFilePath cabalFilePath
        inFields <- useWithStale ParseCabalFields $ toNormalizedFilePath cabalFilePath
        inPackDescr <- useWithStale ParseCabalFile $ toNormalizedFilePath cabalFilePath
        let mbCnfOrigContents = case contents of
                    (Just txt) -> Just $ encodeUtf8 $ Rope.toText txt
                    _          -> Nothing
        let mbFields = fst <$> inFields
        let mbPackDescr = fst <$> inPackDescr
        pure (mbCnfOrigContents, mbFields, mbPackDescr)

  -- Check if required info was received,
  -- otherwise fall back on other options.
  (cnfOrigContents, fields, packDescr) <- do
    cnfOrigContents <- case mbCnfOrigContents of
          (Just cnfOrigContents) -> pure cnfOrigContents
          Nothing                -> readCabalFile cabalFilePath
    (fields, packDescr) <- case (mbFields, mbPackDescr) of
          (Just fields, Just packDescr) -> pure (fields, packDescr)
          (_, _) -> case parseCabalFile cabalFilePath cnfOrigContents of
                        Left err   -> throwE $ PluginInternalError $ T.pack err
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
      logWith recorder Debug $ LogCreatedEdit edit
      pure edit

-- | Given a path to a haskell file, returns the closest cabal file.
--   If a package.yaml is present in same directory as the .cabal file, returns nothing, because adding a dependency to a generated cabal file
--   will break propagation of changes from package.yaml to cabal files in stack projects.
--   If cabal file wasn't found, gives Nothing.
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
        Just cabalFile -> guardAgainstHpack path cabalFile
          where
            guardAgainstHpack :: FilePath -> FilePath -> IO (Maybe FilePath)
            guardAgainstHpack path cabalFile = do
              exists <- doesFileExist $ path </> "package.yaml"
              if exists then pure Nothing else pure $ Just cabalFile

-- | Gives cabal file's contents or throws error.
--   Inspired by @readCabalFile@ in cabal-add,
--   Distribution.Client.Main
--
--   This is a fallback option!
--   Use only if the `GetFileContents` fails.
readCabalFile :: MonadIO m => FilePath -> ExceptT PluginError m ByteString
readCabalFile fileName = do
  cabalFileExists <- liftIO $ doesFileExist fileName
  if cabalFileExists
    then snd . patchQuirks <$> liftIO (B.readFile fileName)
    else throwE $ PluginInternalError $ T.pack ("Failed to read cabal file at " <> fileName)
