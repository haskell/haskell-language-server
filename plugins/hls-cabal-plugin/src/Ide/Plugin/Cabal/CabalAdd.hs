{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}

module Ide.Plugin.Cabal.CabalAdd
(  findResponsibleCabalFile
 , hiddenPackageAction
 , hiddenPackageSuggestion
 , hiddenPackageAction
 , cabalAddCommand
 , command
)
where

import           Control.Monad                                 (filterM, void)
import           Control.Monad.IO.Class                        (liftIO)
import           Data.String                                   (IsString)
import qualified Data.Text                                     as T
import           Development.IDE                               (IdeState)
import           Distribution.PackageDescription.Quirks        (patchQuirks)
import           Ide.PluginUtils                               (mkLspCommand)
import           Ide.Types                                     (CommandFunction,
                                                                CommandId (CommandId),
                                                                PluginId)
import           Language.LSP.Protocol.Types                   (CodeAction (CodeAction),
                                                                CodeActionDisabled (CodeActionDisabled),
                                                                CodeActionKind (CodeActionKind_QuickFix),
                                                                Diagnostic (..),
                                                                Null (Null),
                                                                Uri (..),
                                                                type (|?) (InR),
                                                                uriToFilePath)
import           System.Directory                              (doesFileExist,
                                                                listDirectory)

import           Data.Aeson.Types                              (FromJSON,
                                                                ToJSON, toJSON)
import           Data.ByteString                               (ByteString)
import qualified Data.ByteString.Char8                         as B
import           Data.List.NonEmpty                            (NonEmpty (..),
                                                                fromList)
import           Data.Maybe                                    (fromJust)
import           Distribution.Client.Add                       as Add
import           Distribution.Compat.Prelude                   (Generic)
import           Distribution.PackageDescription               (packageDescription,
                                                                specVersion)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Pretty                           (pretty)
import           Distribution.Simple.BuildTarget               (BuildTarget,
                                                                buildTargetComponentName,
                                                                readBuildTargets)
import           Distribution.Verbosity                        (silent,
                                                                verboseNoStderr)
import           System.FilePath                               (dropFileName,
                                                                makeRelative,
                                                                splitPath,
                                                                takeExtension,
                                                                (</>))
import           System.IO.Unsafe                              (unsafeInterleaveIO)
import           Text.PrettyPrint                              (render)
import           Text.Regex.TDFA

-- | Given a path to a haskell file, finds all cabal files paths
--   sorted from the closest to the farthest.
--   Gives all found paths all the way to the root directory.
findResponsibleCabalFile :: FilePath -> IO [FilePath]
findResponsibleCabalFile uriPath = do
  contents <- mapM (unsafeInterleaveIO . listDirectory) allDirPaths
  let objectWithPaths = concat $ zipWith (\path content -> map (path </>) content) allDirPaths contents
  let objectCabalExtension = filter (\c -> takeExtension c == ".cabal") objectWithPaths
  cabalFiles <- filterM (\c -> doesFileExist c) objectCabalExtension
  pure $ reverse cabalFiles -- sorted from closest to the uriPath
  where dirPath = dropFileName uriPath
        allDirPaths = scanl1 (</>) (splitPath dirPath)


-- | Gives a code action that calls the command,
--   if a suggestion for a missing dependency is found.
--   Disabled action if no cabal files given.
--   Conducts IO action on a cabal file to find build targets.
hiddenPackageAction :: PluginId -> Int -> Uri -> Diagnostic -> [FilePath] -> IO [CodeAction]
hiddenPackageAction plId maxCompletions uri diag cabalFiles =
  case cabalFiles of
    [] -> pure [CodeAction "No .cabal file found" (Just CodeActionKind_QuickFix) (Just []) Nothing
                                             (Just (CodeActionDisabled "No .cabal file found")) Nothing Nothing Nothing]
    (cabalFile:_) -> do
        buildTargets <- liftIO $ getBuildTargets cabalFile (fromJust $ uriToFilePath uri)
        case buildTargets of
          [] -> pure $ mkCodeAction cabalFile Nothing <$> hiddenPackageSuggestion maxCompletions (_message diag)
          targets -> pure $ concat [mkCodeAction cabalFile (Just $ buildTargetToStringRepr target) <$>
                              hiddenPackageSuggestion maxCompletions (_message diag) | target <- targets]
  where
    buildTargetToStringRepr target = render $ pretty $ buildTargetComponentName target
    mkCodeAction cabalFile target (suggestedDep, suggestedVersion) =
      let
        versionTitle = if T.null suggestedVersion then T.empty else "  version " <> suggestedVersion
        targetTitle = case target of
          Nothing -> T.empty
          Just t  -> "  target " <> T.pack t
        title = "Add dependency " <> suggestedDep <> versionTitle <> targetTitle

        version = if T.null suggestedVersion then Nothing else Just suggestedVersion

        params = CabalAddCommandParams {cabalPath = cabalFile, buildTarget = target, dependency = suggestedDep, version=version}
        command = mkLspCommand plId (CommandId cabalAddCommand) "Execute Code Action" (Just [toJSON params])
      in CodeAction title (Just CodeActionKind_QuickFix) (Just []) Nothing Nothing Nothing (Just command) Nothing

-- | Gives a mentioned number of hidden packages given
--   a specific error message
hiddenPackageSuggestion :: Int -> T.Text -> [(T.Text, T.Text)]
hiddenPackageSuggestion maxCompletions msg = take maxCompletions $ getMatch (msg =~ regex)
  where
    regex :: T.Text -- TODO: Support multiple packages suggestion
    regex = "Could not load module \8216.*\8217.\nIt is a member of the hidden package [\8216\\']([a-z]+)[-]?([0-9\\.]*)[\8217\\']"
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [(T.Text, T.Text)]
    getMatch (_, _, _, []) = []
    getMatch (_, _, _, [dependency]) = [(dependency, T.empty)]
    getMatch (_, _, _, [dependency, version]) = [(dependency, version)]
    getMatch (_, _, _, _) = error "Impossible pattern matching case"


cabalAddCommand :: IsString p => p
cabalAddCommand = "cabalAdd"

data CabalAddCommandParams =
     CabalAddCommandParams { cabalPath   :: FilePath
                           , buildTarget :: Maybe String
                           , dependency  :: T.Text
                           , version     :: Maybe T.Text
                           }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

command :: CommandFunction IdeState CabalAddCommandParams
command _ _ (CabalAddCommandParams {cabalPath = path, buildTarget = target, dependency = dep, version = mbVer}) = do
  let specifiedDep = case mbVer of
        Nothing  -> dep
        Just ver -> dep <> " ^>=" <> ver
  void $ liftIO $ addDependency path target (fromList [T.unpack specifiedDep])
  pure $ InR Null

-- | Gives cabal file's contents or throws error.
--   Inspired by @readCabalFile@ in cabal-add,
--   Distribution.Client.Main
readCabalFile :: FilePath -> IO ByteString
readCabalFile fileName = do
  cabalFileExists <- doesFileExist fileName
  if cabalFileExists
    then snd . patchQuirks <$> B.readFile fileName
    else error ("Failed to read cabal file at " <> fileName)

getBuildTargets :: FilePath -> FilePath -> IO [BuildTarget]
getBuildTargets cabalFilePath haskellFilePath = do
  cabalContents <- readCabalFile cabalFilePath
  (_, packDescr) <- case parseCabalFile cabalFilePath cabalContents of
    Left err   -> error err
    Right pair -> pure pair

  let haskellFileRelativePath = makeRelative (dropFileName cabalFilePath) haskellFilePath
  readBuildTargets (verboseNoStderr silent) (flattenPackageDescription packDescr) [haskellFileRelativePath]


-- | Constructs prerequisets for the @executeConfig@
--   and runs it, given path to the cabal file and a dependency message.
--
--   Inspired by @main@ in cabal-add,
--   Distribution.Client.Main
addDependency :: FilePath -> Maybe String -> NonEmpty String -> IO ()
addDependency cabalFilePath buildTarget dependency = do

  cnfOrigContents <- readCabalFile cabalFilePath

  (fields, packDescr) <- case parseCabalFile cabalFilePath cnfOrigContents of
    Left err   -> error err
    Right pair -> pure pair

  let inputs = do
        let rcnfComponent = buildTarget
        let specVer = specVersion $ packageDescription packDescr
        cmp <- resolveComponent cabalFilePath (fields, packDescr) rcnfComponent
        deps <- traverse (validateDependency specVer) dependency
        pure (fields, packDescr, cmp, deps)

  (cnfFields, origPackDescr, cnfComponent, cnfDependencies) <- case inputs of
    Left err   -> error err
    Right pair -> pure pair

  case executeConfig (validateChanges origPackDescr) (Config {..}) of
    Nothing -> error $ "Cannot extend build-depends in " ++ cabalFilePath
    Just r  -> B.writeFile cabalFilePath r
