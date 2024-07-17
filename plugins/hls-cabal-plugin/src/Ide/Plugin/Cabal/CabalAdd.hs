{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}

module Ide.Plugin.Cabal.CabalAdd
(  findResponsibleCabalFile
 , missingDependenciesAction
 , missingDependenciesSuggestion
 , hiddenPackageAction
 , cabalAddCommand
 , command
)
where

import           Control.Monad                          (void, filterM)
import           Control.Monad.IO.Class                 (liftIO)
import           Data.String                            (IsString)
import qualified Data.Text                              as T
import           Development.IDE                        (IdeState)
import           Distribution.PackageDescription.Quirks (patchQuirks)
import           Ide.PluginUtils                        (mkLspCommand)
import           Ide.Types                              (CommandFunction,
                                                         CommandId (CommandId),
                                                         PluginId)
import           Language.LSP.Protocol.Types            (CodeAction (CodeAction),
                                                         CodeActionDisabled (CodeActionDisabled),
                                                         CodeActionKind (CodeActionKind_QuickFix),
                                                         Diagnostic (..),
                                                         Null (Null), Uri (..),
                                                         type (|?) (InR))
import           System.Directory                       (doesFileExist,
                                                         listDirectory)

import           Data.Aeson.Types                       (FromJSON, ToJSON,
                                                         toJSON)
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Char8                  as B
import           Data.List.NonEmpty                     (NonEmpty (..),
                                                         fromList)
import           Distribution.Client.Add                as Add
import           Distribution.Compat.Prelude            (Generic)
import           Distribution.PackageDescription        (packageDescription,
                                                         specVersion, GenericPackageDescription (GenericPackageDescription), showComponentName, componentNameRaw)
import           System.FilePath                        (dropFileName,
                                                         splitPath,
                                                         takeExtension, (</>), takeFileName, dropExtension)
import           Text.Regex.TDFA
import           System.IO.Unsafe                       (unsafeInterleaveIO)
import System.Directory (doesFileExist)
import Distribution.Simple.BuildTarget                  (readBuildTargets, buildTargetComponentName)
import Distribution.Verbosity (normal)
import Debug.Trace
import Distribution.PackageDescription.Configuration (flattenPackageDescription)
import Distribution.Types.ComponentName (componentNameStanza)

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
missingDependenciesAction :: PluginId -> Int -> Uri -> Diagnostic -> [FilePath] -> [CodeAction]
missingDependenciesAction plId maxCompletions _ diag cabalFiles =
  case cabalFiles of
    [] -> [CodeAction "No .cabal file found" (Just CodeActionKind_QuickFix) (Just []) Nothing
                                             (Just (CodeActionDisabled "No .cabal file found")) Nothing Nothing Nothing]
    (cabalFile:_) -> mkCodeAction cabalFile <$> missingDependenciesSuggestion maxCompletions (_message diag)
  where
    mkCodeAction cabalFile (suggestedDep, suggestedVersion) =
      let
        versionTitle = if T.null suggestedVersion then T.empty else "version " <> suggestedVersion
        title = "Add dependency " <> suggestedDep <> " " <> versionTitle

        version = if T.null suggestedVersion then Nothing else Just suggestedVersion
        params = CabalAddCommandParams {cabalPath = cabalFile, dependency = suggestedDep, version=version}
        command = mkLspCommand plId (CommandId cabalAddCommand) "Execute Code Action" (Just [toJSON params])
      in CodeAction title (Just CodeActionKind_QuickFix) (Just []) Nothing Nothing Nothing (Just command) Nothing

-- | Gives a mentioned number of hidden packages given
--   a specific error message
missingDependenciesSuggestion :: Int -> T.Text -> [(T.Text, T.Text)]
missingDependenciesSuggestion maxCompletions msg = take maxCompletions $ getMatch (msg =~ regex)
  where
    regex :: T.Text -- TODO: Support multiple packages suggestion
    regex = "Could not load module \8216.*\8217.\nIt is a member of the hidden package [\8216\\']([a-z]+)[-]?([0-9\\.]*)[\8217\\']"
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [(T.Text, T.Text)]
    getMatch (_, _, _, []) = []
    getMatch (_, _, _, [dependency]) = [(dependency, T.empty)]
    getMatch (_, _, _, [dependency, version]) = [(dependency, version)]
    getMatch (_, _, _, _) = error "Impossible pattern matching case"


hiddenPackageAction :: Int -> Uri -> Diagnostic -> [CodeAction]
hiddenPackageAction = undefined

hiddenPackageSuggestion :: Int -> T.Text -> [T.Text]
hiddenPackageSuggestion maxCompletions msg = undefined

cabalAddCommand :: IsString p => p
cabalAddCommand = "cabalAdd"

data CabalAddCommandParams =
     CabalAddCommandParams { cabalPath  :: FilePath
                           , dependency :: T.Text
                           , version    :: Maybe T.Text
                           }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

command :: CommandFunction IdeState CabalAddCommandParams
command _ _ (CabalAddCommandParams {cabalPath = path, dependency = dep, version = mbVer}) = do
  let specifiedDep = case mbVer of
        Nothing  -> dep
        Just ver -> dep <> " ^>=" <> ver
  void $ liftIO $ addDependency path (fromList [T.unpack specifiedDep])
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

-- | Constructs prerequisets for the @executeConfig@
--   and runs it, given path to the cabal file and
--   a dependency message.
--
--   Inspired by @main@ in cabal-add,
--   Distribution.Client.Main
addDependency :: FilePath -> NonEmpty String -> IO ()
addDependency cabalFilePath dependency = do

  cnfOrigContents <- readCabalFile cabalFilePath

  (fields, packDescr) <- case parseCabalFile cabalFilePath cnfOrigContents of
    Left err   -> error err
    Right pair -> pure pair

  let cabalName = dropExtension $ takeFileName cabalFilePath
  buildTargets <- readBuildTargets normal (flattenPackageDescription packDescr) [cabalName]

  let inputs = do
        let rcnfComponent = case buildTargets of
                []         -> Nothing
                (target:_) -> Just $ componentNameRaw $ buildTargetComponentName target

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
