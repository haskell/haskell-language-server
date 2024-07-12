{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Ide.Plugin.Cabal.CabalAdd
(  findResponsibleCabalFile
 , missingDependenciesAction
 , missingDependenciesSuggestion
 , hiddenPackageAction
 , cabalAddCommand
 , command
)
where

import           Control.Monad               (void)
import           Control.Monad.IO.Class      (liftIO)
import           Data.String                 (IsString)
import qualified Data.Text                   as T
import           Development.IDE             (IdeState)
import           Ide.PluginUtils             (mkLspCommand)
import           Ide.Types                   (CommandFunction,
                                              CommandId (CommandId), PluginId)
import           Language.LSP.Protocol.Types (CodeAction (CodeAction),
                                              CodeActionDisabled (CodeActionDisabled),
                                              CodeActionKind (CodeActionKind_QuickFix),
                                              Diagnostic (..), Null (Null),
                                              Uri (..), type (|?) (InR))
import           System.Directory            (listDirectory, doesFileExist)
import Distribution.PackageDescription.Quirks (patchQuirks)

import           System.FilePath             (dropFileName, splitPath,
                                              takeExtension, (</>), takeFileName)
import           System.Process              (readProcess)
import           Text.Regex.TDFA
import Data.Aeson.Types                      (toJSON, FromJSON, ToJSON)
import Debug.Trace
import Distribution.Compat.Prelude (Generic)
import Data.List.NonEmpty (NonEmpty (..), fromList)
import Distribution.Client.Add as Add
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Maybe (fromJust)
import Distribution.PackageDescription (
  ComponentName,
  GenericPackageDescription,
  packageDescription,
  specVersion,
 )

findResponsibleCabalFile :: FilePath -> IO [FilePath]
findResponsibleCabalFile uriPath = do
  contents <- mapM listDirectory allDirPaths
  let filesWithPaths = concat $ zipWith (\path content -> map (path </>) content) allDirPaths contents
  let cabalFiles = filter (\c -> takeExtension c == ".cabal") filesWithPaths
  pure $ reverse cabalFiles -- sorted from closest to the uriPath
  where dirPath = dropFileName uriPath
        allDirPaths = scanl1 (</>) (splitPath dirPath)


-- | Gives a code action that calls the command,
--   if a suggestion for a missing dependency is found.
--   Disabled action if no cabal files given.
missingDependenciesAction :: PluginId -> Int -> Uri -> Diagnostic -> [FilePath] -> [CodeAction]
missingDependenciesAction plId maxCompletions uri diag cabalFiles =
  case cabalFiles of
    [] -> [CodeAction "No .cabal file found" (Just CodeActionKind_QuickFix) (Just []) Nothing
                                             (Just (CodeActionDisabled "No .cabal file found")) Nothing Nothing Nothing]
    (cabalFile:_) -> mkCodeAction cabalFile <$> missingDependenciesSuggestion maxCompletions (_message diag)
  where
    mkCodeAction cabalFile suggestedDep =
      let
        cabalName = T.pack $ takeFileName cabalFile
        params = CabalAddCommandParams {cabalPath = cabalFile, dependency = suggestedDep}
        title = "Add dependency " <> suggestedDep <> " at " <> cabalName <> " " <> (T.pack $ show params)
        command = mkLspCommand plId (CommandId cabalAddCommand) "Execute Code Action" (Just [toJSON params]) -- TODO: add cabal-add CL arguments
      in CodeAction title (Just CodeActionKind_QuickFix) (Just []) Nothing Nothing Nothing (Just command) Nothing

-- | Gives a mentioned number of hidden packages given
--   a specific error message
missingDependenciesSuggestion :: Int -> T.Text -> [T.Text]
missingDependenciesSuggestion maxCompletions msg = take maxCompletions $ getMatch (msg =~ regex)
  where
    regex :: T.Text -- TODO: Support multiple packages suggestion
    regex = "Could not load module \8216.*\8217.\nIt is a member of the hidden package \8216([a-z]+)[-]?[0-9\\.]*\8217"
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [T.Text]
    getMatch (_, _, _, results) = results

hiddenPackageAction
  :: Int
  -> Uri
  -> Diagnostic
  -> [CodeAction]
hiddenPackageAction = undefined

hiddenPackageSuggestion :: Int -> T.Text -> [T.Text]
hiddenPackageSuggestion maxCompletions msg = take maxCompletions $ getMatch (msg =~ regex)
  where
    regex :: T.Text
    regex = "It is a member of the package '.*'\nwhich is unusable due to missing dependencies:[\n ]*([:word:-.]*)"
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [T.Text]
    getMatch (_, _, _, results) = results

cabalAddCommand :: IsString p => p
cabalAddCommand = "cabalAdd"

data CabalAddCommandParams =
     CabalAddCommandParams { cabalPath :: FilePath
                           , dependency :: T.Text
                           }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

command :: CommandFunction IdeState CabalAddCommandParams
command _ _ (CabalAddCommandParams {cabalPath = path, dependency = dep}) = do
  traceShowM ("cabalPath ", path)
  traceShowM ("dependency ", dep)
  void $ liftIO $ addDependency path (fromList [T.unpack dep])
  pure $ InR Null

data RawConfig = RawConfig
  { rcnfCabalFile :: !FilePath
  , rcnfComponent :: !(Maybe String)
  , rcnfDependencies :: !(NonEmpty String)
  }
  deriving (Show)

readCabalFile :: FilePath -> IO (Maybe ByteString)
readCabalFile fileName = do
  cabalFileExists <- doesFileExist fileName
  if cabalFileExists
    then Just . snd . patchQuirks <$> B.readFile fileName
    else pure Nothing

addDependency :: FilePath -> NonEmpty String -> IO ()
addDependency cabalFilePath dependency = do
  let rcnfComponent = Nothing -- Just "component?"

  cnfOrigContents <- fromJust <$> readCabalFile cabalFilePath
  let inputs :: Either _ _ = do
        (fields, packDescr) <- parseCabalFile cabalFilePath cnfOrigContents
        --                                    ^ cabal path  ^ cabal raw contents
        let specVer = specVersion $ packageDescription packDescr
        cmp <- resolveComponent cabalFilePath (fields, packDescr) rcnfComponent
        deps <- traverse (validateDependency specVer) dependency
        pure (fields, packDescr, cmp, deps)

  (cnfFields, origPackDescr, cnfComponent, cnfDependencies) <- case inputs of
    Left err -> error err
    Right pair -> pure pair

  case executeConfig (validateChanges origPackDescr) (Config {..}) of
    Nothing -> error $ "Cannot extend build-depends in " ++ cabalFilePath
    Just r -> B.writeFile cabalFilePath r