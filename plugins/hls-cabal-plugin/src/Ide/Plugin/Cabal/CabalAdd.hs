{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE OverloadedStrings   #-}
module Ide.Plugin.Cabal.CabalAdd
(  findResponsibleCabalFile
 , missingDependenciesAction
 , missingDependenciesSuggestion
 , hiddenPackageAction
 , cabalAddNameCommand
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
import           System.Directory            (listDirectory)
import           System.FilePath             (dropFileName, splitPath,
                                              takeExtension, (</>), takeFileName)
import           System.Process              (readProcess)
import           Text.Regex.TDFA
import Data.Aeson.Types                      (toJSON)
import Debug.Trace

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
        title = "Add dependency " <> suggestedDep <> " at " <> cabalName <> " " <> (T.pack $ show args)
        -- args = Just [toJSON suggestedDep, toJSON ("--project-file " <> cabalFile)]
        args = Just [toJSON suggestedDep]

        command = mkLspCommand plId (CommandId cabalAddNameCommand) "Execute Code Action" args -- TODO: add cabal-add CL arguments
      in CodeAction title (Just CodeActionKind_QuickFix) (Just []) Nothing Nothing Nothing (Just command) Nothing

-- | Gives a mentioned number of hidden packages given
--   a specific error message
missingDependenciesSuggestion :: Int -> T.Text -> [T.Text]
missingDependenciesSuggestion maxCompletions msg = take maxCompletions $ getMatch (msg =~ regex)
  where
    regex :: T.Text -- TODO: Support multiple packages suggestion
    regex = "Could not load module \8216.*\8217.\nIt is a member of the hidden package \8216(.*)\8217"
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

cabalAddNameCommand :: IsString p => p
cabalAddNameCommand = "cabalAdd"

-- | Registering a cabal-add as a HLS command
command :: CommandFunction IdeState Uri
command state _ uri = do
  traceShowM ("uri ", uri)
  void $ liftIO $ readProcess "cabal-add" [] []
  pure $ InR Null -- TODO: return cabal-add output (?)
