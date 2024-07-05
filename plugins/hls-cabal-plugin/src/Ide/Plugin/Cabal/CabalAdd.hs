{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE OverloadedStrings   #-}
module Ide.Plugin.Cabal.CabalAdd
( missingDependenciesAction
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
                                              CodeActionKind (CodeActionKind_QuickFix),
                                              Command (..), Diagnostic (..),
                                              Null (Null), Uri, type (|?) (InR))
import           System.Process              (readProcess)
import           Text.Regex.TDFA

missingDependenciesAction :: PluginId -> Int -> Uri -> Diagnostic -> [CodeAction]
missingDependenciesAction plId maxCompletions uri diag =
  mkCodeAction <$> missingDependenciesSuggestion maxCompletions (_message diag)
  where
    mkCodeAction suggestedDep =
      let
        title = "Add dependency " <> suggestedDep
        command = mkLspCommand plId (CommandId cabalAddNameCommand) "Execute Code Action" (Nothing)
      in CodeAction title (Just CodeActionKind_QuickFix) (Just []) Nothing Nothing Nothing (Just command) Nothing

missingDependenciesSuggestion :: Int -> T.Text -> [T.Text]
missingDependenciesSuggestion maxCompletions msg = take maxCompletions $ getMatch (msg =~ regex)
  where
    regex :: T.Text
    regex = "Could not load module \8216.*\8217.\nIt is a member of the hidden package \8216(.*)\8217"
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [T.Text]
    getMatch (_, _, _, results) = results

hiddenPackageAction
  :: Int -- ^ Maximum number of suggestions to return
  -> Uri -- ^ File for which the diagnostic was generated
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

command :: CommandFunction IdeState Uri
command state _ uri = do
  void $ liftIO $ readProcess "cabal-add" [] []
  pure $ InR Null
