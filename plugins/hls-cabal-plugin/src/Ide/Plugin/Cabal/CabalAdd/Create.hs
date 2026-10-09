{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Cabal.CabalAdd.Create (
  createHandler,
  Log,
)
where

import           Control.Monad.Trans
import           Control.Monad.Trans.Except                    (ExceptT)
import           Control.Monad.Trans.Maybe
import           Data.ByteString                               (ByteString)
import qualified Data.Text                                     as T
import qualified Data.Text.Encoding                            as T
import qualified Data.Text.IO                                  as Text
import qualified Data.Text.Utf16.Rope.Mixed                    as Rope
import           Development.IDE.Core.FileStore                (getUriContents,
                                                                getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils              (runActionE,
                                                                useE)
import qualified Development.IDE.Core.Shake                    as Shake
import           Development.IDE.Types.Location                (toNormalizedUri)
import qualified Distribution.Client.Add                       as Add
import           Distribution.Client.Add                       (AddConfig (..),
                                                                executeAddConfig)
import           Ide.Plugin.Cabal.CabalAdd.Rename              (resolveBuildInfoE,
                                                                toRelativeModulePathE)
import           Distribution.Fields                           (Field)
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.Parsec.Position                  (Position)
import           Ide.Logger
import           Ide.Plugin.Cabal.CabalAdd.CodeAction          (buildInfoToHsSourceDirs, mkModuleInsertionConfig, mkStanzaItems)
import           Ide.Plugin.Cabal.Completion.Types             (ParseCabalFields (..),
                                                                ParseCabalFile (..))
import           Ide.Plugin.Error
import           Ide.PluginUtils                               (WithDeletions (IncludeDeletions),
                                                                diffText)
import           Language.LSP.Protocol.Types                   (ClientCapabilities,
                                                                TextDocumentIdentifier (TextDocumentIdentifier),
                                                                VersionedTextDocumentIdentifier,
                                                                WorkspaceEdit,
                                                                filePathToUri,
                                                                toNormalizedFilePath)
import Ide.Plugin.Cabal.CabalAdd.Types hiding (Log)
import qualified Data.List.NonEmpty as NE
import Data.Maybe

data Log
  = LogDidCreate FilePath
  | CabalCreateLog T.Text
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogDidCreate newFp ->
      "Received create info for" <+> pretty newFp
    CabalCreateLog newModulePath ->
      "Executing create for module" <+> pretty newModulePath <+> "in cabal file."

--------------------------------------------
-- Create module in cabal file
--------------------------------------------

createHandler ::
  forall m.
  (MonadIO m) =>
  Recorder (WithPriority Log) ->
  Shake.IdeState ->
  ClientCapabilities ->
  -- | the new file path
  FilePath ->
  -- | the path to the cabal file, responsible for the create module
  FilePath ->
  Shake.IdeState ->
  ExceptT PluginError m WorkspaceEdit
createHandler recorder _ caps newHaskellFilePath cabalFilePath ideState = do
  logWith recorder Info $ LogDidCreate newHaskellFilePath
  (contents, fields, gpd, verTxtDocId) <- runActionE "cabal-plugin.getUriContents" ideState $ do
    let nuri = toNormalizedUri $ filePathToUri cabalFilePath
        nfp = toNormalizedFilePath cabalFilePath
    mContent <- lift $ getUriContents nuri
    verTxtDocId <-
      runActionE "cabalAdd.getVersionedTextDoc" ideState $
        lift $
          getVersionedTextDoc $
            TextDocumentIdentifier (filePathToUri cabalFilePath)
    content <- case mContent of
      Just content -> pure $ Rope.toText content
      Nothing      -> liftIO $ Text.readFile cabalFilePath
    fields <- useE ParseCabalFields nfp
    gpd <- useE ParseCabalFile nfp
    pure (content, fields, gpd, verTxtDocId)
  cabalFileEdit <-
    applyModuleAddToCabalFile
      recorder
      (caps, verTxtDocId)
      newHaskellFilePath
      cabalFilePath
      (T.encodeUtf8 contents)
      fields
      gpd
  pure cabalFileEdit

{- | Apply create to the given cabal file

Adds the module name corresponding to the new file path in the given cabal file.
Fails if the cabal file cannot be parsed or the file path cannot be parsed to module
names.

TODO: check for occurrence of the module in the cabal file.
-}
applyModuleAddToCabalFile ::
  forall m.
  (MonadIO m) =>
  Recorder (WithPriority Log) ->
  (ClientCapabilities, VersionedTextDocumentIdentifier) ->
  -- | the new file path after the create
  FilePath ->
  -- | the path to the cabal file, responsible for the create module
  FilePath ->
  -- | the responsible cabal file's contents
  ByteString ->
  -- | the responsible cabal file's fields
  [Field Position] ->
  GenericPackageDescription ->
  ExceptT PluginError m WorkspaceEdit
applyModuleAddToCabalFile recorder (caps, verTxtDocId) newHaskellFilePath cabalFilePath cnfOrigContents fields gpd = do
  let pd = flattenPackageDescription gpd
  compName <- guessComponentName verTxtDocId gpd cabalFilePath newHaskellFilePath
  buildInfo <- resolveBuildInfoE pd compName
  newModulePath <- toRelativeModulePathE (buildInfoToHsSourceDirs buildInfo) cabalFilePath newHaskellFilePath
  newContents <-
    maybeToExceptT PluginStaleResolve $
      hoistMaybe $
        executeAddConfig (Add.validateChanges gpd) (addConfig (Right $ compName) (guessTargetField compName) newModulePath)
  logWith recorder Info $ CabalCreateLog newModulePath
  pure $ diffText caps (verTxtDocId, T.decodeUtf8 cnfOrigContents) (T.decodeUtf8 newContents) IncludeDeletions
 where
  -- define addConfig to pass to cabal-add
  addConfig compName targetField additions =
    AddConfig
      { Add.cnfOrigContents = cnfOrigContents
      , Add.cnfFields = fields
      , Add.cnfComponent = compName
      , Add.cnfTargetField = targetField
      , Add.cnfAdditions = NE.singleton $ T.encodeUtf8 additions
      }

{- | Tries to guess the best fitting target field based on the provided component name.

Currently it picks `exposed-modules` for main library components and `other-modules` otherwise.
-}
guessTargetField :: ComponentName -> Add.TargetField
guessTargetField = \case
  CLibName LMainLibName    -> Add.ExposedModules
  CLibName (LSubLibName _) -> Add.OtherModules
  CExeName _               -> Add.OtherModules
  CTestName _              -> Add.OtherModules
  CBenchName _             -> Add.OtherModules
  CFLibName _              -> Add.OtherModules

{- | Tries to guess the best fitting cabal component name based on the file location. -}
guessComponentName ::
  Applicative m => VersionedTextDocumentIdentifier ->
  GenericPackageDescription ->
  -- | the path to the cabal file, responsible for the create module
  FilePath ->
  -- | the new file path after the create
  FilePath ->
  ExceptT PluginError m ComponentName
guessComponentName verTxtDocId gpd cabalFilePath newHaskellFilePath = do
  maybeToExceptT
    (PluginInvalidUserState "unable to guess a component type")
    $ hoistMaybe
    $ listToMaybe
        [ insertionStanza
        | stanzaItem <- mkStanzaItems gpd
        , ModuleInsertionConfig{insertionStanza} <-
            mkModuleInsertionConfig verTxtDocId cabalFilePath newHaskellFilePath stanzaItem
        ]
