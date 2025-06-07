{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}

module Ide.Plugin.Cabal.CabalAdd.Command (
  cabalAddPackageCommandId,
  cabalAddModuleCommandId,
  addPackageCommand,
  addModuleCommand,
  Log,
)
where

import           Control.Monad                     (void)
import           Control.Monad.Except              (modifyError)
import           Control.Monad.IO.Class            (MonadIO, liftIO)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Except
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Char8             as B
import           Data.List.NonEmpty                (singleton)
import qualified Data.Text                         as T
import           Data.Text.Encoding                (encodeUtf8)
import qualified Data.Text.Encoding                as T
import           Data.Text.Utf16.Rope.Mixed        as Rope
import           Development.IDE.Core.FileStore    (getFileContents)
import           Development.IDE.Core.Rules        (IdeState)
import           Development.IDE.Core.Service      (runAction)
import           Development.IDE.Core.Shake        (useWithStale)
import           Distribution.Client.Add           as Add
import           Distribution.Fields               (Field)
import           Distribution.PackageDescription
import           Distribution.Parsec.Position      (Position)
import qualified Distribution.Pretty               as CabalPretty
import           Ide.Logger
import           Ide.Plugin.Cabal.CabalAdd.Types
import           Ide.Plugin.Cabal.Completion.Types (ParseCabalFields (..),
                                                    ParseCabalFile (..))
import           Ide.Plugin.Cabal.Files
import           Ide.Plugin.Cabal.Orphans          ()
import           Ide.Plugin.Error
import           Ide.PluginUtils                   (WithDeletions (SkipDeletions),
                                                    diffText)
import           Ide.Types                         (CommandFunction,
                                                    pluginGetClientCapabilities,
                                                    pluginSendRequest)
import           Language.LSP.Protocol.Message     (SMethod (SMethod_WorkspaceApplyEdit))
import           Language.LSP.Protocol.Types       (ApplyWorkspaceEditParams (ApplyWorkspaceEditParams),
                                                    ClientCapabilities,
                                                    Null (Null),
                                                    VersionedTextDocumentIdentifier,
                                                    WorkspaceEdit,
                                                    toNormalizedFilePath,
                                                    type (|?) (InR))

--------------------------------------------
-- Add Module
--------------------------------------------

addModuleCommand :: Recorder (WithPriority Log) -> CommandFunction IdeState ModuleInsertionConfig
addModuleCommand recorder state _ params@(ModuleInsertionConfig{..}) = do
  logWith recorder Debug $ LogCalledCabalAddModuleCommand params
  caps <- lift pluginGetClientCapabilities
  let env = (state, caps, modVerTxtDocId)
  edit <- getModuleEdit recorder env targetFile insertionStanza (T.unpack insertionLabel) (T.unpack moduleToInsert)
  void $ lift $ pluginSendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  logWith recorder Debug LogExecutedCommand
  pure $ InR Null

{- | Constructs prerequisites for the @executeConfig@
  and runs it, given path to the cabal file and a dependency message.
  Given the new contents of the cabal file constructs and returns the @edit@.

  Inspired by @main@ in cabal-add, Distribution.Client.Main
-}
getModuleEdit ::
  forall m.
  (MonadIO m) =>
  Recorder (WithPriority Log) ->
  (IdeState, ClientCapabilities, VersionedTextDocumentIdentifier) ->
  -- | The cabal file to edit.
  FilePath ->
  -- | The component to add the module to.
  ComponentName ->
  -- | The specific field in the component to add the module to.
  String ->
  -- | The module to add.
  String ->
  ExceptT PluginError m WorkspaceEdit
getModuleEdit recorder env cabalFilePath stanza targetFieldStr modulePath =
  mkCabalAddConfig
    recorder
    env
    cabalFilePath
    mkConfig
 where
  mkConfig :: (ByteString -> [Field Position] -> GenericPackageDescription -> ExceptT PluginError m Config)
  mkConfig cnfOrigContents fields packDescr = do
    let compName =
          case Add.resolveComponent cabalFilePath (fields, packDescr) $ Just $ CabalPretty.prettyShow stanza of
            Right x -> x
            Left _  -> error ""
    pure $
      Config
        { cnfOrigContents = cnfOrigContents
        , cnfFields = fields
        , cnfComponent = compName
        , cnfTargetField = if targetFieldStr == "exposed-modules" then ExposedModules else OtherModules
        , cnfAdditions = singleton $ B.pack modulePath
        }

--------------------------------------------
-- Add Package
--------------------------------------------

addPackageCommand :: Recorder (WithPriority Log) -> CommandFunction IdeState CabalAddPackageCommandParams
addPackageCommand recorder state _ params@(CabalAddPackageCommandParams{..}) = do
  logWith recorder Debug $ LogCalledCabalAddPackageCommand params
  let specifiedDep = case pkgVersion of
        Nothing  -> pkgDependency
        Just ver -> pkgDependency <> " ^>=" <> ver
  caps <- lift pluginGetClientCapabilities
  let env = (state, caps, pkgVerTxtDocId)
  edit <- getDependencyEdit recorder env pkgCabalPath pkgBuildTarget (T.unpack specifiedDep)
  void $ lift $ pluginSendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  logWith recorder Debug LogExecutedCommand
  pure $ InR Null

{- | Constructs prerequisites for the @executeConfig@
  and runs it, given path to the cabal file and a dependency message.
  Given the new contents of the cabal file constructs and returns the @edit@.
  Inspired by @main@ in cabal-add,
  Distribution.Client.Main
-}
getDependencyEdit ::
  forall m.
  (MonadIO m) =>
  Recorder (WithPriority Log) ->
  (IdeState, ClientCapabilities, VersionedTextDocumentIdentifier) ->
  FilePath ->
  Maybe String ->
  String ->
  ExceptT PluginError m WorkspaceEdit
getDependencyEdit recorder env cabalFilePath buildTarget dependency =
  mkCabalAddConfig recorder env cabalFilePath mkConfig
 where
  mkConfig :: (ByteString -> [Field Position] -> GenericPackageDescription -> ExceptT PluginError m Config)
  mkConfig cnfOrigContents fields packDescr = do
    let specVer = specVersion $ packageDescription packDescr
    (deps, compName) <-
      modifyError (\t -> PluginInternalError $ T.pack t) $ do
        deps <- validateDependency specVer dependency
        compName <- resolveComponent cabalFilePath (fields, packDescr) buildTarget
        pure (deps, compName)
    pure $
      Config
        { cnfOrigContents = cnfOrigContents
        , cnfFields = fields
        , cnfComponent = compName
        , cnfTargetField = BuildDepends
        , cnfAdditions = singleton deps
        }

--------------------------------------------
-- Shared Functions
--------------------------------------------

mkCabalAddConfig ::
  (MonadIO m) =>
  Recorder (WithPriority Log) ->
  (IdeState, ClientCapabilities, VersionedTextDocumentIdentifier) ->
  -- | The cabal file to edit
  FilePath ->
  -- | Function to generate a config to be used by cabal-add
  ( ByteString ->
    [Field Position] ->
    GenericPackageDescription ->
    ExceptT PluginError m Config
  ) ->
  ExceptT PluginError m WorkspaceEdit
mkCabalAddConfig recorder env cabalFilePath mkConfig = do
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
        Left err       -> throwE $ PluginInternalError $ T.pack err
        Right (f, gpd) -> pure (f, gpd)
    pure (cnfOrigContents, fields, packDescr)

  cabalAddConfig <- mkConfig cnfOrigContents fields packDescr

  case executeConfig (validateChanges packDescr) cabalAddConfig of
    Nothing ->
      throwE $
        PluginInternalError $
          T.pack $
            "Cannot extend "
              ++ show (cnfTargetField cabalAddConfig)
              ++ " of "
              ++ case (cnfComponent cabalAddConfig) of
                Right compName    -> showComponentName compName
                Left commonStanza -> show commonStanza
              ++ " in "
              ++ cabalFilePath
    Just newContents -> do
      let edit = diffText caps (verTxtDocId, T.decodeUtf8 cnfOrigContents) (T.decodeUtf8 newContents) SkipDeletions
      logWith recorder Debug $ LogCreatedEdit edit
      pure edit
