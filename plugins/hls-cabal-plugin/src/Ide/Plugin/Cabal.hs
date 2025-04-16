{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module Ide.Plugin.Cabal (descriptor, haskellInteractionDescriptor, Log (..)) where

import           Control.Lens                                  ((^.))
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class                     (lift)
import           Control.Monad.Trans.Maybe                     (runMaybeT)
import           Data.HashMap.Strict                           (HashMap)
import qualified Data.List                                     as List
import qualified Data.Maybe                                    as Maybe
import qualified Data.Text                                     ()
import qualified Data.Text                                     as T
import           Development.IDE                               as D
import           Development.IDE.Core.FileStore                (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.Shake                    (restartShakeSession)
import           Development.IDE.Graph                         (Key)
import           Development.IDE.LSP.HoverDefinition           (foundHover)
import qualified Development.IDE.Plugin.Completions.Logic      as Ghcide
import           Development.IDE.Types.Shake                   (toKey)
import qualified Distribution.Fields                           as Syntax
import           Distribution.Package                          (Dependency)
import           Distribution.PackageDescription               (allBuildDepends,
                                                                depPkgName,
                                                                unPackageName)
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import qualified Distribution.Parsec.Position                  as Syntax
import qualified Ide.Plugin.Cabal.CabalAdd.CodeAction          as CabalAdd
import qualified Ide.Plugin.Cabal.CabalAdd.Command             as CabalAdd
import           Ide.Plugin.Cabal.Completion.CabalFields       as CabalFields
import qualified Ide.Plugin.Cabal.Completion.Completer.Types   as CompleterTypes
import qualified Ide.Plugin.Cabal.Completion.Completions       as Completions
import           Ide.Plugin.Cabal.Completion.Types             (ParseCabalCommonSections (ParseCabalCommonSections),
                                                                ParseCabalFields (..),
                                                                ParseCabalFile (..))
import qualified Ide.Plugin.Cabal.Completion.Types             as Types
import           Ide.Plugin.Cabal.Definition                   (gotoDefinition)
import qualified Ide.Plugin.Cabal.FieldSuggest                 as FieldSuggest
import qualified Ide.Plugin.Cabal.Files                        as CabalAdd
import qualified Ide.Plugin.Cabal.LicenseSuggest               as LicenseSuggest
import qualified Ide.Plugin.Cabal.OfInterest                   as OfInterest
import           Ide.Plugin.Cabal.Orphans                      ()
import           Ide.Plugin.Cabal.Outline
import qualified Ide.Plugin.Cabal.Rules                        as Rules
import           Ide.Plugin.Error
import           Ide.Types
import qualified Language.LSP.Protocol.Lens                    as JL
import qualified Language.LSP.Protocol.Message                 as LSP
import           Language.LSP.Protocol.Types
import qualified Language.LSP.VFS                              as VFS
import           Text.Regex.TDFA

data Log
  = LogModificationTime NormalizedFilePath FileVersion
  | LogRule Rules.Log
  | LogOfInterest OfInterest.Log
  | LogDocOpened Uri
  | LogDocModified Uri
  | LogDocSaved Uri
  | LogDocClosed Uri
  | LogFOI (HashMap NormalizedFilePath FileOfInterestStatus)
  | LogCompletionContext Types.Context Position
  | LogCompletions Types.Log
  | LogCabalAdd CabalAdd.Log
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogRule log' -> pretty log'
    LogOfInterest log' -> pretty log'
    LogModificationTime nfp modTime ->
      "Modified:" <+> pretty (fromNormalizedFilePath nfp) <+> pretty (show modTime)
    LogDocOpened uri ->
      "Opened text document:" <+> pretty (getUri uri)
    LogDocModified uri ->
      "Modified text document:" <+> pretty (getUri uri)
    LogDocSaved uri ->
      "Saved text document:" <+> pretty (getUri uri)
    LogDocClosed uri ->
      "Closed text document:" <+> pretty (getUri uri)
    LogFOI files ->
      "Set files of interest to:" <+> viaShow files
    LogCompletionContext context position ->
      "Determined completion context:"
        <+> pretty context
        <+> "for cursor position:"
        <+> pretty position
    LogCompletions logs -> pretty logs
    LogCabalAdd logs -> pretty logs

{- | Some actions in cabal files can be triggered from haskell files.
This descriptor allows us to hook into the diagnostics of haskell source files and
allows us to provide code actions and commands that interact with `.cabal` files.
-}
haskellInteractionDescriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
haskellInteractionDescriptor recorder plId =
  (defaultPluginDescriptor plId "Provides the cabal-add code action in haskell files")
    { pluginHandlers =
        mconcat
          [ mkPluginHandler LSP.SMethod_TextDocumentCodeAction $ cabalAddDependencyCodeAction recorder
          , mkPluginHandler LSP.SMethod_TextDocumentCodeAction $ cabalAddModuleCodeAction recorder
          ]
    , pluginCommands =
        [ PluginCommand CabalAdd.cabalAddDependencyCommandId "add a dependency to a cabal file" (CabalAdd.addDependencyCommand cabalAddRecorder)
        , PluginCommand CabalAdd.cabalAddModuleCommandId "add a module to a cabal file" (CabalAdd.addModuleCommand cabalAddRecorder)
        ]
    }
 where
  cabalAddRecorder = cmapWithPrio LogCabalAdd recorder

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultCabalPluginDescriptor plId "Provides a variety of IDE features in cabal files")
    { pluginRules = Rules.cabalRules ruleRecorder plId
    , pluginHandlers =
        mconcat
          [ mkPluginHandler LSP.SMethod_TextDocumentCodeAction licenseSuggestCodeAction
          , mkPluginHandler LSP.SMethod_TextDocumentCompletion $ completion recorder
          , mkPluginHandler LSP.SMethod_TextDocumentDocumentSymbol moduleOutline
          , mkPluginHandler LSP.SMethod_TextDocumentCodeAction $ fieldSuggestCodeAction recorder
          , mkPluginHandler LSP.SMethod_TextDocumentDefinition gotoDefinition
          , mkPluginHandler LSP.SMethod_TextDocumentHover hover
          ]
    , pluginNotificationHandlers =
        mconcat
          [ mkPluginNotificationHandler LSP.SMethod_TextDocumentDidOpen $
              \ide vfs _ (DidOpenTextDocumentParams TextDocumentItem{_uri, _version}) -> liftIO $ do
                whenUriFile _uri $ \file -> do
                  log' Debug $ LogDocOpened _uri
                  restartCabalShakeSession (shakeExtras ide) vfs file "(opened)" $
                    OfInterest.addFileOfInterest ofInterestRecorder ide file Modified{firstOpen = True}
          , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidChange $
              \ide vfs _ (DidChangeTextDocumentParams VersionedTextDocumentIdentifier{_uri} _) -> liftIO $ do
                whenUriFile _uri $ \file -> do
                  log' Debug $ LogDocModified _uri
                  restartCabalShakeSession (shakeExtras ide) vfs file "(changed)" $
                    OfInterest.addFileOfInterest ofInterestRecorder ide file Modified{firstOpen = False}
          , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidSave $
              \ide vfs _ (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $ do
                whenUriFile _uri $ \file -> do
                  log' Debug $ LogDocSaved _uri
                  restartCabalShakeSession (shakeExtras ide) vfs file "(saved)" $
                    OfInterest.addFileOfInterest ofInterestRecorder ide file OnDisk
          , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidClose $
              \ide vfs _ (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $ do
                whenUriFile _uri $ \file -> do
                  log' Debug $ LogDocClosed _uri
                  restartCabalShakeSession (shakeExtras ide) vfs file "(closed)" $
                    OfInterest.deleteFileOfInterest ofInterestRecorder ide file
          ]
    , pluginConfigDescriptor =
        defaultConfigDescriptor
          { configHasDiagnostics = True
          }
    }
 where
  log' = logWith recorder
  ruleRecorder = cmapWithPrio LogRule recorder
  ofInterestRecorder = cmapWithPrio LogOfInterest recorder

  whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
  whenUriFile uri act = whenJust (uriToFilePath uri) $ act . toNormalizedFilePath'

{- | Helper function to restart the shake session, specifically for modifying .cabal files.
No special logic, just group up a bunch of functions you need for the base
Notification Handlers.

To make sure diagnostics are up to date, we need to tell shake that the file was touched and
needs to be re-parsed. That's what we do when we record the dirty key that our parsing
rule depends on.
Then we restart the shake session, so that changes to our virtual files are actually picked up.
-}
restartCabalShakeSession :: ShakeExtras -> VFS.VFS -> NormalizedFilePath -> String -> IO [Key] -> IO ()
restartCabalShakeSession shakeExtras vfs file actionMsg actionBetweenSession = do
  restartShakeSession shakeExtras (VFSModified vfs) (fromNormalizedFilePath file ++ " " ++ actionMsg) [] $ do
    keys <- actionBetweenSession
    return (toKey GetModificationTime file : keys)

-- ----------------------------------------------------------------
-- Code Actions
-- ----------------------------------------------------------------

licenseSuggestCodeAction :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
licenseSuggestCodeAction ideState _ (CodeActionParams _ _ (TextDocumentIdentifier uri) _range CodeActionContext{_diagnostics = diags}) = do
  maxCompls <- fmap maxCompletions . liftIO $ runAction "cabal-plugin.suggestLicense" ideState getClientConfigAction
  pure $ InL $ diags >>= (fmap InR . LicenseSuggest.licenseErrorAction maxCompls uri)

{- | CodeActions for correcting field names with typos in them.

Provides CodeActions that fix typos in both stanzas and top-level field names.
The suggestions are computed based on the completion context, where we "move" a fake cursor
to the end of the field name and trigger cabal file completions. The completions are then
suggested to the user.

TODO: Relying on completions here often does not produce the desired results, we should
use some sort of fuzzy matching in the future, see issue #4357.
-}
fieldSuggestCodeAction :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
fieldSuggestCodeAction recorder ide _ (CodeActionParams _ _ (TextDocumentIdentifier uri) _ CodeActionContext{_diagnostics = diags}) = do
  mContents <- liftIO $ runAction "cabal-plugin.getUriContents" ide $ getUriContents $ toNormalizedUri uri
  case (,) <$> mContents <*> uriToFilePath' uri of
    Nothing -> pure $ InL []
    Just (fileContents, path) -> do
      -- We decide on `useWithStale` here, since `useWithStaleFast` often leads to the wrong completions being suggested.
      -- In case it fails, we still will get some completion results instead of an error.
      mFields <- liftIO $ runAction "cabal-plugin.fields" ide $ useWithStale ParseCabalFields $ toNormalizedFilePath path
      case mFields of
        Nothing ->
          pure $ InL []
        Just (cabalFields, _) -> do
          let fields = Maybe.mapMaybe FieldSuggest.fieldErrorName diags
          results <- forM fields (getSuggestion fileContents path cabalFields)
          pure $ InL $ map InR $ concat results
 where
  getSuggestion fileContents fp cabalFields (fieldName, Diagnostic{_range = _range@(Range (Position lineNr col) _)}) = do
    let
      -- Compute where we would anticipate the cursor to be.
      fakeLspCursorPosition = Position lineNr (col + fromIntegral (T.length fieldName))
      lspPrefixInfo = Ghcide.getCompletionPrefixFromRope fakeLspCursorPosition fileContents
      cabalPrefixInfo = Completions.getCabalPrefixInfo fp lspPrefixInfo
    completions <- liftIO $ computeCompletionsAt recorder ide cabalPrefixInfo fp cabalFields
    let completionTexts = fmap (^. JL.label) completions
    pure $ FieldSuggest.fieldErrorAction uri fieldName completionTexts _range

cabalAddDependencyCodeAction :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
cabalAddDependencyCodeAction _ state plId (CodeActionParams _ _ (TextDocumentIdentifier uri) _ CodeActionContext{_diagnostics = diags}) = do
  let suggestions = concatMap CabalAdd.hiddenPackageSuggestion diags
  case suggestions of
    [] -> pure $ InL []
    _ -> do
      haskellFilePath <- uriToFilePathE uri
      mbCabalFile <- liftIO $ CabalAdd.findResponsibleCabalFile haskellFilePath
      case mbCabalFile of
        Nothing -> pure $ InL []
        Just cabalFilePath -> do
          verTxtDocId <-
            runActionE "cabalAdd.getVersionedTextDoc" state $
              lift $
                getVersionedTextDoc $
                  TextDocumentIdentifier (filePathToUri cabalFilePath)
          mbGPD <- liftIO $ runAction "cabal.cabal-add" state $ useWithStale ParseCabalFile $ toNormalizedFilePath cabalFilePath
          case mbGPD of
            Nothing -> pure $ InL []
            Just (gpd, _) -> do
              actions <-
                liftIO $
                  CabalAdd.addDependencySuggestCodeAction
                    plId
                    verTxtDocId
                    suggestions
                    haskellFilePath
                    cabalFilePath
                    gpd
              pure $ InL $ fmap InR actions

cabalAddModuleCodeAction :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
cabalAddModuleCodeAction recorder state plId (CodeActionParams _ _ (TextDocumentIdentifier uri) _ CodeActionContext{_diagnostics = diags}) =
  case List.find CabalAdd.isUnknownModuleDiagnostic diags of
    Just diag ->
      do
        haskellFilePath <- uriToFilePathE uri
        mbCabalFile <- liftIO $ CabalAdd.findResponsibleCabalFile haskellFilePath
        case mbCabalFile of
          Nothing -> pure $ InL []
          Just cabalFilePath -> do
            verTextDocId <-
              runActionE "cabalAdd.getVersionedTextDoc" state $
                lift $
                  getVersionedTextDoc $
                    TextDocumentIdentifier (filePathToUri cabalFilePath)
            (gpd, _) <- runActionE "cabal.cabal-add" state $ useWithStaleE ParseCabalFile $ toNormalizedFilePath cabalFilePath
            actions <-
              CabalAdd.collectModuleInsertionOptions
                (cmapWithPrio LogCabalAdd recorder)
                plId
                verTextDocId
                diag
                cabalFilePath
                gpd
                uri
            pure $ InL $ fmap InR actions
    Nothing -> pure $ InL []

{- | Handler for hover messages.

If the cursor is hovering on a dependency, add a documentation link to that dependency.
-}
hover :: PluginMethodHandler IdeState LSP.Method_TextDocumentHover
hover ide _ msgParam = do
  nfp <- getNormalizedFilePathE uri
  cabalFields <- runActionE "cabal.cabal-hover" ide $ useE ParseCabalFields nfp
  case CabalFields.findTextWord cursor cabalFields of
    Nothing ->
      pure $ InR Null
    Just cursorText -> do
      gpd <- runActionE "cabal.GPD" ide $ useE ParseCabalFile nfp
      let depsNames = map dependencyName $ allBuildDepends $ flattenPackageDescription gpd
      case filterVersion cursorText of
        Nothing -> pure $ InR Null
        Just txt ->
          if txt `elem` depsNames
            then pure $ foundHover (Nothing, [txt <> "\n", documentationText txt])
            else pure $ InR Null
 where
  cursor = Types.lspPositionToCabalPosition (msgParam ^. JL.position)
  uri = msgParam ^. JL.textDocument . JL.uri

  dependencyName :: Dependency -> T.Text
  dependencyName dep = T.pack $ unPackageName $ depPkgName dep

  -- \| Removes version requirements like
  -- `==1.0.0.0`, `>= 2.1.1` that could be included in
  -- hover message. Assumes that the dependency consists
  -- of alphanums with dashes in between. Ends with an alphanum.
  --
  -- Examples:
  -- >>> filterVersion "imp-deps>=2.1.1"
  -- "imp-deps"
  filterVersion :: T.Text -> Maybe T.Text
  filterVersion msg = getMatch (msg =~ regex)
   where
    regex :: T.Text
    regex = "([a-zA-Z0-9-]*[a-zA-Z0-9])"

    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> Maybe T.Text
    getMatch (_, _, _, [dependency]) = Just dependency
    getMatch (_, _, _, _)            = Nothing -- impossible case

  documentationText :: T.Text -> T.Text
  documentationText package = "[Documentation](https://hackage.haskell.org/package/" <> package <> ")"

-- ----------------------------------------------------------------
-- Completion
-- ----------------------------------------------------------------

completion :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'LSP.Method_TextDocumentCompletion
completion recorder ide _ complParams = do
  let TextDocumentIdentifier uri = complParams ^. JL.textDocument
      position = complParams ^. JL.position
  mContents <- liftIO $ runAction "cabal-plugin.getUriContents" ide $ getUriContents $ toNormalizedUri uri
  case (,) <$> mContents <*> uriToFilePath' uri of
    Just (cnts, path) -> do
      -- We decide on `useWithStale` here, since `useWithStaleFast` often leads to the wrong completions being suggested.
      -- In case it fails, we still will get some completion results instead of an error.
      mFields <- liftIO $ runAction "cabal-plugin.fields" ide $ useWithStale ParseCabalFields $ toNormalizedFilePath path
      case mFields of
        Nothing ->
          pure . InR $ InR Null
        Just (fields, _) -> do
          let lspPrefInfo = Ghcide.getCompletionPrefixFromRope position cnts
              cabalPrefInfo = Completions.getCabalPrefixInfo path lspPrefInfo
          let res = computeCompletionsAt recorder ide cabalPrefInfo path fields
          liftIO $ fmap InL res
    Nothing -> pure . InR $ InR Null

computeCompletionsAt :: Recorder (WithPriority Log) -> IdeState -> Types.CabalPrefixInfo -> FilePath -> [Syntax.Field Syntax.Position] -> IO [CompletionItem]
computeCompletionsAt recorder ide prefInfo fp fields = do
  runMaybeT (context fields) >>= \case
    Nothing -> pure []
    Just ctx -> do
      logWith recorder Debug $ LogCompletionContext ctx pos
      let completer = Completions.contextToCompleter ctx
      let completerData =
            CompleterTypes.CompleterData
              { getLatestGPD = do
                  -- We decide on useWithStaleFast here, since we mostly care about the file's meta information,
                  -- thus, a quick response gives us the desired result most of the time.
                  -- The `withStale` option is very important here, since we often call this rule with invalid cabal files.
                  mGPD <- runAction "cabal-plugin.modulesCompleter.gpd" ide $ useWithStale ParseCabalFile $ toNormalizedFilePath fp
                  pure $ fmap fst mGPD
              , getCabalCommonSections = runAction "cabal-plugin.commonSections" ide $ use ParseCabalCommonSections $ toNormalizedFilePath fp
              , cabalPrefixInfo = prefInfo
              , stanzaName =
                  case fst ctx of
                    Types.Stanza _ name -> name
                    _                   -> Nothing
              }
      completions <- completer completerRecorder completerData
      pure completions
 where
  pos = Types.completionCursorPosition prefInfo
  context fields = Completions.getContext completerRecorder prefInfo fields
  completerRecorder = cmapWithPrio LogCompletions recorder
