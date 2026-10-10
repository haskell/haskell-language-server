{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.CabalProject (descriptor, Log (..), getProjectContext, isCabalProjectFile) where

import           Control.Lens                                 ((^.))
import           Control.Monad                                (when)
import           Control.Monad.IO.Class
import qualified Data.List                                    as List
import           Data.List.NonEmpty                           (NonEmpty (..))
import qualified Data.List.NonEmpty                           as NE
import           Data.Map.Strict                              (Map)
import qualified Data.Map.Strict                              as Map
import           Data.Maybe                                   (fromMaybe,
                                                               mapMaybe)
import qualified Data.Text                                    as T
import           Development.IDE                              as D
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.Shake                   (restartShakeSession,
                                                               runIdeAction)
import qualified Development.IDE.Plugin.Completions.Logic     as Ghcide
import qualified Development.IDE.Types.Shake                  as Shake
import qualified Distribution.Fields                          as Syntax
import qualified Distribution.Parsec.Position                 as Syntax
import qualified Ide.Plugin.Cabal.Completion.CabalFields      as CabalFields
import qualified Ide.Plugin.Cabal.Completion.Completer.Simple as Simple
import qualified Ide.Plugin.Cabal.Completion.Completer.Types  as CompleterTypes
import qualified Ide.Plugin.Cabal.Completion.Completions      as Completions
import           Ide.Plugin.Cabal.Completion.Types            (CabalPrefixInfo (..),
                                                               Context,
                                                               FieldContext (..),
                                                               StanzaContext (..))
import qualified Ide.Plugin.Cabal.Completion.Types            as Types
import qualified Ide.Plugin.Cabal.Outline                     as Outline
import qualified Ide.Plugin.CabalProject.Data                 as Data
import qualified Ide.Plugin.CabalProject.Docs                 as Docs
import qualified Ide.Plugin.CabalProject.OfInterest           as OfInterest
import qualified Ide.Plugin.CabalProject.Rules                as Rules
import qualified Ide.Plugin.CabalProject.Tokens               as Tokens
import           Ide.Types
import qualified Language.LSP.Protocol.Lens                   as JL
import qualified Language.LSP.Protocol.Message                as LSP
import           Language.LSP.Protocol.Types
import           Prettyprinter                                ((<+>))
import           System.FilePath                              (takeFileName)
import qualified Text.Fuzzy.Parallel                          as Fuzzy

data Log
  = LogRule Rules.Log
  | LogTokens Tokens.Log
  | LogDocOpened Uri
  | LogDocModified Uri
  | LogDocSaved Uri
  | LogDocClosed Uri
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogRule log' -> pretty log'
    LogTokens log' -> pretty log'
    LogDocOpened uri ->
      "Opened text document:" <+> pretty (getUri uri)
    LogDocModified uri ->
      "Modified text document:" <+> pretty (getUri uri)
    LogDocSaved uri ->
      "Saved text document:" <+> pretty (getUri uri)
    LogDocClosed uri ->
      "Closed text document:" <+> pretty (getUri uri)

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId "Provides diagnostics, completions and semantic tokens for cabal.project files")
    { pluginLanguageIds =
        [ LanguageKind_Custom "cabal"
        , LanguageKind_Custom "cabalproject"
        , LanguageKind_Custom "plaintext"
        ]
    , pluginRules = Rules.projectRules ruleRecorder plId
    , pluginHandlers =
        mconcat
          [ mkPluginHandler LSP.SMethod_TextDocumentCompletion completion
          , mkPluginHandler LSP.SMethod_TextDocumentHover projectHover
          , mkPluginHandler LSP.SMethod_TextDocumentDocumentSymbol projectOutline
          -- Claim these methods so clients get a valid null response instead
          -- of a -32601 error (no other plugin supports them for
          -- "cabalproject" files).
          , mkPluginHandler LSP.SMethod_TextDocumentDocumentHighlight $ \_ _ _ -> pure (InR Null)
          , mkPluginHandler LSP.SMethod_TextDocumentSignatureHelp $ \_ _ _ -> pure (InR Null)
          , mkPluginHandler LSP.SMethod_TextDocumentSemanticTokensFull Tokens.projectSemanticTokensFull
          , mkPluginHandler LSP.SMethod_TextDocumentSemanticTokensFullDelta (Tokens.projectSemanticTokensFullDelta tokensRecorder)
          ]
    , pluginNotificationHandlers =
        mconcat
          [ mkPluginNotificationHandler LSP.SMethod_TextDocumentDidOpen $
              \ide vfs _ (DidOpenTextDocumentParams TextDocumentItem{_uri}) -> liftIO $
                whenUriFile _uri $ \file -> when (isCabalProjectFile file) $ do
                  log' Debug $ LogDocOpened _uri
                  restartShakeSession' (shakeExtras ide) vfs file "(opened)" $ do
                    OfInterest.addProjectFileOfInterest ide file Modified{firstOpen = True}
                    return [Shake.toKey GetModificationTime file]
          , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidChange $
              \ide vfs _ (DidChangeTextDocumentParams VersionedTextDocumentIdentifier{_uri} _) -> liftIO $
                whenUriFile _uri $ \file -> when (isCabalProjectFile file) $ do
                  log' Debug $ LogDocModified _uri
                  restartShakeSession' (shakeExtras ide) vfs file "(changed)" $ do
                    OfInterest.addProjectFileOfInterest ide file Modified{firstOpen = False}
                    return [Shake.toKey GetModificationTime file]
          , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidSave $
              \ide vfs _ (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $
                whenUriFile _uri $ \file -> when (isCabalProjectFile file) $ do
                  log' Debug $ LogDocSaved _uri
                  restartShakeSession' (shakeExtras ide) vfs file "(saved)" $ do
                    OfInterest.addProjectFileOfInterest ide file OnDisk
                    return [Shake.toKey GetModificationTime file, Shake.toKey GetPhysicalModificationTime file]
          , mkPluginNotificationHandler LSP.SMethod_TextDocumentDidClose $
              \ide vfs _ (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $
                whenUriFile _uri $ \file -> when (isCabalProjectFile file) $ do
                  log' Debug $ LogDocClosed _uri
                  restartShakeSession' (shakeExtras ide) vfs file "(closed)" $ do
                    OfInterest.deleteProjectFileOfInterest ide file
                    return [Shake.toKey GetModificationTime file]
          ]
    , pluginConfigDescriptor =
        defaultConfigDescriptor
          { configHasDiagnostics = True
          }
    }
  where
    log' = logWith recorder
    ruleRecorder = cmapWithPrio LogRule recorder
    tokensRecorder = cmapWithPrio LogTokens recorder
    whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
    whenUriFile uri act = maybe (pure ()) (act . toNormalizedFilePath') (uriToFilePath uri)

    restartShakeSession' shakeExtras vfs file actionMsg actionBetweenSession = do
      restartShakeSession shakeExtras (VFSModified vfs) (fromNormalizedFilePath file ++ " " ++ actionMsg) [] actionBetweenSession

{- | Checks whether the file is a cabal project file:
its base name is @cabal.project@ or has the prefix @cabal.project.@
(covering e.g. @cabal.project.freeze@ and @cabal.project.local@).
-}
isCabalProjectFile :: NormalizedFilePath -> Bool
isCabalProjectFile fp =
  let n = takeFileName (fromNormalizedFilePath fp)
   in n == "cabal.project" || "cabal.project." `List.isPrefixOf` n

-- ----------------------------------------------------------------
-- Hover
-- ----------------------------------------------------------------

projectHover :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentHover
projectHover ide _ hoverParams = do
  let TextDocumentIdentifier uri = hoverParams ^. JL.textDocument
      position = hoverParams ^. JL.position
  case uriToFilePath' uri of
    Nothing -> pure $ InR Null
    Just path
      | not (isCabalProjectFile (toNormalizedFilePath path)) -> pure $ InR Null
      | otherwise -> do
          mFields <- liftIO $ runAction "cabal-project.hover" ide $ useWithStale Rules.ParseProjectFields $ toNormalizedFilePath path
          pure $ case mFields of
            Nothing -> InR Null
            Just (fields, _) -> maybe (InR Null) (InL . uncurry Docs.renderHover) $
              Docs.projectHover (Types.lspPositionToCabalPosition position) fields

-- ----------------------------------------------------------------
-- Document symbols
-- ----------------------------------------------------------------

projectOutline :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentDocumentSymbol
projectOutline ide _ DocumentSymbolParams{_textDocument = TextDocumentIdentifier uri}
  | Just (toNormalizedFilePath' -> fp) <- uriToFilePath uri
  , isCabalProjectFile fp = do
      mFields <- liftIO $ runAction "cabal-project.outline" ide $ fmap fst <$> useWithStale Rules.ParseProjectFields fp
      pure $ maybe (InL []) (InR . InL . mapMaybe Outline.documentSymbolForField) mFields
  | otherwise = pure (InL [])

-- ----------------------------------------------------------------
-- Completion
-- ----------------------------------------------------------------
type ProjectContext = Context

completion :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentCompletion
completion ide _ complParams = do
  let TextDocumentIdentifier uri = complParams ^. JL.textDocument
      position = complParams ^. JL.position
  mContents <- liftIO $ runAction "cabal-project.getUriContents" ide $ getUriContents $ toNormalizedUri uri
  case (,) <$> mContents <*> uriToFilePath' uri of
    Nothing -> pure $ InR $ InR Null
    Just (cnts, path)
      | not (isCabalProjectFile (toNormalizedFilePath path)) -> pure $ InR $ InR Null
      | otherwise -> do
          -- We decide on `useWithStale` here, since `useWithStaleFast` often leads to
          -- the wrong completions being suggested.
          mFields <- liftIO $ runAction "cabal-project.fields" ide $ useWithStale Rules.ParseProjectFields $ toNormalizedFilePath path
          case mFields of
            Nothing -> pure $ InR $ InR Null
            Just (fields, _) -> do
              let lspPrefInfo = Ghcide.getCompletionPrefixFromRope position cnts
                  cabalPrefInfo = Completions.getCabalPrefixInfo path lspPrefInfo
                  res = computeCompletionsAt cabalPrefInfo fields $
                    CompleterTypes.Matcher $
                      Fuzzy.simpleFilter Fuzzy.defChunkSize Fuzzy.defMaxResults
              liftIO $ InL <$> res

computeCompletionsAt
  :: Types.CabalPrefixInfo
  -> [Syntax.Field Syntax.Position]
  -> CompleterTypes.Matcher T.Text
  -> IO [CompletionItem]
computeCompletionsAt prefInfo fields matcher = do
  let ctx = getProjectContext prefInfo fields
      completer = contextToCompleter ctx
      completerData =
        CompleterTypes.CompleterData
          { getLatestGPD = pure Nothing
          , getCabalCommonSections = pure Nothing
          , cabalPrefixInfo = prefInfo
          , stanzaName =
              case fst ctx of
                Types.Stanza _ name -> name
                _                   -> Nothing
          , matcher = matcher
          }
  completer mempty completerData

contextToCompleter :: ProjectContext -> CompleterTypes.Completer
-- if we are in the top level of the project file and not in a keyword context,
-- we can write any top level keywords or a section declaration
contextToCompleter (TopLevel, None) =
  Simple.constantCompleter (Map.keys Data.projectTopLevelFields ++ Data.projectSectionNames)
-- if we are in a keyword context in the top level,
-- we look up that keyword in the top level context and can complete its possible values
contextToCompleter (TopLevel, KeyWord kw) =
  fromMaybe Simple.noopCompleter $ Map.lookup kw Data.projectTopLevelFields
-- if we are in a section and not in a keyword context,
-- we can write any of the section's keywords
contextToCompleter (Stanza s _, None) =
  maybe Simple.noopCompleter (Simple.constantCompleter . Map.keys) $ Map.lookup s Data.projectStanzaKeywordMap
-- if we are in a section's keyword's context we can complete possible values of that keyword
contextToCompleter (Stanza s _, KeyWord kw) =
  maybe Simple.noopCompleter (fromMaybe Simple.noopCompleter . Map.lookup kw) $
    Map.lookup s Data.projectStanzaKeywordMap

-- | Determines the completion context for the given cursor position.
getProjectContext :: Types.CabalPrefixInfo -> [Syntax.Field Syntax.Position] -> ProjectContext
getProjectContext prefInfo fields =
  findCursorContext cursor (NE.singleton (0, TopLevel)) fields
  where
    cursor = Types.lspPositionToCabalPosition (Types.completionCursorPosition prefInfo)

    findCursorContext ::
      Syntax.Position ->
      -- ^ The cursor position we look for in the fields
      NonEmpty (Int, StanzaContext) ->
      -- ^ A stack of current stanza contexts and their starting line numbers
      [Syntax.Field Syntax.Position] ->
      ProjectContext
    findCursorContext cur parentHistory fs =
      case CabalFields.findFieldSection cur fs of
        Nothing -> (snd $ NE.head parentHistory, None)
        -- We found the most likely field. Now, are we starting a new field or completing an existing one?
        Just field@(Syntax.Field _ _) -> classifyFieldContext parentHistory cur field
        Just section@(Syntax.Section _ args sectionFields)
          | inSameLineAsSectionName section -> (stanzaCtx, None)
          | CabalFields.getFieldName section `elem` Docs.conditionalKeywords -> findCursorContext cur parentHistory sectionFields -- Ignore conditionals, they are not real sections
          | otherwise ->
              findCursorContext cur
                (NE.cons (Syntax.positionCol (CabalFields.getAnnotation section) + 1, Stanza (CabalFields.getFieldName section) (CabalFields.getOptionalSectionName args)) parentHistory)
                sectionFields
      where
        inSameLineAsSectionName section = Syntax.positionRow (CabalFields.getAnnotation section) == Syntax.positionRow cur
        stanzaCtx = snd $ NE.head parentHistory

    -- | Finds the cursor's context, where the cursor is already found to be in a specific field.
    classifyFieldContext :: NonEmpty (Int, StanzaContext) -> Syntax.Position -> Syntax.Field Syntax.Position -> ProjectContext
    classifyFieldContext ctx cur field
      -- the cursor is not indented enough to be within the field
      -- but still indented enough to be within the stanza
      | cursorColumn <= fieldColumn && minIndent <= cursorColumn = (stanzaCtx, None)
      -- the cursor is not in the current stanza's context as it is not indented enough
      | cursorColumn < minIndent = CabalFields.findStanzaForColumn cursorColumn ctx
      | cursorIsInFieldName = (stanzaCtx, None)
      | cursorIsBeforeFieldName = (stanzaCtx, None)
      | otherwise = (stanzaCtx, KeyWord (CabalFields.getFieldName field <> ":"))
      where
        (minIndent, stanzaCtx) = NE.head ctx

        cursorIsInFieldName = inSameLineAsFieldName &&
          fieldColumn <= cursorColumn &&
          cursorColumn <= fieldColumn + T.length (CabalFields.getFieldName field)

        cursorIsBeforeFieldName = inSameLineAsFieldName &&
          cursorColumn < fieldColumn

        inSameLineAsFieldName = Syntax.positionRow (CabalFields.getAnnotation field) == Syntax.positionRow cur

        cursorColumn = Syntax.positionCol cur
        fieldColumn = Syntax.positionCol (CabalFields.getAnnotation field)
