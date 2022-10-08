{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE DeriveGeneric           #-}
{-# LANGUAGE DuplicateRecordFields   #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE LambdaCase              #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE OverloadedStrings       #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE DisambiguateRecordFields#-}

module Ide.Plugin.Cabal where

import           Control.Concurrent.STM
import           Control.DeepSeq                 (NFData)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import qualified Data.ByteString                 as BS
import           Data.Hashable
import qualified Data.List.NonEmpty              as NE
import           Data.Maybe                      (mapMaybe)
import qualified Data.Text.Encoding              as Encoding
import           Data.Typeable
import           Development.IDE                 as D
import           Development.IDE.Core.Shake      (restartShakeSession)
import qualified Development.IDE.Core.Shake      as Shake
import           GHC.Generics
import qualified Ide.Plugin.Cabal.Diagnostics    as Diagnostics
import qualified Ide.Plugin.Cabal.LicenseSuggest as LicenseSuggest
import qualified Ide.Plugin.Cabal.FieldSuggest as FieldSuggest
import qualified Ide.Plugin.Cabal.Parse          as Parse
import           Ide.Plugin.Config               (Config)
import           Ide.Types
import qualified Language.LSP.Server                as LSP
import           Language.LSP.Types
import qualified Language.LSP.Types              as LSP
import qualified Language.LSP.VFS                as VFS
import qualified Data.Text as T
import qualified Language.LSP.Types.Lens as JL
import qualified Language.LSP.Types as J
import Distribution.Compat.Lens((^.))
import qualified Text.Fuzzy.Parallel as Fuzzy
import           Data.Map (Map)
import qualified Data.Map as Map
import Language.LSP.VFS (VirtualFile)
import qualified Data.Text.Utf16.Rope as Rope
import qualified Data.List as List
import qualified Data.HashMap.Strict as MapStrict
data Log
  = LogModificationTime NormalizedFilePath (Maybe FileVersion)
  | LogDiagnostics NormalizedFilePath [FileDiagnostic]
  | LogShake Shake.Log
  | LogDocOpened Uri
  | LogDocModified Uri
  | LogDocSaved Uri
  | LogDocClosed Uri
  deriving Show

instance Pretty Log where
  pretty = \case
    LogShake log' -> pretty log'
    LogModificationTime nfp modTime  ->
      "Modified:" <+> pretty (fromNormalizedFilePath nfp) <+> pretty (show modTime)
    LogDiagnostics nfp diags ->
      "Diagnostics for" <+> pretty (fromNormalizedFilePath nfp) <> ":" <+> pretty (show diags)
    LogDocOpened uri ->
      "Opened text document:" <+> pretty (getUri uri)
    LogDocModified uri ->
      "Modified text document:" <+> pretty (getUri uri)
    LogDocSaved uri ->
      "Saved text document:" <+> pretty (getUri uri)
    LogDocClosed uri ->
      "Closed text document:" <+> pretty (getUri uri)

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultCabalPluginDescriptor plId)
  { pluginRules = cabalRules recorder
  , pluginHandlers = mkPluginHandler STextDocumentCodeAction licenseSuggestCodeAction
                      <> mkPluginHandler J.STextDocumentCompletion completion
                      <> mkPluginHandler STextDocumentCodeAction fieldSuggestCodeAction
  , pluginNotificationHandlers = mconcat
  [ mkPluginNotificationHandler LSP.STextDocumentDidOpen $
      \ide vfs _ (DidOpenTextDocumentParams TextDocumentItem{_uri,_version}) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        log' Debug $ LogDocOpened _uri
        restartCabalShakeSession ide vfs file "(opened)"

  , mkPluginNotificationHandler LSP.STextDocumentDidChange $
      \ide vfs _ (DidChangeTextDocumentParams VersionedTextDocumentIdentifier{_uri} _) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        log' Debug $ LogDocModified _uri
        restartCabalShakeSession ide vfs file "(changed)"

  , mkPluginNotificationHandler LSP.STextDocumentDidSave $
      \ide vfs _ (DidSaveTextDocumentParams TextDocumentIdentifier{_uri} _) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        log' Debug $ LogDocSaved _uri
        restartCabalShakeSession ide vfs file "(saved)"

  , mkPluginNotificationHandler LSP.STextDocumentDidClose $
      \ide vfs _ (DidCloseTextDocumentParams TextDocumentIdentifier{_uri}) -> liftIO $ do
      whenUriFile _uri $ \file -> do
        log' Debug $ LogDocClosed _uri
        restartCabalShakeSession ide vfs file "(closed)"
  ]
  }
  where
    log' = logWith recorder

    whenUriFile :: Uri -> (NormalizedFilePath -> IO ()) -> IO ()
    whenUriFile uri act = whenJust (LSP.uriToFilePath uri) $ act . toNormalizedFilePath'

-- | Helper function to restart the shake session, specifically for modifying .cabal files.
-- No special logic, just group up a bunch of functions you need for the base
-- Notification Handlers.
restartCabalShakeSession :: IdeState -> VFS.VFS -> NormalizedFilePath -> String -> IO ()
restartCabalShakeSession ide vfs file actionMsg = do
  join $ atomically $ Shake.recordDirtyKeys (shakeExtras ide) GetModificationTime [file]
  restartShakeSession (shakeExtras ide) (VFSModified vfs) (fromNormalizedFilePath file ++ " " ++ actionMsg) []
  join $ Shake.shakeEnqueue (shakeExtras ide) $ Shake.mkDelayedAction "cabal parse modified" Info $ void $ use ParseCabal file

-- ----------------------------------------------------------------
-- Plugin Rules
-- ----------------------------------------------------------------

data ParseCabal = ParseCabal
    deriving (Eq, Show, Typeable, Generic)
instance Hashable ParseCabal
instance NFData   ParseCabal

type instance RuleResult ParseCabal = ()

cabalRules :: Recorder (WithPriority Log) -> Rules ()
cabalRules recorder = do
  define (cmapWithPrio LogShake recorder) $ \ParseCabal file -> do
    t <- use GetModificationTime file
    log' Debug $ LogModificationTime file t
    mVirtualFile <- Shake.getVirtualFile file
    contents <- case mVirtualFile of
      Just vfile -> pure $ Encoding.encodeUtf8 $ VFS.virtualFileText vfile
      Nothing -> do
        liftIO $ BS.readFile $ fromNormalizedFilePath file

    (pWarnings, pm) <- liftIO $ Parse.parseCabalFileContents contents
    let warningDiags = fmap (Diagnostics.warningDiagnostic file) pWarnings
    case pm of
      Left (_cabalVersion, pErrorNE) -> do
        let errorDiags = NE.toList $ NE.map (Diagnostics.errorDiagnostic file) pErrorNE
            allDiags = errorDiags <> warningDiags
        log' Debug $ LogDiagnostics file allDiags
        pure (allDiags, Nothing)
      Right _ -> do
        log' Debug $ LogDiagnostics file warningDiags
        pure (warningDiags, Just ())
  where
    log' = logWith recorder

-- ----------------------------------------------------------------
-- Code Actions
-- ----------------------------------------------------------------

-- | CodeActions for unsupported license values 
licenseSuggestCodeAction
  :: IdeState
  -> PluginId
  -> CodeActionParams
  -> LSP.LspM Config (Either ResponseError (ResponseResult 'TextDocumentCodeAction))
licenseSuggestCodeAction _ _ (CodeActionParams _ _ (TextDocumentIdentifier uri) _range CodeActionContext{_diagnostics=List diags}) =
  pure $ Right $ List $ mapMaybe (fmap InR . LicenseSuggest.licenseErrorAction uri) diags

-- | CodeActions for misspelled fields in cabal files
--   both for toplevel fields, and fields in stanzas.
--   uses same logic as completions but reacts on diagnostics from cabal
fieldSuggestCodeAction
  :: IdeState
  -> PluginId
  -> CodeActionParams
  -> LSP.LspM Config (Either ResponseError (ResponseResult 'TextDocumentCodeAction))
fieldSuggestCodeAction _ _ (CodeActionParams _ _ (TextDocumentIdentifier uri) _ CodeActionContext{_diagnostics=List diags}) =do
  cnts <- LSP.getVirtualFile $ toNormalizedUri uri
  let fields = mapMaybe FieldSuggest.fieldErrorName diags
  results <- forM fields (getSuggestion cnts)
  return $ Right $  J.List $ map InR $ concat results
  where
    getSuggestion :: Maybe VirtualFile -> (T.Text,Diagnostic) -> LSP.LspM Config [CodeAction]
    getSuggestion cnts (field,Diagnostic{ _range=_range@(Range (Position lineNr col) _) })= do
      completions <- completionAtPosition uri (Position lineNr (col + fromIntegral (T.length field))) cnts
      pure $ fieldErrorAction uri field completions _range
-- ----------------------------------------------------------------
-- Completion
-- ----------------------------------------------------------------
-- | Generates similiar field names for given file, position and contents of file
completionAtPosition :: Uri -> Position -> Maybe VirtualFile -> LSP.LspM Config [T.Text]
completionAtPosition uri pos contents = do
  case (contents, uriToFilePath' uri) of
    (Just cnts, Just _path) -> do
      pref <- VFS.getCompletionPrefix pos cnts
      return $ result pref cnts
    _ -> return  []
  where
    result :: Maybe VFS.PosPrefixInfo -> VirtualFile -> [T.Text]
    result Nothing _ =  []
    result (Just pfix) cnts
      | VFS.cursorPos pfix ^. JL.line == 0 = [cabalVersionKeyword]
      | Stanza s <- findCurrentLevel (getPreviousLines pfix cnts) =
          case Map.lookup s stanzaKeywordMap of
            Nothing ->getCompletions pfix topLevelKeywords
            Just l -> getCompletions pfix l ++ (getCompletions pfix $ Map.keys stanzaKeywordMap)
      | otherwise =
        getCompletions pfix topLevelKeywords
          where
            topLevelKeywords = cabalKeywords ++ Map.keys stanzaKeywordMap

completion :: PluginMethodHandler IdeState 'J.TextDocumentCompletion
completion _ide _ complParams = do
  let (J.TextDocumentIdentifier uri) = complParams ^. JL.textDocument
      position = complParams ^. JL.position
  contents <- LSP.getVirtualFile $ toNormalizedUri uri
  fmap (Right . J.InL . J.List . fmap buildCompletion) $ completionAtPosition uri position contents

-- | Takes info about the current cursor position and a set of possible keywords 
--   and creates completion suggestions that fit the current input from the given list
getCompletions :: VFS.PosPrefixInfo -> [T.Text] -> [T.Text]
getCompletions pfix l =
  map
     Fuzzy.original
    (Fuzzy.simpleFilter 1000 10 (VFS.prefixText pfix) l)

-- | Parse the given set of lines (starting before current cursor position 
--   up to the start of the file) to find the nearest stanza declaration, 
--   if none is found we are in the top level
findCurrentLevel :: [T.Text] -> Context
findCurrentLevel [] = TopLevel
findCurrentLevel (cur : xs)
  | Just s <- stanza = Stanza s
  | otherwise = findCurrentLevel xs
  where
    stanza = List.find (`T.isPrefixOf` cur) (Map.keys stanzaKeywordMap)

-- | Get all lines before the given cursor position in the given file 
--   and reverse them since we want to traverse starting from our current position
getPreviousLines :: VFS.PosPrefixInfo -> VirtualFile -> [T.Text]
getPreviousLines pos cont = reverse $ take (fromIntegral currentLine) allLines
  where
    allLines = Rope.lines $ cont ^. VFS.file_text
    currentLine = VFS.cursorPos pos ^. JL.line


data Context
  = TopLevel
  -- ^ top level context in a cabal file such as 'author'
  | Stanza T.Text
  -- ^ nested context in a cabal file, such as 'library', which has nested keywords, specific to the stanza
  deriving (Eq)

-- | Keyword for cabal version required to be the top line in a cabal file
cabalVersionKeyword :: T.Text
cabalVersionKeyword = "cabal-version:"

-- | Top level keywords of a cabal file
cabalKeywords :: [T.Text]
cabalKeywords =
  [
    "name:",
    "version:",
    "build-type:",
    "license:",
    "license-file:",
    "license-files:",
    "copyright:",
    "author:",
    "maintainer:",
    "stability:",
    "homepage:",
    "bug-reports:",
    "package-url:",
    "synopsis:",
    "description:",
    "category:",
    "tested-with:",
    "data-files:",
    "data-dir:",
    "data-dir:",
    "extra-doc-files:",
    "extra-tmp-files:"
  ]

-- | Map, containing all stanzas in a cabal file as keys and lists of their possible nested keywords as values
stanzaKeywordMap :: Map T.Text [T.Text]
stanzaKeywordMap = Map.fromList [("library", [
    "exposed-modules:",
    "virtual-modules:",
    "exposed:",
    "visibility:",
    "reexported-modules:",
    "signatures:"
  ])]


-- TODO move out toplevel commands i.e. test-suite
-- cabalTestKeywords :: [T.Text]
-- cabalTestKeywords = 
--   [
--     "test-suite",
--     "type:",
--     "main-is:",
--     "test-module:",
--     "benchmark",
--     "main-is:",
--     "foreign-library",
--     "type:",
--     "options:",
--     "mod-def-file:",
--     "lib-version-info:",
--     "lib-version-linux:",
--     "build-depends:",
--     "other-modules:",
--     "hs-source-dir:",
--     "hs-source-dirs:",
--     "default-extensions:",
--     "other-extensions:",
--     "default-language:",
--     "other-languages:",
--     "extensions:",
--     "build-tool-depends:",
--     "build-tools:",
--     "buildable:",
--     "ghc-options:",
--     "ghc-prof-options:",
--     "ghc-shared-options:",
--     "ghcjs-options:",
--     "ghcjs-prof-options:",
--     "ghcjs-shared-options:",
--     "includes:",
--     "install-includes:",
--     ("include-dirs:", "directory list"),
--     ("c-sources:", "filename list"),
--     ("cxx-sources:", "filename list"),
--     ("asm-sources:", "filename list"),
--     ("cmm-sources:", "filename list"),
--     ("js-sources:", "filename list"),
--     ("extra-libraries:", "token list"),
--     ("extra-libraries-static:", "token list"),
--     ("extra-ghci-libraries:", "token list"),
--     ("extra-bundled-libraries:", "token list"),
--     ("extra-lib-dirs:", "directory list")
--     ("extra-lib-dirs-static:", "directory list"),
--     ("extra-library-flavours:", "notsure"),
--     ("extra-dynamic-library-flavours:", "notsure"),
--     ("cc-options:", "token list"),
--     ("cpp-options:", "token list"),
--     ("cxx-options:", "token list"),
--     ("cmm-options:", "token list"),
--     ("asm-options:", "token list"),
--     ("ld-options:", "token list"),
--     ("hsc2hs-options:", "token list"),
--     ("pkgconfig-depends:", "package list"),
--     ("frameworks:", "token list"),
--     ("extra-framework-dirs:", "directory list"),
--     ("mixins:", "mixin list")
--   ]

-- cabalFlagKeywords :: [(T.Text, T.Text)]
-- cabalFlagKeywords = 
--   [
--     ("flag", "name"),
--     ("description:", "freeform"),
--     ("default:", "boolean"),
--     ("manual:", "boolean")
--   ]

-- cabalStanzaKeywords :: [(T.Text, T.Text)]
-- cabalStanzaKeywords = 
--   [
--     ("common", "name"),
--     ("import:", "token-list")
--   ]

-- cabalSourceRepoKeywords :: [(T.Text, T.Text)]
-- cabalSourceRepoKeywords = 
--   [
--     ("source-repository", ""),
--     ("type:", "token"),
--     ("location:", "URL")
--       ]

buildCompletion :: T.Text -> J.CompletionItem
buildCompletion label =
  J.CompletionItem label (Just J.CiKeyword) Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing

-- | Generate all code action for given file, error field in position and suggestions
fieldErrorAction
  :: Uri
  -- ^ File for which the diagnostic was generated
  -> T.Text
  -- ^ Original field
  -> [T.Text]
  -- ^ Suggestions
  -> Range
  -- ^ location of diagnostic
  ->  [CodeAction]
fieldErrorAction uri original suggestions range =
  fmap mkCodeAction  suggestions
  where
    mkCodeAction suggestion =
      let
        -- Range returned by cabal here represents fragment from start of 
        -- offending identifier to end of line, we modify it to the end of identifier
        adjustRange (Range rangeFrom@(Position lineNr col) _) =
          Range rangeFrom (Position lineNr (col + fromIntegral (T.length original)))
        title = "Replace with " <> suggestion'
        tedit = [TextEdit (adjustRange range ) suggestion']
        edit  = WorkspaceEdit (Just $ MapStrict.singleton uri $ List tedit) Nothing Nothing
      in CodeAction title (Just CodeActionQuickFix) (Just $ List []) Nothing Nothing (Just edit) Nothing Nothing
      where
        -- dropping colon from the end of suggestion
        suggestion' :: T.Text
        suggestion' = T.dropEnd 1 suggestion