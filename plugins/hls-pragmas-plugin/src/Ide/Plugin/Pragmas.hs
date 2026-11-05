{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

-- | Provides code actions to add missing pragmas (whenever GHC suggests to)
module Ide.Plugin.Pragmas
  ( suggestPragmaDescriptor
  , completionDescriptor
  , hoverDescriptor
  , suggestDisableWarningDescriptor
  -- For testing
  , validPragmas
  , AppearWhere(..)
  ) where

import           Control.Lens                             hiding (List)
import           Control.Monad.IO.Class                   (MonadIO (liftIO))
import qualified Data.Aeson                               as JSON
import           Data.Char                                (isAlphaNum)
import qualified Data.Foldable                            as Foldable
import           Data.List.Extra                          (nubOrdOn)
import qualified Data.Map                                 as M
import           Data.Maybe                               (mapMaybe)
import qualified Data.Text                                as T
import qualified Data.Text.Utf16.Rope.Mixed               as Rope
import           Development.IDE                          hiding (line)
import           Development.IDE.Core.Compile             (sourceParser,
                                                           sourceTypecheck)
import           Development.IDE.Core.FileStore           (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Error         (GhcHint (SuggestExtension),
                                                           LanguageExtensionHint (..),
                                                           diagnosticHints,
                                                           msgEnvelopeErrorL)
import           Development.IDE.Plugin.Completions       (ghcideCompletionsPluginPriority)
import           Development.IDE.Plugin.Completions.Logic (getCompletionPrefixFromRope)
import           Development.IDE.Plugin.Completions.Types (PosPrefixInfo (..))
import qualified Development.IDE.Spans.Pragmas            as Pragmas
import           Ide.Plugin.Error
import           Ide.Types
import qualified Language.LSP.Protocol.Lens               as L
import qualified Language.LSP.Protocol.Message            as LSP
import qualified Language.LSP.Protocol.Types              as LSP
import qualified Text.Fuzzy                               as Fuzzy

-- ---------------------------------------------------------------------

suggestPragmaDescriptor :: PluginId -> PluginDescriptor IdeState
suggestPragmaDescriptor plId = (defaultPluginDescriptor plId "Provides a code action to add missing LANGUAGE pragmas")
  { pluginHandlers = mkPluginHandler LSP.SMethod_TextDocumentCodeAction suggestPragmaProvider
  , pluginPriority = defaultPluginPriority + 1000
  }

completionDescriptor :: PluginId -> PluginDescriptor IdeState
completionDescriptor plId = (defaultPluginDescriptor plId "Provides completion of LANGAUGE pragmas")
  { pluginHandlers = mkPluginHandler LSP.SMethod_TextDocumentCompletion completion
  , pluginPriority = ghcideCompletionsPluginPriority + 1
  }

hoverDescriptor :: PluginId -> PluginDescriptor IdeState
hoverDescriptor plId = (defaultPluginDescriptor plId "Provides documentation for LANGUAGE pragmas")
  { Ide.Types.pluginHandlers = mkPluginHandler LSP.SMethod_TextDocumentHover pragmaHover
  }

suggestDisableWarningDescriptor :: PluginId -> PluginDescriptor IdeState
suggestDisableWarningDescriptor plId = (defaultPluginDescriptor plId "Provides a code action to disable warnings")
  { pluginHandlers = mkPluginHandler LSP.SMethod_TextDocumentCodeAction suggestDisableWarningProvider
    -- #3636 Suggestions to disable warnings should appear last.
  , pluginPriority = 0
  }

pragmaHover :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentHover
pragmaHover state _ params = do
  let uri = params ^. L.textDocument . L.uri
      position = params ^. L.position
  contents <- liftIO $ runAction "Pragmas.GetUriContents" state $ getUriContents $ toNormalizedUri uri
  pure $ case contents >>= pragmaAtPosition position . Rope.toText of
    Just extension -> LSP.InL $ LSP.Hover (LSP.InL $ pragmaDocumentation extension) Nothing
    Nothing -> LSP.InR LSP.Null

pragmaAtPosition :: LSP.Position -> T.Text -> Maybe T.Text
pragmaAtPosition (LSP.Position line column) contents = do
  lineText <- atMay (T.splitOn "\n" contents) (fromIntegral line)
  let column' = fromIntegral column
  (_, _, extension) <- case filter (\(start, end, _) -> start <= column' && column' < end)
      (languageExtensions lineText) of
    range : _ -> Just range
    []        -> Nothing
  pure extension

languageExtensions :: T.Text -> [(Int, Int, T.Text)]
languageExtensions lineText = concatMap extensionsInPragma (T.breakOnAll "{-# LANGUAGE " lineText)
  where
    prefix = "{-# LANGUAGE "

    extensionsInPragma (before, after) =
      let body = T.takeWhile (/= '#') $ T.drop (T.length prefix) after
          base = T.length before + T.length prefix
      in extensionRanges base body

    extensionRanges _ body | T.null body = []
    extensionRanges base body =
      let (extensionText, rest) = T.breakOn "," body
          extension = T.strip extensionText
          start = base + T.length body - T.length (extensionText <> rest) + leadingSpaces extensionText
          end = start + T.length extension
      in if T.null extension
           then extensionRanges (base + T.length extensionText + separatorLength rest) (T.drop (separatorLength rest) rest)
           else (start, end, extension) :
                if T.null rest
                  then []
                  else extensionRanges (base + T.length extensionText + separatorLength rest) (T.drop (separatorLength rest) rest)

    separatorLength rest = if T.null rest then 0 else 1
    leadingSpaces = T.length . T.takeWhile (== ' ')

atMay :: [a] -> Int -> Maybe a
atMay xs index
  | index < 0 = Nothing
  | otherwise = case drop index xs of
      value : _ -> Just value
      []        -> Nothing

-- ---------------------------------------------------------------------
-- | Title and pragma
type PragmaEdit = (T.Text, Pragma)

data Pragma = LangExt T.Text | OptGHC T.Text
  deriving (Show, Eq, Ord)

suggestPragmaProvider :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
suggestPragmaProvider = if ghcVersion /= GHC96 then
    mkCodeActionProvider suggestAddPragma
    else mkCodeActionProvider96 suggestAddPragma96

suggestDisableWarningProvider :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
suggestDisableWarningProvider = mkCodeActionProvider $ const suggestDisableWarning

mkCodeActionProvider :: (Maybe DynFlags -> FileDiagnostic -> [PragmaEdit]) -> PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
mkCodeActionProvider mkSuggest state _plId
  (LSP.CodeActionParams _ _ docId@LSP.TextDocumentIdentifier{ _uri = uri } caRange _) = do
    verTxtDocId <- liftIO $ runAction "classplugin.codeAction.getVersionedTextDoc" state $ getVersionedTextDoc docId
    normalizedFilePath <- getNormalizedFilePathE (verTxtDocId ^. L.uri)
    -- ghc session to get some dynflags even if module isn't parsed
    (hscEnv -> hsc_dflags -> sessionDynFlags, _) <-
      runActionE "Pragmas.GhcSession" state $ useWithStaleE GhcSession normalizedFilePath
    fileContents <- liftIO $ runAction "Pragmas.GetFileContents" state $ getFileContents normalizedFilePath
    parsedModule <- liftIO $ runAction "Pragmas.GetParsedModule" state $ getParsedModule normalizedFilePath
    let parsedModuleDynFlags = ms_hspp_opts . pm_mod_summary <$> parsedModule
        nextPragmaInfo = Pragmas.getNextPragmaInfo sessionDynFlags fileContents
    activeDiagnosticsInRange (shakeExtras state) normalizedFilePath caRange >>= \fileDiags -> do
            let actions = concatMap (mkSuggest parsedModuleDynFlags) fileDiags
            pure $ LSP.InL $ pragmaEditToAction uri nextPragmaInfo <$> nubOrdOn snd actions

mkCodeActionProvider96 :: (Maybe DynFlags -> Diagnostic -> [PragmaEdit]) -> PluginMethodHandler IdeState 'LSP.Method_TextDocumentCodeAction
mkCodeActionProvider96 mkSuggest state _plId
  (LSP.CodeActionParams _ _ LSP.TextDocumentIdentifier{ _uri = uri } _ (LSP.CodeActionContext diags _monly _)) = do
    normalizedFilePath <- getNormalizedFilePathE uri
    -- ghc session to get some dynflags even if module isn't parsed
    (hscEnv -> hsc_dflags -> sessionDynFlags, _) <-
      runActionE "Pragmas.GhcSession" state $ useWithStaleE GhcSession normalizedFilePath
    fileContents <- liftIO $ runAction "Pragmas.GetFileContents" state $ getFileContents normalizedFilePath
    parsedModule <- liftIO $ runAction "Pragmas.GetParsedModule" state $ getParsedModule normalizedFilePath
    let parsedModuleDynFlags = ms_hspp_opts . pm_mod_summary <$> parsedModule
        nextPragmaInfo = Pragmas.getNextPragmaInfo sessionDynFlags fileContents
        pedits = nubOrdOn snd $ concatMap (mkSuggest parsedModuleDynFlags) diags
    pure  $ LSP.InL $ pragmaEditToAction uri nextPragmaInfo <$> pedits


-- | Add a Pragma to the given URI at the top of the file.
-- It is assumed that the pragma name is a valid pragma,
-- thus, not validated.
pragmaEditToAction :: Uri -> Pragmas.NextPragmaInfo -> PragmaEdit -> (LSP.Command LSP.|? LSP.CodeAction)
pragmaEditToAction uri Pragmas.NextPragmaInfo{ nextPragmaLine, lineSplitTextEdits } (title, p) =
  LSP.InR $ LSP.CodeAction title (Just LSP.CodeActionKind_QuickFix) (Just []) Nothing Nothing (Just edit) Nothing Nothing
  where
    render (OptGHC x)  = "{-# OPTIONS_GHC -Wno-" <> x <> " #-}\n"
    render (LangExt x) = "{-# LANGUAGE " <> x <> " #-}\n"
    pragmaInsertPosition = Position (fromIntegral nextPragmaLine) 0
    pragmaInsertRange = Range pragmaInsertPosition pragmaInsertPosition
    -- workaround the fact that for some reason lsp-test applies text
    -- edits in reverse order than lsp (tried in both coc.nvim and vscode)
    textEdits =
      if | Just (Pragmas.LineSplitTextEdits insertTextEdit deleteTextEdit) <- lineSplitTextEdits
         , let LSP.TextEdit{ _range, _newText } = insertTextEdit ->
             [LSP.TextEdit _range (render p <> _newText), deleteTextEdit]
         | otherwise -> [LSP.TextEdit pragmaInsertRange (render p)]
    edit =
      LSP.WorkspaceEdit
        (Just $ M.singleton uri textEdits)
        Nothing
        Nothing

-- ---------------------------------------------------------------------

suggestDisableWarning :: FileDiagnostic -> [PragmaEdit]
suggestDisableWarning diagnostic
  | Just (Just (JSON.Array attachedReasons)) <- diagnostic ^? fdLspDiagnosticL . attachedReason
    =
    [ ("Disable \"" <> w <> "\" warnings", OptGHC w)
    | JSON.String attachedReason <- Foldable.toList attachedReasons
    , Just w <- [T.stripPrefix "-W" attachedReason]
    , w `notElem` warningBlacklist
    ]
  | otherwise = []

warningBlacklist :: [T.Text]
warningBlacklist =
  -- Don't suggest disabling type errors as a solution to all type errors.
  [ "deferred-type-errors"
  -- Don't suggest disabling out of scope errors as a solution to all out of scope errors.
  , "deferred-out-of-scope-variables"
  ]

-- ---------------------------------------------------------------------

-- | Offer to add a missing Language Pragma to the top of a file.
suggestAddPragma :: Maybe DynFlags -> FileDiagnostic -> [PragmaEdit]
suggestAddPragma mDynflags fd= [("Add \"" <> r <> "\"", LangExt r) | r <- map (T.pack . show) $ suggestsExtension fd, r `notElem` disabled]
  where
    disabled
      | Just dynFlags <- mDynflags =
        -- GHC does not export 'OnOff', so we have to view it as string
        mapMaybe (T.stripPrefix "Off " . printOutputable) (extensions dynFlags)
      | otherwise =
        -- When the module failed to parse, we don't have access to its
        -- dynFlags. In that case, simply don't disable any pragmas.
        []

-- | Offer to add a missing Language Pragma to the top of a file.
-- Pragmas are defined by a curated list of known pragmas, see 'possiblePragmas'.
-- Kept for compatibility with ghc9.6 - it is missing some structured diagnostics
suggestAddPragma96 :: Maybe DynFlags -> Diagnostic -> [PragmaEdit]
suggestAddPragma96 mDynflags Diagnostic {_message, _source}
    | _source == Just sourceTypecheck || _source == Just sourceParser = genPragma _message
  where
    genPragma target =
      [("Add \"" <> r <> "\"", LangExt r) | r <- findPragma target, r `notElem` disabled]
    disabled
      | Just dynFlags <- mDynflags =
        -- GHC does not export 'OnOff', so we have to view it as string
        mapMaybe (T.stripPrefix "Off " . printOutputable) (extensions dynFlags)
      | otherwise =
        -- When the module failed to parse, we don't have access to its
        -- dynFlags. In that case, simply don't disable any pragmas.
        []
suggestAddPragma96 _ _ = []

-- | Find all Pragmas are an infix of the search term.
findPragma :: T.Text -> [T.Text]
findPragma str = concatMap check possiblePragmas
  where
    check p = [p | T.isInfixOf p str]

    -- We exclude the Strict extension as it causes many false positives, see
    -- the discussion at https://github.com/haskell/ghcide/pull/638
    --
    -- We don't include the No- variants, as GHC never suggests disabling an
    -- extension in an error message.
    possiblePragmas :: [T.Text]
    possiblePragmas =
       [ name
       | FlagSpec{flagSpecName = T.pack -> name} <- xFlags
       , "Strict" /= name
       ]

suggestsExtension :: FileDiagnostic -> [Extension]
suggestsExtension message = case   message ^? fdStructuredMessageL . _SomeStructuredMessage . msgEnvelopeErrorL of
    Just s -> concat $ map (\case
      SuggestExtension s -> ghcHintSuggestsExtension s
      _ -> []) (diagnosticHints s)
    _          -> []

ghcHintSuggestsExtension :: LanguageExtensionHint -> [Extension]
ghcHintSuggestsExtension (SuggestSingleExtension _ ext)    = [ext]
ghcHintSuggestsExtension (SuggestAnyExtension _ (ext:_))   = [ext] -- ghc suggests any of those, we pick first
ghcHintSuggestsExtension (SuggestAnyExtension _ [])        = []
ghcHintSuggestsExtension (SuggestExtensions _ ext)         = ext
ghcHintSuggestsExtension (SuggestExtensionInOrderTo _ ext) = [ext]

-- | All language pragmas, including the No- variants
allPragmas :: [T.Text]
allPragmas =
  concat
    [ [name, "No" <> name]
    | FlagSpec{flagSpecName = T.pack -> name} <- xFlags
    ]
  <>
  -- These pragmas are not part of xFlags as they are not reversable
  -- by prepending "No".
  [ -- Safe Haskell
    "Unsafe"
  , "Trustworthy"
  , "Safe"

    -- Language Version Extensions
  , "Haskell98"
  , "Haskell2010"
  , "GHC2021"
  ]

-- ---------------------------------------------------------------------
flags :: [T.Text]
flags = map T.pack $ flagsForCompletion False

completion :: PluginMethodHandler IdeState 'LSP.Method_TextDocumentCompletion
completion ide _ complParams = do
    let (LSP.TextDocumentIdentifier uri) = complParams ^. L.textDocument
        position@(Position ln col) = complParams ^. L.position
    contents <- liftIO $ runAction "Pragmas.GetUriContents" ide $ getUriContents $ toNormalizedUri uri
    fmap LSP.InL $ case (contents, uriToFilePath' uri) of
        (Just cnts, Just _path) ->
            pure $ result $ getCompletionPrefixFromRope position cnts
            where
                result pfix
                    | "{-# language" `T.isPrefixOf` line
                    = map mkLanguagePragmaCompl $
                        Fuzzy.simpleFilter word allPragmas
                    | "{-# options_ghc" `T.isPrefixOf` line
                    = let optionPrefix = getGhcOptionPrefix pfix
                          prefixLength = fromIntegral $ T.length optionPrefix
                          prefixRange = LSP.Range (Position ln (col - prefixLength)) position
                      in map (mkGhcOptionCompl prefixRange) $ Fuzzy.simpleFilter optionPrefix flags
                    | "{-#" `T.isPrefixOf` line
                    = [ mkPragmaCompl (a <> suffix) b c
                      | (a, b, c, w) <- validPragmas, w == NewLine
                      ]
                    | -- Do not suggest any pragmas under any of these conditions:
                      -- 1. Current line is an import
                      -- 2. There is a module name right before the current word.
                      --    Something like `Text.la` shouldn't suggest adding the
                      --    'LANGUAGE' pragma.
                      -- 3. The user has not typed anything yet.
                      "import" `T.isPrefixOf` line || not (T.null module_) || T.null word
                    = []
                    | otherwise
                    = [ mkPragmaCompl (prefix <> pragmaTemplate <> suffix) matcher detail
                      | (pragmaTemplate, matcher, detail, appearWhere) <- validPragmas
                      , case appearWhere of
                            -- Only suggest a pragma that needs its own line if the whole line
                            -- fuzzily matches the pragma
                            NewLine   -> Fuzzy.test line matcher
                            -- Only suggest a pragma that appears in the middle of a line when
                            -- the current word is not the only thing in the line and the
                            -- current word fuzzily matches the pragma
                            CanInline -> line /= word && Fuzzy.test word matcher
                      ]
                    where
                        line = T.toLower $ fullLine pfix
                        module_ = prefixScope pfix
                        word = prefixText pfix
                        -- Not completely correct, may fail if more than one "{-#" exists.
                        -- We can ignore it since it rarely happens.
                        prefix
                            | "{-# "  `T.isInfixOf` line = ""
                            | "{-#"   `T.isInfixOf` line = " "
                            | otherwise                 = "{-# "
                        suffix
                            | " #-}" `T.isSuffixOf` line = ""
                            | "#-}"  `T.isSuffixOf` line = " "
                            | "-}"   `T.isSuffixOf` line = " #"
                            | "}"    `T.isSuffixOf` line = " #-"
                            | otherwise                 = " #-}"
        _ -> return []

-----------------------------------------------------------------------

-- | Pragma where exist
data AppearWhere =
  NewLine
  -- ^Must be on a new line
  | CanInline
  -- ^Can appear in the line
  deriving (Show, Eq)

validPragmas :: [(T.Text, T.Text, T.Text, AppearWhere)]
validPragmas =
  [ ("LANGUAGE ${1:extension}"        , "LANGUAGE"         , "{-# LANGUAGE #-}"         ,   NewLine)
  , ("OPTIONS_GHC -${1:option}"       , "OPTIONS_GHC"      , "{-# OPTIONS_GHC #-}"      ,   NewLine)
  , ("INLINE ${1:function}"           , "INLINE"           , "{-# INLINE #-}"           ,   NewLine)
  , ("NOINLINE ${1:function}"         , "NOINLINE"         , "{-# NOINLINE #-}"         ,   NewLine)
  , ("INLINABLE ${1:function}"        , "INLINABLE"        , "{-# INLINABLE #-}"        ,   NewLine)
  , ("WARNING ${1:message}"           , "WARNING"          , "{-# WARNING #-}"          , CanInline)
  , ("DEPRECATED ${1:message}"        , "DEPRECATED"       , "{-# DEPRECATED  #-}"      , CanInline)
  , ("ANN ${1:annotation}"            , "ANN"              , "{-# ANN #-}"              ,   NewLine)
  , ("RULES"                          , "RULES"            , "{-# RULES #-}"            ,   NewLine)
  , ("SPECIALIZE ${1:function}"       , "SPECIALIZE"       , "{-# SPECIALIZE #-}"       ,   NewLine)
  , ("SPECIALIZE INLINE ${1:function}", "SPECIALIZE INLINE", "{-# SPECIALIZE INLINE #-}",   NewLine)
  , ("SPECIALISE ${1:function}"       , "SPECIALISE"       , "{-# SPECIALISE #-}"       ,   NewLine)
  , ("SPECIALISE INLINE ${1:function}", "SPECIALISE INLINE", "{-# SPECIALISE INLINE #-}",   NewLine)
  , ("MINIMAL ${1:functions}"         , "MINIMAL"          , "{-# MINIMAL #-}"          , CanInline)
  , ("UNPACK"                         , "UNPACK"           , "{-# UNPACK #-}"           , CanInline)
  , ("NOUNPACK"                       , "NOUNPACK"         , "{-# NOUNPACK #-}"         , CanInline)
  , ("COMPLETE ${1:function}"         , "COMPLETE"         , "{-# COMPLETE #-}"         ,   NewLine)
  , ("OVERLAPPING"                    , "OVERLAPPING"      , "{-# OVERLAPPING #-}"      , CanInline)
  , ("OVERLAPPABLE"                   , "OVERLAPPABLE"     , "{-# OVERLAPPABLE #-}"     , CanInline)
  , ("OVERLAPS"                       , "OVERLAPS"         , "{-# OVERLAPS #-}"         , CanInline)
  , ("INCOHERENT"                     , "INCOHERENT"       , "{-# INCOHERENT #-}"       , CanInline)
  ]

mkPragmaCompl :: T.Text -> T.Text -> T.Text -> LSP.CompletionItem
mkPragmaCompl insertText label detail =
  LSP.CompletionItem label Nothing (Just LSP.CompletionItemKind_Keyword) Nothing (Just detail)
    Nothing Nothing Nothing Nothing Nothing (Just insertText) (Just LSP.InsertTextFormat_Snippet)
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing

mkLanguagePragmaCompl :: T.Text -> LSP.CompletionItem
mkLanguagePragmaCompl label =
  LSP.CompletionItem label Nothing (Just LSP.CompletionItemKind_Keyword) Nothing Nothing
    (Just $ LSP.InR $ pragmaDocumentation label) Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing

pragmaDocumentation :: T.Text -> LSP.MarkupContent
pragmaDocumentation label = LSP.MarkupContent LSP.MarkupKind_Markdown $ T.unlines $
  [ "**" <> label <> "**"
  , ""
  , "| Field | Value |"
  , "| --- | --- |"
  , "| Description | " <> description <> " |"
  ]
  <> [ "| Since | Since GHC " <> since <> " |" | not $ T.null since ]
  <> [ "| Status | " <> status <> " |" | not $ T.null status ]
  <> [ "| Implies | " <> T.intercalate ", " (map impliedLink implications) <> " |" | not isNegated && not (null implications) ]
  <> [ "| Guide | [Read the GHC User's Guide](" <> guideUrl url <> ") |" ]
  where
    extension = case T.stripPrefix "No" label of
      Just base | M.member base extensionDocs -> base
      _ -> label
    isNegated = extension /= label
    (url, baseDescription, since, included) = extensionDocumentation extension
    description = if isNegated then "Disable the `" <> extension <> "` language extension. " <> baseDescription else baseDescription
    status = M.findWithDefault (if T.null included then "" else "Included in " <> included) extension extensionStatuses
    implications = M.findWithDefault [] extension extensionImplications
    impliedLink implied = "[" <> implied <> "](" <> guideUrl impliedUrl <> ")"
      where (impliedUrl, _, _, _) = extensionDocumentation implied

guideUrl :: T.Text -> T.Text
guideUrl url = "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/" <> url

extensionDocumentation :: T.Text -> (T.Text, T.Text, T.Text, T.Text)
extensionDocumentation extension =
  M.findWithDefault ("table.html", "", "", "") extension extensionDocs

-- | Statuses not represented by the edition list in 'extensionDocs'.
extensionStatuses :: M.Map T.Text T.Text
extensionStatuses = M.fromList
  [ ("CUSKs", "Included in Haskell98, Haskell2010")
  , ("DatatypeContexts", "Deprecated, Included in Haskell98, Haskell2010")
  , ("DoAndIfThenElse", "Included in GHC2024, GHC2021, Haskell2010")
  , ("EmptyDataDecls", "Included in GHC2024, GHC2021 and Haskell2010")
  , ("EmptyDataDeriving", "Included in GHC2024, GHC2021, Haskell2010")
  , ("FieldSelectors", "Included in GHC2024, GHC2021, Haskell2010, Haskell98")
  , ("ForeignFunctionInterface", "Included in GHC2024, GHC2021, Haskell2010")
  , ("GHCForeignImportPrim", "InternalUseOnly")
  , ("IncoherentInstances", "Deprecated")
  , ("LinearTypes", "Experimental")
  , ("MonomorphismRestriction", "Enabled by default.")
  , ("NPlusKPatterns", "Included in Haskell98")
  , ("NondecreasingIndentation", "Included in Haskell98")
  , ("NullaryTypeClasses", "Deprecated")
  , ("OverlappingInstances", "Deprecated")
  , ("PatternGuards", "Disabled in Haskell98, enabled in Haskell2010 and later.")
  , ("Rank2Types", "Deprecated")
  , ("RequiredTypeArguments", "Experimental")
  , ("StarIsType", "Included in GHC2024, GHC2021, Haskell2010, Haskell98")
  , ("TraditionalRecordSyntax", "Enabled by default.")
  , ("TypeAbstractions", "Experimental")
  , ("TypeInType", "Deprecated")
  ]

-- | Direct implications listed on the corresponding GHC User's Guide pages.
extensionImplications :: M.Map T.Text [T.Text]
extensionImplications = M.fromList
  [ ("DeriveTraversable", ["DeriveFoldable", "DeriveFunctor"])
  , ("DerivingVia", ["DerivingStrategies"])
  , ("DuplicateRecordFields", ["DisambiguateRecordFields"])
  , ("ExistentialQuantification", ["ExplicitForAll"])
  , ("ExplicitLevelImports", ["ImplicitStagePersistence"])
  , ("FlexibleInstances", ["TypeSynonymInstances"])
  , ("FunctionalDependencies", ["MultiParamTypeClasses"])
  , ("GADTs", ["MonoLocalBinds", "GADTSyntax"])
  , ("ImpredicativeTypes", ["RankNTypes"])
  , ("IncoherentInstances", ["OverlappingInstances"])
  , ("LiberalTypeSynonyms", ["ExplicitForAll"])
  , ("LinearTypes", ["MonoLocalBinds"])
  , ("MonadComprehensions", ["ParallelListComp"])
  , ("MultiParamTypeClasses", ["ConstrainedClassMethods"])
  , ("PolyKinds", ["KindSignatures"])
  , ("QuantifiedConstraints", ["ExplicitForAll"])
  , ("RankNTypes", ["ExplicitForAll"])
  , ("RebindableSyntax", ["ImplicitPrelude"])
  , ("RecordWildCards", ["DisambiguateRecordFields"])
  , ("ScopedTypeVariables", ["ExplicitForAll"])
  , ("StandaloneKindSignatures", ["CUSKs"])
  , ("Strict", ["StrictData"])
  , ("TemplateHaskell", ["TemplateHaskellQuotes"])
  , ("TypeFamilies", ["MonoLocalBinds", "KindSignatures", "ExplicitNamespaces"])
  , ("TypeFamilyDependencies", ["TypeFamilies"])
  , ("TypeInType", ["PolyKinds", "DataKinds", "KindSignatures"])
  , ("TypeOperators", ["ExplicitNamespaces"])
  , ("UnboxedTuples", ["UnboxedSums"])
  , ("UnliftedDatatypes", ["DataKinds", "StandaloneKindSignatures"])
  ]

-- | Metadata mirrored from the GHC User's Guide extension table.
extensionDocs :: M.Map T.Text (T.Text, T.Text, T.Text, T.Text)
extensionDocs = M.fromList
  [ ("AllowAmbiguousTypes", ("ambiguous_types.html#extension-AllowAmbiguousTypes", "Allow the user to write ambiguous types, and the type inference engine to infer them.", "7.8.1", ""))
  , ("ApplicativeDo", ("applicative_do.html#extension-ApplicativeDo", "Allow do-notation statements to be desugared via `Applicative`.", "8.0.1", ""))
  , ("Arrows", ("arrows.html#extension-Arrows", "Allow arrow notation (e.g. `proc`)", "6.8.1", ""))
  , ("BangPatterns", ("strict.html#extension-BangPatterns", "Allow bang pattern syntax.", "6.8.1", "GHC2024, GHC2021"))
  , ("BinaryLiterals", ("binary_literals.html#extension-BinaryLiterals", "Allow binary literal syntax.", "7.10.1", "GHC2024, GHC2021"))
  , ("BlockArguments", ("block_arguments.html#extension-BlockArguments", "Allow `do` blocks and other constructs as function arguments.", "8.6.1", ""))
  , ("CApiFFI", ("ffi.html#extension-CApiFFI", "Allow `foreign import`s to be declared with the `capi` calling convention.", "7.6.1", ""))
  , ("ConstrainedClassMethods", ("constrained_class_methods.html#extension-ConstrainedClassMethods", "Allow class methods to have non-empty contexts.", "6.8.1", "GHC2024, GHC2021"))
  , ("ConstraintKinds", ("constraint_kind.html#extension-ConstraintKinds", "Allow constraints to be used as types of kind `Constraint`.", "7.4.1", "GHC2024, GHC2021"))
  , ("CPP", ("../phases.html#extension-CPP", "Resolve C preprocessor directives.", "6.8.1", ""))
  , ("CUSKs", ("poly_kinds.html#extension-CUSKs", "Detect complete user-supplied kind signatures.", "8.10.1", ""))
  , ("DataKinds", ("data_kinds.html#extension-DataKinds", "Allow use of data constructors in types.", "7.4.1", "GHC2024"))
  , ("DatatypeContexts", ("datatype_contexts.html#extension-DatatypeContexts", "Allow contexts on `data` types.", "7.0.1", ""))
  , ("DeepSubsumption", ("rank_polymorphism.html#extension-DeepSubsumption", "Use GHC's deep subsumption checking.", "9.2.4", ""))
  , ("DefaultSignatures", ("default_signatures.html#extension-DefaultSignatures", "Allow default signatures for typeclass methods.", "7.2.1", ""))
  , ("DeriveAnyClass", ("derive_any_class.html#extension-DeriveAnyClass", "Allow `deriving` syntax to be used for any class.", "7.10.1", ""))
  , ("DeriveDataTypeable", ("deriving_extra.html#extension-DeriveDataTypeable", "Allow deriving for the `Data` class.", "6.8.1", "GHC2024, GHC2021"))
  , ("DeriveFoldable", ("deriving_extra.html#extension-DeriveFoldable", "Allow deriving for the `Foldable` class.", "7.10.1", "GHC2024, GHC2021"))
  , ("DeriveFunctor", ("deriving_extra.html#extension-DeriveFunctor", "Allow deriving for the `Functor` class.", "7.10.1", "GHC2024, GHC2021"))
  , ("DeriveGeneric", ("generics.html#extension-DeriveGeneric", "Allow deriving of `Generic` instances.", "7.2.1", "GHC2024, GHC2021"))
  , ("DeriveLift", ("deriving_extra.html#extension-DeriveLift", "Allow deriving for the `Lift` class", "8.0.1", "GHC2024, GHC2021"))
  , ("DeriveTraversable", ("deriving_extra.html#extension-DeriveTraversable", "Allow deriving for the `Traversable` class.", "7.10.1", ""))
  , ("DerivingStrategies", ("deriving_strategies.html#extension-DerivingStrategies", "Allow use of instance deriving strategies.", "8.2.1", "GHC2024"))
  , ("DerivingVia", ("deriving_via.html#extension-DerivingVia", "Allow deriving instances `via` types of the same runtime representation.", "8.6.1", ""))
  , ("DisambiguateRecordFields", ("disambiguate_record_fields.html#extension-DisambiguateRecordFields", "Automatically disambiguate some record field references.", "6.8.1", "GHC2024"))
  , ("DoAndIfThenElse", ("doandifthenelse.html#extension-DoAndIfThenElse", "Allow semicolons in `if` expressions.", "7.0.1", "GHC2024, GHC2021"))
  , ("DuplicateRecordFields", ("duplicate_record_fields.html#extension-DuplicateRecordFields", "Allow definition of record types with identically-named fields.", "8.0.1", ""))
  , ("EmptyCase", ("empty_case.html#extension-EmptyCase", "Allow `case` expressions with no alternatives.", "7.8.1", "GHC2024, GHC2021"))
  , ("EmptyDataDecls", ("nullary_types.html#extension-EmptyDataDecls", "Allow definition of empty `data` types.", "6.8.1", "GHC2024, GHC2021"))
  , ("EmptyDataDeriving", ("empty_data_deriving.html#extension-EmptyDataDeriving", "Allow deriving instances of standard type classes for empty data types.", "8.4.1", "GHC2024, GHC2021"))
  , ("ExistentialQuantification", ("existential_quantification.html#extension-ExistentialQuantification", "Allow existentially quantified type variables in types.", "6.8.1", "GHC2024, GHC2021"))
  , ("ExplicitForAll", ("explicit_forall.html#extension-ExplicitForAll", "Allow explicit universal quantification.", "6.12.1", "GHC2024, GHC2021"))
  , ("ExplicitLevelImports", ("template_haskell.html#extension-ExplicitLevelImports", "Allow explicit level imports in Template Haskell.", "9.14.1", ""))
  , ("ExplicitNamespaces", ("explicit_namespaces.html#extension-ExplicitNamespaces", "Allow use of the `type` and `data` keywords to specify the namespace of entries in import/export lists and in other contexts.", "7.6.1", "GHC2024"))
  , ("ExtendedDefaultRules", ("../ghci.html#extension-ExtendedDefaultRules", "Use GHCi's extended default rules in a normal module.", "6.8.1", ""))
  , ("ExtendedLiterals", ("extended_literals.html#extension-ExtendedLiterals", "Allow numeric literal postfix syntax for unboxed integers.", "9.8.1", ""))
  , ("FieldSelectors", ("field_selectors.html#extension-FieldSelectors", "Make record field selector functions visible in expressions.", "9.2.1", "GHC2024, GHC2021"))
  , ("FlexibleContexts", ("flexible_contexts.html#extension-FlexibleContexts", "Remove some restrictions on class contexts", "6.8.1", "GHC2024, GHC2021"))
  , ("FlexibleInstances", ("instances.html#extension-FlexibleInstances", "Allow instance heads to mention arbitrary nested types.", "6.8.1", "GHC2024, GHC2021"))
  , ("ForeignFunctionInterface", ("ffi.html#extension-ForeignFunctionInterface", "Allow foreign function interface syntax.", "6.8.1", "GHC2024, GHC2021"))
  , ("FunctionalDependencies", ("functional_dependencies.html#extension-FunctionalDependencies", "Allow functional dependencies to be given on typeclass declarations.", "6.8.1", ""))
  , ("GADTs", ("gadt.html#extension-GADTs", "Allow definition of generalised algebraic data types.", "6.8.1", "GHC2024"))
  , ("GADTSyntax", ("gadt_syntax.html#extension-GADTSyntax", "Allow generalised algebraic data type syntax.", "7.2.1", "GHC2024, GHC2021"))
  , ("GeneralisedNewtypeDeriving", ("newtype_deriving.html#extension-GeneralisedNewtypeDeriving", "Allow instances to be derived via `newtype` deriving.", "6.8.1. British spelling since 8.6.1.", "GHC2024, GHC2021"))
  , ("GHC2021", ("control.html#extension-GHC2021", "Use GHC’s set of default language extensions from 2021", "9.2.1", ""))
  , ("GHC2024", ("control.html#extension-GHC2024", "Use GHC’s set of default language extensions from 2024", "9.10.1", ""))
  , ("GHCForeignImportPrim", ("ffi.html#extension-GHCForeignImportPrim", "Allow `prim` calling convention. Intended for internal use only.", "6.12.1", ""))
  , ("Haskell2010", ("control.html#extension-Haskell2010", "Use the Haskell 2010 language edition.", "", ""))
  , ("Haskell98", ("control.html#extension-Haskell98", "Use the Haskell 98 language edition.", "", ""))
  , ("HexFloatLiterals", ("hex_float_literals.html#extension-HexFloatLiterals", "Allow hexadecimal floating-point literal syntax.", "8.4.1", "GHC2024, GHC2021"))
  , ("ImplicitParams", ("implicit_parameters.html#extension-ImplicitParams", "Allow implicit parameter constraints.", "6.8.1", ""))
  , ("ImplicitPrelude", ("rebindable_syntax.html#extension-ImplicitPrelude", "Implicitly import `Prelude`.", "6.8.1", ""))
  , ("ImplicitStagePersistence", ("template_haskell.html#extension-ImplicitStagePersistence", "Allow identifiers to be used at different levels from where they are defined.", "9.14.1", ""))
  , ("ImportQualifiedPost", ("import_qualified_post.html#extension-ImportQualifiedPost", "Allows the syntax `import M qualified`", "8.10.1", "GHC2024, GHC2021"))
  , ("ImpredicativeTypes", ("impredicative_types.html#extension-ImpredicativeTypes", "Allow impredicative types.", "9.2.1 (unreliable in 6.10 - 9.0)", ""))
  , ("IncoherentInstances", ("instances.html#extension-IncoherentInstances", "Allow definitions of instances that may result in incoherence.", "6.8.1", ""))
  , ("InstanceSigs", ("instances.html#extension-InstanceSigs", "Allow type signatures to be written for instance methods.", "7.6.1", "GHC2024, GHC2021"))
  , ("InterruptibleFFI", ("ffi.html#extension-InterruptibleFFI", "Allow `interruptible` FFI imports.", "7.2.1", ""))
  , ("KindSignatures", ("kind_signatures.html#extension-KindSignatures", "Allow kind signatures to be given for types.", "6.8.1", "GHC2024, GHC2021"))
  , ("LambdaCase", ("lambda_case.html#extension-LambdaCase", "Allow `\\case` expressions.", "7.6.1", "GHC2024"))
  , ("LexicalNegation", ("lexical_negation.html#extension-LexicalNegation", "Use whitespace to determine whether the minus sign stands for negation or subtraction.", "9.0.1", ""))
  , ("LiberalTypeSynonyms", ("liberal_type_synonyms.html#extension-LiberalTypeSynonyms", "Relax many of Haskell 98's rules on type synonym definitions.", "6.8.1", ""))
  , ("LinearTypes", ("linear_types.html#extension-LinearTypes", "Allow writing of linear arrow types.", "9.0.1", ""))
  , ("ListTuplePuns", ("data_kinds.html#extension-ListTuplePuns", "Enable punning for list, tuple and sum types.", "9.10.1", ""))
  , ("MagicHash", ("magic_hash.html#extension-MagicHash", "Allow `#` as a postfix modifier on identifiers.", "6.8.1", ""))
  , ("MonadComprehensions", ("monad_comprehensions.html#extension-MonadComprehensions", "Allow list comprehension syntax to be used at monads other than `List`.", "7.2.1", ""))
  , ("MonoLocalBinds", ("let_generalisation.html#extension-MonoLocalBinds", "Do not generalise types of local bindings.", "6.12.1", "GHC2024"))
  , ("MonomorphismRestriction", ("monomorphism.html#extension-MonomorphismRestriction", "Apply the Haskell 2010 monomorphism restriction.", "6.8.1", ""))
  , ("MultilineStrings", ("multiline_strings.html#extension-MultilineStrings", "Enable multiline string literals.", "9.12.1", ""))
  , ("MultiParamTypeClasses", ("multi_param_type_classes.html#extension-MultiParamTypeClasses", "Enable multi-parameter type classes.", "6.8.1", "GHC2024, GHC2021"))
  , ("MultiWayIf", ("multiway_if.html#extension-MultiWayIf", "Allow multi-way `if`-expressions.", "7.6.1", ""))
  , ("NamedDefaults", ("named_defaults.html#extension-NamedDefaults", "Enable `default` declarations with explicitly named class, extending Type class defaulting.", "9.12.1", ""))
  , ("NamedFieldPuns", ("record_puns.html#extension-NamedFieldPuns", "Allow record field punning syntax.", "6.10.1", "GHC2024, GHC2021"))
  , ("NamedWildCards", ("partial_type_signatures.html#extension-NamedWildCards", "Allow named wildcards in types.", "7.10.1", "GHC2024, GHC2021"))
  , ("NegativeLiterals", ("negative_literals.html#extension-NegativeLiterals", "Allow negative numeric literal syntax.", "7.8.1", ""))
  , ("NondecreasingIndentation", ("../bugs.html#extension-NondecreasingIndentation", "Allow nested contexts to be at the same indentation level as its enclosing context.", "7.2.1", ""))
  , ("NPlusKPatterns", ("nk_patterns.html#extension-NPlusKPatterns", "Allow use of `n+k` patterns.", "6.12.1", ""))
  , ("NullaryTypeClasses", ("nullary_type_classes.html#extension-NullaryTypeClasses", "Deprecated, does nothing. nullary type classes are now enabled using `MultiParamTypeClasses`.", "7.8.1", ""))
  , ("NumDecimals", ("num_decimals.html#extension-NumDecimals", "Allow use of scientific notation syntax for integer literals.", "7.8.1", ""))
  , ("NumericUnderscores", ("numeric_underscores.html#extension-NumericUnderscores", "Allow underscores in numeric literals.", "8.6.1", "GHC2024, GHC2021"))
  , ("OrPatterns", ("or_patterns.html#extension-OrPatterns", "Enable or-patterns.", "9.12.1", ""))
  , ("OverlappingInstances", ("instances.html#extension-OverlappingInstances", "Allow definition of overlapping instances.", "6.8.1", ""))
  , ("OverloadedLabels", ("overloaded_labels.html#extension-OverloadedLabels", "Allow overloaded label syntax.", "8.0.1", ""))
  , ("OverloadedLists", ("overloaded_lists.html#extension-OverloadedLists", "Desugar list syntax via the `IsList` class.", "7.8.1", ""))
  , ("OverloadedRecordDot", ("overloaded_record_dot.html#extension-OverloadedRecordDot", "Allow `.` to be used for record field access.", "9.2.0", ""))
  , ("OverloadedRecordUpdate", ("overloaded_record_update.html#extension-OverloadedRecordUpdate", "Allow `.` syntax in record updates", "9.2.0", ""))
  , ("OverloadedStrings", ("overloaded_strings.html#extension-OverloadedStrings", "Desugar string literals via `IsString` class.", "6.8.1", ""))
  , ("PackageImports", ("package_qualified_imports.html#extension-PackageImports", "Allow package-qualified `import` syntax.", "6.10.1", ""))
  , ("ParallelListComp", ("parallel_list_comprehensions.html#extension-ParallelListComp", "Allow parallel list comprehension syntax.", "6.8.1", ""))
  , ("PartialTypeSignatures", ("partial_type_signatures.html#extension-PartialTypeSignatures", "Allow type signatures to contain wildcards.", "7.10.1", ""))
  , ("PatternGuards", ("pattern_guards.html#extension-PatternGuards", "Allow pattern guards syntax.", "6.8.1", ""))
  , ("PatternSynonyms", ("pattern_synonyms.html#extension-PatternSynonyms", "Allow definition of pattern synonyms.", "7.8.1", ""))
  , ("PolyKinds", ("poly_kinds.html#extension-PolyKinds", "Allow kind polymorphism.", "7.4.1", "GHC2024, GHC2021"))
  , ("PostfixOperators", ("rebindable_syntax.html#extension-PostfixOperators", "Allow the use of postfix operators.", "7.10.1", "GHC2024, GHC2021"))
  , ("QualifiedDo", ("qualified_do.html#extension-QualifiedDo", "Allow qualified `do`-notation desugaring.", "9.0.1", ""))
  , ("QualifiedStrings", ("qualified_strings.html#extension-QualifiedStrings", "Enable qualified string literals.", "9.16.1", ""))
  , ("QuantifiedConstraints", ("quantified_constraints.html#extension-QuantifiedConstraints", "Allow `forall` quantifiers in constraints.", "8.6.1", ""))
  , ("QuasiQuotes", ("template_haskell.html#extension-QuasiQuotes", "Allow quasiquotation syntax.", "6.10.1", ""))
  , ("Rank2Types", ("rank_polymorphism.html#extension-Rank2Types", "Enable rank-2 types.", "6.8.1", ""))
  , ("RankNTypes", ("rank_polymorphism.html#extension-RankNTypes", "Allow types of rank greater than one.", "6.8.1", "GHC2024, GHC2021"))
  , ("RebindableSyntax", ("rebindable_syntax.html#extension-RebindableSyntax", "Allow rebinding of builtin syntax.", "7.0.1", ""))
  , ("RecordWildCards", ("record_wildcards.html#extension-RecordWildCards", "Allow use of record wildcard syntax.", "6.8.1", ""))
  , ("RecursiveDo", ("recursive_do.html#extension-RecursiveDo", "Allow recursive do (e.g. `mdo`) notation.", "6.8.1", ""))
  , ("RelaxedPolyRec", ("relaxed_poly_rec.html#extension-RelaxedPolyRec", "Generalised typing of mutually recursive bindings.", "6.8.1", "GHC2024, GHC2021"))
  , ("RequiredTypeArguments", ("required_type_arguments.html#extension-RequiredTypeArguments", "Allow use of required type argument syntax in terms.", "9.10.1", ""))
  , ("RoleAnnotations", ("roles.html#extension-RoleAnnotations", "Allow role annotation syntax.", "7.8.1", "GHC2024"))
  , ("Safe", ("safe_haskell.html#extension-Safe", "Enable the Safe Haskell Safe mode.", "7.2.1", ""))
  , ("ScopedTypeVariables", ("scoped_type_variables.html#extension-ScopedTypeVariables", "Lexically scoped explicitly-introduced type variables.", "6.8.1", "GHC2024, GHC2021"))
  , ("StandaloneDeriving", ("standalone_deriving.html#extension-StandaloneDeriving", "Allow standalone instance deriving declarations.", "6.8.1", "GHC2024, GHC2021"))
  , ("StandaloneKindSignatures", ("poly_kinds.html#extension-StandaloneKindSignatures", "Allow standalone kind signature declarations.", "8.10.1", "GHC2024, GHC2021"))
  , ("StarIsType", ("poly_kinds.html#extension-StarIsType", "Treat `*` as `Data.Kind.Type`.", "8.6.1", "GHC2024, GHC2021"))
  , ("StaticPointers", ("static_pointers.html#extension-StaticPointers", "Allow `static` syntax.", "7.10.1", ""))
  , ("Strict", ("strict.html#extension-Strict", "Make bindings in the current module strict by default.", "8.0.1", ""))
  , ("StrictData", ("strict.html#extension-StrictData", "Treat datatype fields as strict by default.", "8.0.1", ""))
  , ("TemplateHaskell", ("template_haskell.html#extension-TemplateHaskell", "Allow Template Haskell's splice and quotation syntax.", "6.0. Typed splices introduced in GHC 7.8.1.", ""))
  , ("TemplateHaskellQuotes", ("template_haskell.html#extension-TemplateHaskellQuotes", "Allow Template Haskell's quotation syntax.", "8.0.1", ""))
  , ("TraditionalRecordSyntax", ("traditional_record_syntax.html#extension-TraditionalRecordSyntax", "Allow traditional record syntax (e.g. `C {f = x}`).", "7.4.1", ""))
  , ("TransformListComp", ("generalised_list_comprehensions.html#extension-TransformListComp", "Allow generalised list comprehension syntax.", "6.10.1", ""))
  , ("Trustworthy", ("safe_haskell.html#extension-Trustworthy", "Enable the Safe Haskell Trustworthy mode.", "7.2.1", ""))
  , ("TupleSections", ("tuple_sections.html#extension-TupleSections", "Allow use of tuple section synxtax.", "6.12", "GHC2024, GHC2021"))
  , ("TypeAbstractions", ("type_abstractions.html#extension-TypeAbstractions", "Allow type abstraction syntax in patterns and type variable binders.", "9.8.1", ""))
  , ("TypeApplications", ("type_applications.html#extension-TypeApplications", "Allow type application syntax in terms and types.", "8.0.1", "GHC2024, GHC2021"))
  , ("TypeData", ("type_data.html#extension-TypeData", "Allow `type data` declarations.", "9.6.1", ""))
  , ("TypeFamilies", ("type_families.html#extension-TypeFamilies", "Allow definition of type families.", "6.8.1", ""))
  , ("TypeFamilyDependencies", ("type_families.html#extension-TypeFamilyDependencies", "Allow injectivity annotations on type families.", "8.0.1", ""))
  , ("TypeInType", ("poly_kinds.html#extension-TypeInType", "Deprecated. Enable kind polymorphism and datatype promotion.", "8.0.1", ""))
  , ("TypeOperators", ("type_operators.html#extension-TypeOperators", "Allow type constructors to be given operator names.", "6.8.1", "GHC2024, GHC2021"))
  , ("TypeSynonymInstances", ("instances.html#extension-TypeSynonymInstances", "Allow type synonyms to be mentioned in instance heads.", "6.8.1", "GHC2024, GHC2021"))
  , ("UnboxedSums", ("primitives.html#extension-UnboxedSums", "Allow the use of unboxed sum syntax.", "8.2.1", ""))
  , ("UnboxedTuples", ("primitives.html#extension-UnboxedTuples", "Allow the use of unboxed tuple syntax.", "6.8.1", ""))
  , ("UndecidableInstances", ("instances.html#extension-UndecidableInstances", "Allow definition of instances which may make solving undecidable.", "6.8.1", ""))
  , ("UndecidableSuperClasses", ("undecidable_super_classes.html#extension-UndecidableSuperClasses", "Allow all superclass constraints, including those that may result in non-termination of the typechecker.", "8.0.1", ""))
  , ("UnicodeSyntax", ("unicode_syntax.html#extension-UnicodeSyntax", "Enable unicode syntax.", "6.8.1", ""))
  , ("UnliftedDatatypes", ("primitives.html#extension-UnliftedDatatypes", "Allow the definition of unlifted data types.", "9.2.1", ""))
  , ("UnliftedFFITypes", ("ffi.html#extension-UnliftedFFITypes", "Allow the types of foreign imports to contain certain unlifted types.", "6.8.1", ""))
  , ("UnliftedNewtypes", ("primitives.html#extension-UnliftedNewtypes", "Allow definition of unlifted newtypes.", "8.10.1", ""))
  , ("Unsafe", ("safe_haskell.html#extension-Unsafe", "Enable Safe Haskell Unsafe mode.", "7.4.1", ""))
  , ("ViewPatterns", ("view_patterns.html#extension-ViewPatterns", "Allow view pattern syntax.", "6.10.1", ""))]

mkGhcOptionCompl :: Range -> T.Text -> LSP.CompletionItem
mkGhcOptionCompl editRange completedFlag =
  LSP.CompletionItem completedFlag Nothing (Just LSP.CompletionItemKind_Keyword) Nothing Nothing
    Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    Nothing (Just insertCompleteFlag) Nothing Nothing Nothing Nothing Nothing
  where
    insertCompleteFlag = LSP.InL $ LSP.TextEdit editRange completedFlag

-- The prefix extraction logic of getCompletionPrefix
-- doesn't consider '-' part of prefix which breaks completion
-- of flags like "-ddump-xyz". For OPTIONS_GHC completion we need the whole thing
-- to be considered completion prefix, but `prefixText posPrefixInfo` would return"xyz" in this case
getGhcOptionPrefix :: PosPrefixInfo -> T.Text
getGhcOptionPrefix PosPrefixInfo {cursorPos = Position _ col, fullLine}=
  T.takeWhileEnd isGhcOptionChar beforePos
  where
    beforePos = T.take (fromIntegral col) fullLine

    -- Is this character contained in some GHC flag? Based on:
    -- >>> nub . sort . concat $ GHC.Driver.Session.flagsForCompletion False
    -- "#-.01234589=ABCDEFGHIJKLMNOPQRSTUVWX_abcdefghijklmnopqrstuvwxyz"
    isGhcOptionChar :: Char -> Bool
    isGhcOptionChar c = isAlphaNum c || c `elem` ("#-.=_" :: String)
