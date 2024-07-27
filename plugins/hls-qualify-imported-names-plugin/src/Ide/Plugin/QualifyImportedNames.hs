{-# LANGUAGE CPP               #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.QualifyImportedNames (descriptor) where

import           Control.Lens                     ((^.))
import           Control.Monad                    (foldM)
import           Control.Monad.Trans.State.Strict (State)
import qualified Control.Monad.Trans.State.Strict as State
import           Data.DList                       (DList)
import qualified Data.DList                       as DList
import           Data.Foldable                    (find)
import           Data.List                        (sortOn)
import qualified Data.List                        as List
import qualified Data.Map.Strict                  as Map
import           Data.Maybe                       (fromMaybe, isJust, mapMaybe)
import qualified Data.Set                         as Set
import           Data.Text                        (Text)
import qualified Data.Text                        as Text
import           Development.IDE                  (spanContainsRange)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.RuleTypes   (GetFileContents (GetFileContents),
                                                   GetHieAst (GetHieAst),
                                                   HieAstResult (HAR, refMap),
                                                   TcModuleResult (TcModuleResult, tmrParsed, tmrTypechecked),
                                                   TypeCheck (TypeCheck))
import           Development.IDE.Core.Shake       (IdeState)
import           Development.IDE.GHC.Compat       (ContextInfo (Use),
                                                   GenLocated (..), GhcPs,
                                                   GlobalRdrElt, GlobalRdrEnv,
                                                   HsModule (hsmodImports),
                                                   Identifier,
                                                   IdentifierDetails (IdentifierDetails, identInfo),
                                                   ImpDeclSpec (ImpDeclSpec, is_as, is_dloc, is_qual),
                                                   ImportSpec (ImpSpec),
                                                   LImportDecl, ModuleName,
                                                   Name, NameEnv, ParsedModule,
                                                   RefMap, Span, SrcSpan,
                                                   TcGblEnv (tcg_rdr_env),
                                                   emptyUFM, globalRdrEnvElts,
                                                   gre_imp, gre_name, locA,
                                                   lookupNameEnv,
                                                   moduleNameString,
                                                   nameOccName, occNameString,
                                                   pattern GRE,
                                                   pattern ParsedModule,
                                                   plusUFM_C, pm_parsed_source,
                                                   srcSpanEndCol,
                                                   srcSpanEndLine,
                                                   srcSpanStartCol,
                                                   srcSpanStartLine, unitUFM)
import           Development.IDE.Types.Location   (Position (Position),
                                                   Range (Range), Uri)
import           Ide.Plugin.Error                 (PluginError (PluginRuleFailed),
                                                   getNormalizedFilePathE,
                                                   handleMaybe)
import           Ide.Types                        (PluginDescriptor (pluginHandlers),
                                                   PluginId,
                                                   PluginMethodHandler,
                                                   defaultPluginDescriptor,
                                                   mkPluginHandler)
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message    (Method (Method_TextDocumentCodeAction),
                                                   SMethod (SMethod_TextDocumentCodeAction))
import           Language.LSP.Protocol.Types      (CodeAction (CodeAction, _command, _data_, _diagnostics, _disabled, _edit, _isPreferred, _kind, _title),
                                                   CodeActionKind (CodeActionKind_QuickFix),
                                                   CodeActionParams (CodeActionParams),
                                                   TextEdit (TextEdit),
                                                   WorkspaceEdit (WorkspaceEdit, _changeAnnotations, _changes, _documentChanges),
                                                   type (|?) (InL, InR))

#if !MIN_VERSION_base(4,20,0)
import           Data.Foldable                    (foldl')
#endif

thenCmp :: Ordering -> Ordering -> Ordering
{-# INLINE thenCmp #-}
thenCmp EQ       ordering = ordering
thenCmp ordering _        = ordering

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId "Provides a code action to qualify imported names") {
  pluginHandlers = mconcat
    [ mkPluginHandler SMethod_TextDocumentCodeAction codeActionProvider
    ]
}

findLImportDeclAt :: Range -> ParsedModule -> Maybe (LImportDecl GhcPs)
findLImportDeclAt range parsedModule
  | ParsedModule {..} <- parsedModule
  , L _ hsModule <- pm_parsed_source
  , locatedImportDecls <- hsmodImports hsModule =
      find (\ (L (locA -> srcSpan) _) -> fromMaybe False $ srcSpan `spanContainsRange` range) locatedImportDecls

makeCodeActions :: Uri -> [TextEdit] -> [a |? CodeAction]
makeCodeActions uri textEdits = [InR CodeAction {..} | not (null textEdits)]
  where _title = "Qualify imported names"
        _kind = Just CodeActionKind_QuickFix
        _command = Nothing
        _edit = Just WorkspaceEdit {..}
        _changes = Just $ Map.singleton uri textEdits
        _documentChanges = Nothing
        _diagnostics = Nothing
        _isPreferred = Nothing
        _disabled = Nothing
        _data_ = Nothing
        _changeAnnotations = Nothing

data ImportedBy = ImportedBy {
  importedByAlias   :: !ModuleName,
  importedBySrcSpan :: !SrcSpan
}

isRangeWithinImportedBy :: Range -> ImportedBy -> Bool
isRangeWithinImportedBy range ImportedBy{importedBySrcSpan} = fromMaybe False $ spanContainsRange importedBySrcSpan range

globalRdrEnvToNameToImportedByMap :: GlobalRdrEnv -> NameEnv [ImportedBy]
globalRdrEnvToNameToImportedByMap =
  fmap DList.toList . foldl' (plusUFM_C (<>)) emptyUFM . map globalRdrEltToNameToImportedByMap . globalRdrEnvElts
  where
    globalRdrEltToNameToImportedByMap :: GlobalRdrElt -> NameEnv (DList ImportedBy)
    globalRdrEltToNameToImportedByMap GRE {..} =
      unitUFM gre_name $ DList.fromList $ mapMaybe importSpecToImportedBy gre_imp

    importSpecToImportedBy :: ImportSpec -> Maybe ImportedBy
    importSpecToImportedBy (ImpSpec ImpDeclSpec {..} _)
      | is_qual = Nothing
      | otherwise = Just (ImportedBy is_as is_dloc)

data IdentifierSpan = IdentifierSpan {
  identifierSpanLine     :: !Int,
  identifierSpanStartCol :: !Int,
  identifierSpanEndCol   :: !Int
} deriving (Show, Eq)

instance Ord IdentifierSpan where
  compare (IdentifierSpan line1 startCol1 endCol1) (IdentifierSpan line2 startCol2 endCol2) =
    (line1 `compare` line2) `thenCmp` (startCol1 `compare` startCol2) `thenCmp` (endCol1 `compare` endCol2)

realSrcSpanToIdentifierSpan :: Span -> Maybe IdentifierSpan
realSrcSpanToIdentifierSpan realSrcSpan
  | let startLine = srcSpanStartLine realSrcSpan - 1
  , let endLine = srcSpanEndLine realSrcSpan - 1
  , startLine == endLine
  , let startCol = srcSpanStartCol realSrcSpan - 1
  , let endCol = srcSpanEndCol realSrcSpan - 1 =
      Just $ IdentifierSpan startLine startCol endCol
  | otherwise = Nothing

identifierSpanToRange :: IdentifierSpan -> Range
identifierSpanToRange (IdentifierSpan line startCol endCol) =
  Range (Position (fromIntegral line) (fromIntegral startCol)) (Position (fromIntegral line) (fromIntegral endCol))

data UsedIdentifier = UsedIdentifier {
  usedIdentifierName :: !Name,
  usedIdentifierSpan :: !IdentifierSpan
}

refMapToUsedIdentifiers :: RefMap a -> [UsedIdentifier]
refMapToUsedIdentifiers = DList.toList . Map.foldlWithKey' folder DList.empty
  where
    folder acc identifier spanIdentifierDetailsPairs =
      DList.fromList (mapMaybe (uncurry (getUsedIdentifier identifier)) spanIdentifierDetailsPairs) <> acc

    getUsedIdentifier :: Identifier -> Span -> IdentifierDetails a -> Maybe UsedIdentifier
    getUsedIdentifier identifier span IdentifierDetails {..}
      | Just identifierSpan <- realSrcSpanToIdentifierSpan span
      , Right name <- identifier
      , Use `Set.member` identInfo = Just $ UsedIdentifier name identifierSpan
      | otherwise = Nothing

updateColOffset :: Int -> Int -> Int -> Int
updateColOffset row lineOffset colOffset
  | row == lineOffset = colOffset
  | otherwise = 0

usedIdentifiersToTextEdits :: Range -> NameEnv [ImportedBy] -> Text -> [UsedIdentifier] -> [TextEdit]
usedIdentifiersToTextEdits range nameToImportedByMap sourceText usedIdentifiers
  | let sortedUsedIdentifiers = sortOn usedIdentifierSpan usedIdentifiers =
      State.evalState (makeStateComputation sortedUsedIdentifiers) (Text.lines sourceText, 0, 0)
  where
    folder :: [TextEdit] -> UsedIdentifier -> State ([Text], Int, Int) [TextEdit]
    folder prevTextEdits UsedIdentifier{usedIdentifierName, usedIdentifierSpan}
      | Just importedBys <- lookupNameEnv nameToImportedByMap usedIdentifierName
      , Just ImportedBy{importedByAlias} <- find (isRangeWithinImportedBy range) importedBys
      , let IdentifierSpan row startCol _ = usedIdentifierSpan
      , let identifierRange = identifierSpanToRange usedIdentifierSpan
      , let aliasText = Text.pack $ moduleNameString importedByAlias
      , let identifierText = Text.pack $ occNameString $ nameOccName usedIdentifierName
      , let qualifiedIdentifierText = aliasText <> "." <> identifierText = do
          (sourceTextLines, lineOffset, updateColOffset row lineOffset -> colOffset) <- State.get
          let lines = List.drop (row - lineOffset) sourceTextLines
          let (replacementText, remainingLines) =
                if | line : remainingLines <- lines
                   , let lineStartingAtIdentifier = Text.drop (startCol - colOffset) line
                   , Just (c, _) <- Text.uncons lineStartingAtIdentifier
                   , let isParenthesized = c == '('
                   , let isBackticked = c == '`'
                   , let replacementText =
                           if | isParenthesized -> "(" <> qualifiedIdentifierText <> ")"
                              | isBackticked -> "`" <> qualifiedIdentifierText <> "`"
                              | otherwise -> qualifiedIdentifierText ->
                       (replacementText, lineStartingAtIdentifier : remainingLines)
                   | otherwise -> (qualifiedIdentifierText, lines)
          let textEdit = TextEdit identifierRange replacementText
          State.put (remainingLines, row, startCol)
          pure $ textEdit : prevTextEdits
      | otherwise = pure prevTextEdits

    makeStateComputation :: [UsedIdentifier] -> State ([Text], Int, Int) [TextEdit]
    makeStateComputation usedIdentifiers = foldM folder [] usedIdentifiers

-- The overall idea:
-- 1. GlobalRdrEnv from typechecking phase contains info on what imported a
--    name.
-- 2. refMap from GetHieAst contains location of names and how they are used.
-- 3. For each used name in refMap check whether the name comes from an import
--    at the origin of the code action.
codeActionProvider :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeActionProvider ideState _pluginId (CodeActionParams _ _ documentId range _) = do
  normalizedFilePath <- getNormalizedFilePathE (documentId ^. L.uri)
  TcModuleResult { tmrParsed, tmrTypechecked } <- runActionE "QualifyImportedNames.TypeCheck" ideState $ useE TypeCheck normalizedFilePath
  if isJust (findLImportDeclAt range tmrParsed)
    then do
          HAR {..} <- runActionE "QualifyImportedNames.GetHieAst" ideState (useE GetHieAst normalizedFilePath)
          (_, sourceTextM) <-  runActionE "QualifyImportedNames.GetFileContents" ideState (useE GetFileContents normalizedFilePath)
          sourceText <- handleMaybe (PluginRuleFailed "GetFileContents") sourceTextM
          let globalRdrEnv = tcg_rdr_env tmrTypechecked
              nameToImportedByMap = globalRdrEnvToNameToImportedByMap globalRdrEnv
              usedIdentifiers = refMapToUsedIdentifiers refMap
              textEdits = usedIdentifiersToTextEdits range nameToImportedByMap sourceText usedIdentifiers
          pure  $ InL (makeCodeActions (documentId ^. L.uri) textEdits)
    else pure  $ InL []

