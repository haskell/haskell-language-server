{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}

module Ide.Plugin.QualifyImportedNames (descriptor) where

import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.DList                        (DList)
import qualified Data.DList                        as DList
import           Data.Foldable                     (Foldable (fold, foldl'),
                                                    find)
import           Data.Function                     ((&))
import qualified Data.HashMap.Internal.Strict      as HashMap
import qualified Data.IntMap.Strict                as IntMap
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (mapMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Development.IDE.Core.RuleTypes    (GetHieAst (GetHieAst),
                                                    GetParsedModule (GetParsedModule),
                                                    HieAstResult (HAR, refMap),
                                                    TcModuleResult (TcModuleResult, tmrParsed, tmrTypechecked),
                                                    TypeCheck (TypeCheck))
import           Development.IDE.Core.Shake        (IdeState,
                                                    ShakeExtras (ShakeExtras),
                                                    getShakeExtras, hiedb, use)
import           Development.IDE.GHC.Compat.Core   (GenLocated (L), GhcPs,
                                                    GhcRn,
                                                    GlobalRdrElt (GRE, gre_imp, gre_name),
                                                    GlobalRdrEnv,
                                                    ImpDeclSpec (ImpDeclSpec, is_as, is_dloc, is_qual),
                                                    ImportAvails (ImportAvails, imp_mods),
                                                    ImportDecl (ImportDecl, ideclAs, ideclHiding, ideclName, ideclQualified),
                                                    ImportDeclQualifiedStyle (NotQualified),
                                                    ImportSpec (ImpSpec),
                                                    LImportDecl,
                                                    Module (Module), ModuleName,
                                                    Name, NameEnv, OccName,
                                                    ParsedModule (ParsedModule, pm_parsed_source),
                                                    ParsedSource, SrcSpan,
                                                    TcGblEnv (TcGblEnv, tcg_imports, tcg_mod, tcg_rdr_env, tcg_used_gres),
                                                    findImportUsage, getLoc,
                                                    getMinimalImports,
                                                    globalRdrEnvElts,
                                                    hsmodImports, ieNames,
                                                    initTcWithGbl,
                                                    lookupModuleEnv,
                                                    lookupNameEnv, mkNameEnv,
                                                    moduleNameString,
                                                    nameModule_maybe,
                                                    nameOccName, occNameString,
                                                    pattern RealSrcSpan,
                                                    rdrNameOcc,
                                                    realSrcSpanStart, unLoc)
import           Development.IDE.GHC.Error         (isInsideSrcSpan,
                                                    realSrcSpanToRange)
import           Development.IDE.Types.Diagnostics (List (List))
import           Development.IDE.Types.Location    (NormalizedFilePath,
                                                    Range (Range), Uri)
import           Ide.Types                         (CommandFunction, CommandId,
                                                    PluginCommand (PluginCommand),
                                                    PluginDescriptor (pluginCommands, pluginHandlers, pluginRules),
                                                    PluginId,
                                                    PluginMethodHandler,
                                                    defaultPluginDescriptor,
                                                    mkPluginHandler)
import           Language.LSP.Types                (ApplyWorkspaceEditParams (ApplyWorkspaceEditParams),
                                                    CodeAction (..),
                                                    CodeActionKind (..),
                                                    CodeActionParams (..),
                                                    Method (TextDocumentCodeAction),
                                                    SMethod (STextDocumentCodeAction, SWorkspaceApplyEdit),
                                                    TextDocumentIdentifier (..),
                                                    TextEdit (TextEdit),
                                                    WorkspaceEdit (..),
                                                    toNormalizedUri,
                                                    type (|?) (..),
                                                    uriToNormalizedFilePath)
import           UniqFM                            (emptyUFM, plusUFM_C,
                                                    unitUFM)

import           Development.IDE.Core.Service      (runAction)
import           Development.IDE.GHC.Compat        (ContextInfo (Use),
                                                    Identifier,
                                                    IdentifierDetails (IdentifierDetails, identInfo),
                                                    RefMap, Span)
import           Prelude

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId) {
  pluginHandlers = mconcat
    [ mkPluginHandler STextDocumentCodeAction codeActionProvider
    ]
}

isRangeWithinSrcSpan :: Range -> SrcSpan -> Bool
isRangeWithinSrcSpan (Range start end) srcSpan =
  isInsideSrcSpan start srcSpan && isInsideSrcSpan end srcSpan

findLImportDeclAt :: Range -> ParsedModule -> Maybe (LImportDecl GhcPs)
findLImportDeclAt range parsedModule
  | ParsedModule {..} <- parsedModule
  , L _ hsModule <- pm_parsed_source
  , locatedImportDecls <- hsmodImports hsModule =
      find (\ (L srcSpan _) -> isRangeWithinSrcSpan range srcSpan) locatedImportDecls
  | otherwise = Nothing

makeCodeActions :: Uri -> [TextEdit] -> [a |? CodeAction]
makeCodeActions uri textEdits = [InR CodeAction {..} | not (null textEdits)]
  where _title = "Qualify imported names"
        _kind = Just CodeActionQuickFix
        _command = Nothing
        _edit = Just WorkspaceEdit {..}
        _changes = Just $ HashMap.singleton uri $ List textEdits
        _documentChanges = Nothing
        _diagnostics = Nothing
        _isPreferred = Nothing
        _disabled = Nothing
        _xdata = Nothing
        _changeAnnotations = Nothing

getTypeCheckedModule :: IdeState -> NormalizedFilePath -> IO (Maybe TcModuleResult)
getTypeCheckedModule ideState normalizedFilePath =
  runAction "QualifyImportedNames.TypeCheck" ideState (use TypeCheck normalizedFilePath)

getHieAst :: IdeState -> NormalizedFilePath -> IO (Maybe HieAstResult)
getHieAst ideState normalizedFilePath =
  runAction "QualifyImportedNames.GetHieAst" ideState (use GetHieAst normalizedFilePath)

data ImportedBy = ImportedBy {
  importedByAlias   :: !ModuleName,
  importedBySrcSpan :: !SrcSpan
}

isRangeWithinImportedBy :: Range -> ImportedBy -> Bool
isRangeWithinImportedBy range (ImportedBy _ srcSpan) = isRangeWithinSrcSpan range srcSpan

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

data UsedIdentifier = UsedIdentifier {
  usedIdentifierName :: !Name,
  usedIdentifierSpan :: !Span
}

refMapToUsedIdentifiers :: RefMap a -> [UsedIdentifier]
refMapToUsedIdentifiers = DList.toList . Map.foldlWithKey' folder DList.empty
  where
    folder acc identifier spanIdentifierDetailsPairs =
      DList.fromList (mapMaybe (uncurry (getUsedIdentifier identifier)) spanIdentifierDetailsPairs) <> acc

    getUsedIdentifier :: Identifier -> Span -> IdentifierDetails a -> Maybe UsedIdentifier
    getUsedIdentifier identifier span IdentifierDetails {..}
      | Right name <- identifier
      , Use `elem` identInfo = Just $ UsedIdentifier name span
      | otherwise = Nothing

occNameToText :: OccName -> Text
occNameToText = Text.pack . occNameString

usedIdentifierToTextEdit :: Range -> NameEnv [ImportedBy] -> UsedIdentifier -> Maybe TextEdit
usedIdentifierToTextEdit range nameToImportedByMap usedIdentifier
  | let UsedIdentifier identifierName identifierSpan = usedIdentifier
  , Just importedBys <- lookupNameEnv nameToImportedByMap identifierName
  , Just (ImportedBy alias _) <- find (isRangeWithinImportedBy range) importedBys
  , let aliasText = Text.pack $ moduleNameString alias
  , let identifierRange = realSrcSpanToRange identifierSpan
  , let identifierText = Text.pack $ occNameString $ nameOccName identifierName =
      Just $ TextEdit identifierRange (aliasText <> "." <> identifierText)
  | otherwise = Nothing

-- The overall idea is to get the GlobalRdrEnv from the type checking phase
-- turn it into a Name to ImportedBy map, and then use the refMap from
-- GetHieAst to iterate through the used names matching
codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider ideState pluginId (CodeActionParams _ _ documentId range context)
  | TextDocumentIdentifier uri <- documentId
  , Just normalizedFilePath <- uriToNormalizedFilePath (toNormalizedUri uri) = liftIO $ do
      tcModuleResult <- getTypeCheckedModule ideState normalizedFilePath
      if | Just TcModuleResult { tmrParsed, tmrTypechecked } <- tcModuleResult
         , Just _ <- findLImportDeclAt range tmrParsed -> do
             hieAstResult <- getHieAst ideState normalizedFilePath
             if | Just HAR {..} <- hieAstResult
                , let globalRdrEnv = tmrTypechecked & tcg_rdr_env
                , let nameToImportedByMap = globalRdrEnvToNameToImportedByMap globalRdrEnv
                , let usedIdentifiers = refMapToUsedIdentifiers refMap
                , let textEdits = mapMaybe (usedIdentifierToTextEdit range nameToImportedByMap) usedIdentifiers ->
                    pure $ Right $ List (makeCodeActions uri textEdits)
                | otherwise -> pure $ Right $ List []
         | otherwise -> pure $ Right $ List []
  | otherwise = pure $ Right $ List []

