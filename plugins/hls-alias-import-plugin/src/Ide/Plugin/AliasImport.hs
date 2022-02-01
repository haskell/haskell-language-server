{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}
-- {-# LANGUAGE ExplicitNamespaces #-}

module Ide.Plugin.AliasImport (descriptor) where

import           Control.DeepSeq                   (rwhnf)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Data.Aeson                        (FromJSON, ToJSON)
import           Data.Aeson.Types                  (Value (Null))
import           Data.Either                       (isRight)
import           Data.Foldable                     (find)
import           Data.Function                     ((&))
import qualified Data.HashMap.Internal.Strict      as HashMap
import           Data.IORef                        (readIORef)
import qualified Data.Map.Strict                   as Map
import           Data.Maybe                        (fromMaybe, mapMaybe)
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import           Debug.Trace                       (trace)
import           Development.IDE                   (realSrcSpanToRange)
import           Development.IDE.Core.RuleTypes    (GetHieAst (GetHieAst),
                                                    GetParsedModule (GetParsedModule),
                                                    HieAstResult (HAR, refMap))
import           Development.IDE.Core.Rules        (GetParsedModule (GetParsedModule),
                                                    IdeState, runAction)
import           Development.IDE.Core.Service      (IdeState, runAction)
import           Development.IDE.Core.Shake        (IdeState, use)
import           Development.IDE.GHC.Compat        (ContextInfo (Use),
                                                    Identifier,
                                                    IdentifierDetails (identInfo),
                                                    OccName (occNameFS), Span,
                                                    generateReferencesMap)
import           Development.IDE.GHC.Compat.Core   (GenLocated (L), GhcPs,
                                                    GhcRn,
                                                    ImportDecl (ImportDecl, ideclAs, ideclHiding, ideclName, ideclQualified),
                                                    ImportDeclQualifiedStyle (NotQualified),
                                                    LImportDecl,
                                                    Module (Module),
                                                    ParsedModule (ParsedModule, pm_parsed_source),
                                                    ParsedSource, SrcSpan,
                                                    TcGblEnv (tcg_used_gres),
                                                    findImportUsage, getLoc,
                                                    getMinimalImports,
                                                    hsmodImports, ieNames,
                                                    initTcWithGbl,
                                                    moduleNameString,
                                                    nameModule_maybe,
                                                    nameOccName,
                                                    pattern RealSrcSpan,
                                                    rdrNameOcc,
                                                    realSrcSpanStart, unLoc)
import           Development.IDE.GHC.Compat.Util   (unpackFS)
import           Development.IDE.GHC.Error         (isInsideSrcSpan)
import           Development.IDE.Graph.Classes     (Hashable, NFData (rnf))
import           Development.IDE.Types.Diagnostics (List (List))
import           Development.IDE.Types.Location    (Range (Range))
import           GHC.Generics                      (Generic)
import           Ide.Types                         (CommandFunction, CommandId,
                                                    PluginCommand (PluginCommand),
                                                    PluginDescriptor (pluginCommands, pluginHandlers, pluginRules),
                                                    PluginId,
                                                    PluginMethodHandler,
                                                    defaultPluginDescriptor,
                                                    mkPluginHandler)
import           Language.LSP.Server               (sendRequest)
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



-- Goal:
-- A code action located on an import that allows you to qualify every unqualified name used
-- in the code, imported from the module.
-- More specifically:
--
-- Unqualified | Explicit List | Hiding | ?
-- True        | True          | True   | qualify unqualified names not on the explicit list with final module name
-- True        | True          | False  | qualify unqualified names on the explicit list with final module name
-- True        | False         | True   | syntax error
-- True        | False         | False  | qualify all unqualified names imported from module with final module name
-- False       | True          | True   | rename qualified names not in explicit list
-- False       | True          | False  | rename qualified names in explicit list
-- False       | False         | True   | syntax error
-- False       | False         | False  | rename all qualified names
--
-- get final module name
-- get all names in source from a certain module, (part of a certain set - prob not necessary)
-- find if its qualified and rename the qualification or qualify it if not qualified
--
-- Algorithm:
-- 0. We are given the range where the code action is initialized which must be
--    an import declaration.
-- 1. Get the parsed source.
-- 2. Get the import of the code action range.
-- 3. Find the module associated with the import.
-- 4. Find the final name of the module.
-- 5. Find and replace each name in the parsed source of the module with a
--    version qualified by the modules final name.
--
-- Notes:
-- The idea is to create a code action provider that gets the parsed (or type
-- checked) source which creates a code action at the proper locations, and
-- gives this to the client.

hehe :: Text
hehe = undefined

-- aliasImportCommandId :: CommandId
-- aliasImportCommandId = "Huh?"

-- aliasImportCommand :: PluginCommand IdeState
-- aliasImportCommand = PluginCommand aliasImportCommandId "Alias import command" runAliasImportCommand

-- newtype AliasImportCommandParams = AliasImportCommandParams WorkspaceEdit
--   deriving Generic
--   deriving anyclass (FromJSON, ToJSON)

-- runAliasImportCommand :: CommandFunction IdeState AliasImportCommandParams
-- runAliasImportCommand state (AliasImportCommandParams edit) = do
--   _ <- sendRequest SWorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
--   pure (Right Null)


descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId) {
  -- pluginRules = minimalImportsRule,
  -- pluginCommands = [aliasImportCommand],
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

getImportedUsedIdentifierTextEdits :: ImportDecl GhcPs -> Identifier -> [(Span, IdentifierDetails i)] -> [TextEdit]
getImportedUsedIdentifierTextEdits importDecl identifier spanIdentifierDetailsPairs
  | ImportDecl {..} <- importDecl
  , Just qualification <- getQualificationIfIdentifierIsImportedByImportDecl importDecl identifier =
      mapMaybe (textEditIfIdentifierDetailsContainUse identifier qualification) spanIdentifierDetailsPairs
  | otherwise = []

getQualificationIfIdentifierIsImportedByImportDecl :: ImportDecl GhcPs -> Identifier -> Maybe Text
getQualificationIfIdentifierIsImportedByImportDecl importDecl identifier
  | ImportDecl {..} <- importDecl
  , Right name <- identifier
  , Just (Module _ identifierModuleName) <- nameModule_maybe name
  , L _ importModuleName <- ideclName
  , identifierModuleName == importModuleName
  , qualificationModuleName <- if | Just (L _ aliasModuleName) <- ideclAs -> aliasModuleName
                                  | otherwise -> importModuleName
  , qualificationText <- Text.pack $ moduleNameString qualificationModuleName =
      case ideclHiding of
        Nothing -> Just qualificationText
        Just (isHiding, L _ locatedImportExportEntities)
          | ieOccNames <- foldMap (map rdrNameOcc . ieNames . unLoc) locatedImportExportEntities
          , identifierOccName <- nameOccName name
          , ieOccNamesContainIdentifierOccName <- identifierOccName `elem` ieOccNames ->
              if (isHiding && not ieOccNamesContainIdentifierOccName)
              || (not isHiding && ieOccNamesContainIdentifierOccName)
              then Just qualificationText
              else Nothing
  | otherwise = Nothing

textEditIfIdentifierDetailsContainUse :: Identifier -> Text -> (Span, IdentifierDetails i) -> Maybe TextEdit
textEditIfIdentifierDetailsContainUse identifier qualification (span, identifierDetails)
  | Right name <- identifier
  , identifierNameText <- Text.pack $ unpackFS $ occNameFS $ nameOccName name
  , identifierDetailsContainUse identifierDetails
  , range <- realSrcSpanToRange span =
      Just $ TextEdit range (qualification <> "." <> identifierNameText)
  | otherwise = Nothing

identifierDetailsContainUse :: IdentifierDetails a -> Bool
identifierDetailsContainUse = elem Use . identInfo

codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider ideState pluginId (CodeActionParams _ _ documentId range context)
  | trace "Hiiiiiiiiiiiiii" False = undefined
  | TextDocumentIdentifier uri <- documentId
  , Just normalizedFilePath <- uriToNormalizedFilePath (toNormalizedUri uri) = liftIO $ do
      parsedModule <- runAction "QualifyImportedNames.GetParsedModule" ideState (use GetParsedModule normalizedFilePath)
      if | Just parsedModule <- parsedModule
         , Just (L _ ImportDecl {..}) <- findLImportDeclAt range parsedModule
         , NotQualified <- ideclQualified -> do
             hieAstResult <- runAction "QualifyImportedNames.GetHieAst" ideState (use GetHieAst normalizedFilePath)
             if | Just HAR {..} <- hieAstResult
                , Just (L _ importDecl) <- findLImportDeclAt range parsedModule
                , let folder acc k v = getImportedUsedIdentifierTextEdits importDecl k v ++ acc
                , importedUsedIdentifierTextEdits <- Map.foldlWithKey' folder [] refMap ->
                    let codeAction = InR CodeAction {..}
                        _title = "Qualify imported names"
                        _kind = Just CodeActionQuickFix
                        _command = Nothing
                        _edit = Just WorkspaceEdit {..}
                        _changes = Just $ HashMap.singleton uri $ List importedUsedIdentifierTextEdits
                        _documentChanges = Nothing
                        _diagnostics = Nothing
                        _isPreferred = Nothing
                        _disabled = Nothing
                        _xdata = Nothing
                        _changeAnnotations = Nothing
                    in pure $ Right $ List [codeAction | not (null importedUsedIdentifierTextEdits)]
                | otherwise -> pure $ Right $ List []
         | otherwise -> pure $ Right $ List []
  | otherwise = pure $ Right $ List []

