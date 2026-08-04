{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror  #-}

{-| Logic for renaming qualified import aliases.

The basic approach is this:

1. Get the parsed AST and see if there is an import alias at the cursor.
2. Check whether multiple modules are imported using the same alias.
3. Rename entities throughout the AST:
    * If only one module uses the alias, perform renaming using 'RdrName' and
    the parsed AST.
    * If multiple modules use the alias, perform alias resolution and renaming
    using 'GlobalRdrEnv' and the typechecked AST.

The common case, with each alias corresponding to one module, should be very
fast, even when the user renames multiple aliases in quick succession.

NOTE: This module avoids manipulating LSP 'Position' and 'Range' values
directly, because by default these are in UTF-16 code units, while GHC source
spans are in Unicode code points. Instead, this module uses
'VFS.CodePointPosition' and 'VFS.CodePointRange'.
-}
module Ide.Plugin.Rename.ImportAlias
    ( getParsedModuleStale
    , resolveAliasAtPos
    , aliasBasedRename
    ) where

import           Control.Lens                     ((&), (+~), (.~), (^.))
import           Control.Monad                    (guard, when)
import           Control.Monad.Except             (ExceptT,
                                                   MonadError (throwError))
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Containers.ListUtils        (nubOrd)
import           Data.Generics
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Text                        as T
import           Development.IDE                  (realSrcSpanToCodePointRange)
import           Development.IDE.Core.FileStore   (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service     hiding (Log)
import           Development.IDE.Core.Shake       hiding (Log)
import           Development.IDE.GHC.Compat       hiding (importDecl)
import           Development.IDE.GHC.Compat.Util  (stringToStringBuffer)
import           GHC.Data.FastString              (lengthFS)
import qualified GHC.Utils.Error                  as GHC
import           Ide.Plugin.Error
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types      hiding (Position, Range)
import qualified Language.LSP.Protocol.Types      as LSP
import           Language.LSP.VFS                 (codePointRangeToRange)
import qualified Language.LSP.VFS                 as VFS

-- | The module name, alias name, declaration text range, and sharing status
-- for an import alias.
-- For example, @import Data.List as L@ corresponds to @ImportAlias "Data.List"
-- "L" <text range of "L"> <whether any other modules are imported as "L">@.
data ImportAlias = ImportAlias
    { aliasModuleName :: ModuleName
    , aliasName       :: ModuleName
    , aliasDeclRange  :: VFS.CodePointRange
    , aliasIsShared   :: Bool
    }
    deriving (Eq, Ord)

-- | Fetch the parsed module for a file, accepting a stale result.
-- Returns @Nothing@ if the file has never been indexed.
getParsedModuleStale ::
    MonadIO m =>
    IdeState ->
    NormalizedFilePath ->
    m (Maybe ParsedModule)
getParsedModuleStale state nfp =
    liftIO $ fmap fst <$>
        runAction "rename.getParsedModuleStale" state
            (useWithStale GetParsedModule nfp)

-- | Return the 'ImportAlias' and corresponding text range at the cursor. The
-- cursor may be on the alias token in an import declaration or on a qualifier
-- at a use site. If multiple imports share the same alias, falls back to the
-- typechecked module's 'GlobalRdrEnv' to disambiguate.
-- Returns @Nothing@ if the cursor is not on an import alias.
-- HACK: The first argument is `Rename.getNamesAtPos`, parameterized to avoid a
-- circular dependency.
resolveAliasAtPos ::
    MonadIO m =>
    (IdeState -> NormalizedFilePath -> LSP.Position -> ExceptT PluginError m [Name]) ->
    IdeState ->
    NormalizedFilePath ->
    LSP.Position ->
    VFS.CodePointPosition ->
    Maybe (XRec GhcPs [LIE GhcPs]) ->
    [LImportDecl GhcPs] ->
    [LHsDecl GhcPs] ->
    ExceptT PluginError m (Maybe (LSP.Range, ImportAlias))
resolveAliasAtPos getNamesAtPosFn state nfp lspPos pos exports imports hsDecls = do
    virtualFile <- runActionE "rename.getVirtualFile" state
        $ handleMaybeM (PluginInternalError
            ("Virtual file not found: " <> T.pack (show nfp)))
        $ getVirtualFile nfp
    let toLSPRange (range, alias) = case codePointRangeToRange virtualFile range of
            Nothing       -> Nothing
            Just lspRange -> Just (lspRange, alias)
    case findAliasDeclAtPos pos imports of
        Just alias -> pure $ toLSPRange (aliasDeclRange alias, alias)
        Nothing -> case findAliasUseAtPos pos exports imports hsDecls of
            Nothing -> pure Nothing
            Just (_, []) -> pure Nothing
            Just (range, [alias]) -> pure $ toLSPRange (range, alias)
            Just (range, candidates) -> do
                tcModule <- runActionE "rename.resolveAlias" state $ useE TypeCheck nfp
                namesAtPos <- getNamesAtPosFn state nfp lspPos
                case disambiguateAliasUse tcModule namesAtPos candidates of
                    [] -> pure Nothing
                    [alias] -> pure $ toLSPRange (range, alias)
                    aliases@(alias1 : alias2 : _) -> throwError $ PluginInvalidParams $
                        let aliasCount = T.pack (show (length aliases))
                            aliasText = moduleNameText (aliasName alias1)
                            module1 = moduleNameText (aliasModuleName alias1)
                            module2 = moduleNameText (aliasModuleName alias2)
                        in "Alias " <> quote aliasText
                            <> " is ambiguous (matching " <> aliasCount
                            <> " imports, including " <> quote module1
                            <> " and " <> quote module2
                            <> "). Try renaming " <> quote aliasText
                            <> " in one of these import declarations directly."

-- | Build a 'WorkspaceEdit' renaming an import alias and all its use sites.
aliasBasedRename ::
    MonadIO m =>
    IdeState ->
    NormalizedFilePath ->
    Uri ->
    ImportAlias ->
    Maybe (XRec GhcPs [LIE GhcPs]) ->
    [LHsDecl GhcPs] ->
    T.Text ->
    ExceptT PluginError m (MessageResult Method_TextDocumentRename)
aliasBasedRename state nfp uri importAlias exports hsDecls newNameText = do
    when (not (isValidAlias newNameText)) $
        throwError (PluginInvalidParams (quote newNameText <> " is an invalid import alias."))
    let ImportAlias{aliasDeclRange, aliasIsShared} = importAlias
    virtualFile <- runActionE "rename.getVirtualFile" state
        $ handleMaybeM (PluginInternalError
            ("Virtual file not found: " <> T.pack (show nfp)))
        $ getVirtualFile nfp
    useSiteRanges <-
        if aliasIsShared
        then do
            tcModule <- runActionE "rename.sharedAliasRanges" state $ useE TypeCheck nfp
            pure $ aliasUseSiteRangesDisambiguated tcModule importAlias exports hsDecls
        else
            pure $ aliasUseSiteRanges importAlias exports hsDecls
    declEdit <- handleMaybe (PluginInternalError "Alias declaration span is out of range")
        $ rangeToTextEdit virtualFile newNameText aliasDeclRange
    useEdits <- handleMaybe (PluginInternalError "A use site span is out of range")
        $ traverse (rangeToTextEdit virtualFile newNameText) useSiteRanges
    let allEdits = declEdit : useEdits
    verTxtDocId <- liftIO $ runAction "rename.getVersionedTextDoc" state $
        getVersionedTextDoc (TextDocumentIdentifier uri)
    let fileChanges = Just $ M.singleton (verTxtDocId ^. L.uri) allEdits
        -- TODO: Replace 'Nothing' with meaningful details (`ChangeAnnotation`).
        workspaceEdit = WorkspaceEdit fileChanges Nothing Nothing
    pure $ InL workspaceEdit

-- | Find the 'ImportAlias' if the cursor is on an import alias declaration,
-- such as @L@ in @import Data.List as L@.
findAliasDeclAtPos ::
    VFS.CodePointPosition ->
    [LImportDecl GhcPs] ->
    Maybe ImportAlias
findAliasDeclAtPos pos imports = listToMaybe $ do
    let allAliases = mapMaybe (fmap unLoc . ideclAs . unLoc) imports
    importDecl <- map unLoc imports
    Just locatedAlias <- [ideclAs importDecl]
    RealSrcSpan aliasDeclSpan _ <- [getLoc locatedAlias]
    let aliasDeclRange = realSrcSpanToCodePointRange aliasDeclSpan
    guard (rangeContainsPositionInclusive aliasDeclRange pos)
    let aliasModuleName = unLoc (ideclName importDecl)
        aliasName = unLoc locatedAlias
        aliasIsShared = case filter (== aliasName) allAliases of
            []          -> False
            [_]         -> False
            (_ : _ : _) -> True
    [ImportAlias{aliasModuleName, aliasName, aliasDeclRange, aliasIsShared}]

-- | Find the text range and matching 'ImportAlias' for the name qualifier at
-- the cursor, such as @L@ in @L.take@.
-- Returns multiple aliases if multiple modules share the same alias.
findAliasUseAtPos ::
    VFS.CodePointPosition ->
    Maybe (XRec GhcPs [LIE GhcPs]) ->
    [LImportDecl GhcPs] ->
    [LHsDecl GhcPs] ->
    Maybe (VFS.CodePointRange, [ImportAlias])
findAliasUseAtPos pos exports imports hsDecls =
    let qualifiersAtPos = do
            locatedRdrName <- locateRdrNames exports hsDecls
            Qual qualifier _ <- [unLoc locatedRdrName]
            RealSrcSpan qualifiedNameSpan _ <- [getLoc locatedRdrName]
            let qualifiedNameRange = realSrcSpanToCodePointRange qualifiedNameSpan
            guard (rangeContainsPositionInclusive qualifiedNameRange pos)
            let qualifierLength = fromIntegral (moduleNameLength qualifier)
                qualifierStart = qualifiedNameRange ^. VFS.start
                qualifierRange = qualifiedNameRange
                    & VFS.end .~ (qualifierStart & VFS.character +~ qualifierLength)
            guard (rangeContainsPositionInclusive qualifierRange pos)
            [(qualifierRange, qualifier)]
    in case qualifiersAtPos of
        [] -> Nothing
        (rangeAtPos, qualifierAtPos) : _ -> Just $ (,) rangeAtPos $ do
            let allAliases = mapMaybe (fmap unLoc . ideclAs . unLoc) imports
            importDecl <- map unLoc imports
            Just locatedAlias <- [ideclAs importDecl]
            let aliasName = unLoc locatedAlias
            guard (aliasName == qualifierAtPos)
            RealSrcSpan aliasDeclSpan _ <- [getLoc locatedAlias]
            let aliasModuleName = unLoc (ideclName importDecl)
                aliasDeclRange = realSrcSpanToCodePointRange aliasDeclSpan
                aliasIsShared = length (filter (== aliasName) allAliases) > 1
            [ImportAlias{aliasModuleName, aliasName, aliasDeclRange, aliasIsShared}]

-- | Collect the 'CodePointRange' of every qualified use of @importAlias@, such
-- as @L@ in @L.take@, @L.drop@, and so on.
aliasUseSiteRanges ::
    ImportAlias ->
    Maybe (XRec GhcPs [LIE GhcPs]) ->
    [LHsDecl GhcPs] ->
    [VFS.CodePointRange]
aliasUseSiteRanges importAlias exports hsDecls = nubOrd $ do
    let ImportAlias{aliasName} = importAlias
        aliasLength = fromIntegral (moduleNameLength aliasName)
    locatedRdrName <- locateRdrNames exports hsDecls
    Qual qualifier _ <- [unLoc locatedRdrName]
    guard (qualifier == aliasName)
    RealSrcSpan qualifiedNameSpan _ <- [getLoc locatedRdrName]
    let qualifiedNameRange = realSrcSpanToCodePointRange qualifiedNameSpan
        qualifierStart = qualifiedNameRange ^. VFS.start
        qualifierRange = qualifiedNameRange
            & VFS.end .~ (qualifierStart & VFS.character +~ aliasLength)
    [qualifierRange]

---------------------------------------------------------------------------------------------------
-- Special case: Multiple imports use the same alias

-- | Resolve an ambiguous name qualifier by consulting the typechecked module's
-- 'GlobalRdrEnv' (or GRE). Used when multiple imports share the same alias. The
-- caller is responsible for providing the names at the cursor.
-- Returns multiple results if multiple modules export the same name (such as
-- @L.view@ with both @Control.Lens as L@ and @Control.Lens.Getter as L@).
disambiguateAliasUse ::
    TcModuleResult ->
    [Name] ->
    [ImportAlias] ->
    [ImportAlias]
disambiguateAliasUse tcModule namesAtPos candidates = nubOrd $ do
    let rdrEnv = tcg_rdr_env (tmrTypechecked tcModule)
    name <- namesAtPos
    nameGREElement <- maybeToList (lookupGRE_Name rdrEnv name)
    importSpec <- gre_imp nameGREElement
    candidate@ImportAlias{aliasModuleName} <- candidates
    guard (importSpecModule importSpec == aliasModuleName)
    [candidate]

-- | A variant of 'aliasUseSiteRanges' that resolves name qualifiers into full
-- module names and only selects those matching the module of @importAlias@.
-- Used when multiple imports share the same alias.
aliasUseSiteRangesDisambiguated ::
    TcModuleResult ->
    ImportAlias ->
    Maybe (XRec GhcPs [LIE GhcPs]) ->
    [LHsDecl GhcPs] ->
    [VFS.CodePointRange]
aliasUseSiteRangesDisambiguated tcModule importAlias exports hsDecls = nubOrd $ do
    let rdrEnv = tcg_rdr_env (tmrTypechecked tcModule)
        ImportAlias{aliasModuleName, aliasName} = importAlias
        aliasLength = fromIntegral (moduleNameLength aliasName)
    locatedRdrName <- locateRdrNames exports hsDecls
    rdrName@(Qual qualifier name) <- [unLoc locatedRdrName]
    guard (qualifier == aliasName)
    nameGREElement <- pickGREs rdrName $ lookupGlobalRdrEnv rdrEnv name
    importSpec <- gre_imp nameGREElement
    guard (importSpecModule importSpec == aliasModuleName)
    RealSrcSpan qualifiedNameSpan _ <- [getLoc locatedRdrName]
    let qualifiedNameRange = realSrcSpanToCodePointRange qualifiedNameSpan
        qualifierStart = qualifiedNameRange ^. VFS.start
        qualifierRange = qualifiedNameRange
            & VFS.end .~ (qualifierStart & VFS.character +~ aliasLength)
    [qualifierRange]

---------------------------------------------------------------------------------------------------
-- Utility functions

-- | Locate 'RdrName' identifiers in the given export list and declarations.
locateRdrNames ::
    Maybe (XRec GhcPs [LIE GhcPs]) ->
    [LHsDecl GhcPs] ->
    [XRec GhcPs RdrName]
locateRdrNames exports hsDecls =
    listify (const True) exports ++ listify (const True) hsDecls

-- | Check whether the given text is a valid alias.
-- Allows Unicode characters the same way GHC does.
-- REVIEW: If this looks good, we can add it to the existing name-based renaming
-- logic too (and move the CPP stuff to @Compat@).
isValidAlias :: T.Text -> Bool
isValidAlias t = case unP parseIdentifier parseState of
    POk _ _ -> True
    _       -> False
    where
        filename = ""
        location = mkRealSrcLoc filename 1 1
        buffer = stringToStringBuffer (T.unpack (t <> ".f"))
        parseState = initParserState minimalParserOpts buffer location

minimalParserOpts :: ParserOpts
#if MIN_VERSION_ghc(9,13,0)
minimalParserOpts = mkParserOpts mempty emptyDiagOpts False False False False
#else
minimalParserOpts = mkParserOpts mempty emptyDiagOpts [] False False False False
#endif

emptyDiagOpts :: GHC.DiagOpts
#if MIN_VERSION_ghc(9,7,0)
emptyDiagOpts = GHC.emptyDiagOpts
#else
emptyDiagOpts = GHC.DiagOpts mempty mempty False False Nothing defaultSDocContext
#endif

-- >>> isValidAlias (T.pack "M") == True
-- >>> isValidAlias (T.pack "M.F") == True
-- >>> isValidAlias (T.pack "m") == False
-- >>> isValidAlias (T.pack "m.F") == False
-- >>> isValidAlias (T.pack "m.f") == False
-- >>> isValidAlias (T.pack "M.F hiding ()") == False
-- >>> isValidAlias (T.pack "Just . M") == False
-- >>> isValidAlias (T.pack "ǲ") == True
-- >>> isValidAlias (T.pack "𝐹") == True
-- >>> isValidAlias (T.pack "𝑓") == False

-- | Check whether a 'CodePointRange' contains a 'CodePointPosition' (inclusive
-- start, inclusive end).
-- NOTE: The use of inclusive end allows the user to place the cursor at the end
-- of an import alias and rename it.
rangeContainsPositionInclusive :: VFS.CodePointRange -> VFS.CodePointPosition -> Bool
rangeContainsPositionInclusive
    (VFS.CodePointRange
        (VFS.CodePointPosition startLine startColumn)
        (VFS.CodePointPosition endLine endColumn))
    (VFS.CodePointPosition posLine posColumn)
    =  (posLine > startLine || (posLine == startLine && posColumn >= startColumn))
    && (posLine < endLine   || (posLine == endLine   && posColumn <= endColumn))

-- | Build a 'TextEdit' from a 'VFS.CodePointRange' and replacement text.
-- Returns @Nothing@ if the range is out of bounds in the VFS.
rangeToTextEdit :: VFS.VirtualFile -> T.Text -> VFS.CodePointRange -> Maybe TextEdit
rangeToTextEdit virtualFile newText range = TextEdit
    <$> VFS.codePointRangeToRange virtualFile range
    <*> Just newText

-- | Return the length in Unicode code points for a 'ModuleName'.
moduleNameLength :: ModuleName -> Int
moduleNameLength = lengthFS . moduleNameFS

-- | Return the module name as a 'Text' value.
moduleNameText :: ModuleName -> T.Text
moduleNameText = T.pack . moduleNameString

-- | Surround the given text with curly single quotation marks (like GHC does in
-- compiler messages).
quote :: T.Text -> T.Text
quote t = "‘" <> t <> "’"
