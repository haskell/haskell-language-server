{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall -Werror #-}

{-| Logic for renaming qualified import aliases.

For example:

> -- Before: ---------------------------
> import qualified Data.List as L
> bar = L.take
> -- After: ----------------------------
> import qualified Data.List as List
> bar = List.take

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
import           Control.Monad                    (guard)
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
import           GHC.Data.FastString              (lengthFS)
import           Ide.Plugin.Error
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types      hiding (Position, Range)
import qualified Language.LSP.Protocol.Types      as LSP
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
    guard (rangeContainsPosition aliasDeclRange pos)
    let aliasModuleName = unLoc (ideclName importDecl)
        aliasName = unLoc locatedAlias
        aliasIsShared = length (filter (== aliasName) allAliases) > 1
    [ImportAlias{aliasModuleName, aliasName, aliasDeclRange, aliasIsShared}]

-- | Find the 'ImportAlias' matching the name qualifier at the cursor, such as
-- @L@ in @L.take@.
-- Returns multiple values if multiple modules share the same alias.
findAliasUseAtPos ::
    VFS.CodePointPosition ->
    [LImportDecl GhcPs] ->
    [LHsDecl GhcPs] ->
    [ImportAlias]
findAliasUseAtPos pos imports hsDecls =
    let qualifiersAtPos = do
            locatedRdrName :: XRec GhcPs RdrName <- listify (const True) hsDecls
            Qual qualifier _ <- [unLoc locatedRdrName]
            RealSrcSpan qualifiedNameSpan _ <- [getLoc locatedRdrName]
            let qualifiedNameRange = realSrcSpanToCodePointRange qualifiedNameSpan
            guard (rangeContainsPosition qualifiedNameRange pos)
            let qualifierLength = fromIntegral (moduleNameLength qualifier)
                qualifierStart = qualifiedNameRange ^. VFS.start
                qualifierRange = qualifiedNameRange
                    & VFS.end .~ (qualifierStart & VFS.character +~ qualifierLength)
            guard (rangeContainsPosition qualifierRange pos)
            [qualifier]
    in case qualifiersAtPos of
        [] -> []
        qualifierAtPos : _ -> do
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

-- | Return the 'ImportAlias' being renamed at the cursor. The cursor may be on
-- the alias token in an import declaration or on a qualifier at a use site. If
-- multiple imports share the same alias, falls back to the typechecked module's
-- 'GlobalRdrEnv' to disambiguate.
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
    [LImportDecl GhcPs] ->
    [LHsDecl GhcPs] ->
    ExceptT PluginError m (Maybe ImportAlias)
resolveAliasAtPos getNamesAtPosFn state nfp lspPos pos imports hsDecls =
    case findAliasDeclAtPos pos imports of
        Just alias -> pure (Just alias)
        Nothing -> case findAliasUseAtPos pos imports hsDecls of
            [] -> pure Nothing
            [alias] -> pure (Just alias)
            candidates -> do
                tcModule <- runActionE "rename.resolveAlias" state $ useE TypeCheck nfp
                namesAtPos <- getNamesAtPosFn state nfp lspPos
                case disambiguateAliasUse tcModule namesAtPos candidates of
                    [] -> pure Nothing
                    [alias] -> pure (Just alias)
                    aliases -> throwError $ PluginInvalidParams $
                        ambiguousAliasErrorMessage aliases

-- | Build a 'WorkspaceEdit' renaming an import alias and all its use sites.
aliasBasedRename ::
    MonadIO m =>
    IdeState ->
    NormalizedFilePath ->
    Uri ->
    ImportAlias ->
    [LHsDecl GhcPs] ->
    T.Text ->
    ExceptT PluginError m (MessageResult Method_TextDocumentRename)
aliasBasedRename state nfp uri importAlias hsDecls newNameText = do
    let ImportAlias{aliasDeclRange, aliasIsShared} = importAlias
    virtualFile <- runActionE "rename.getVirtualFile" state
        $ handleMaybeM (PluginInternalError
            ("Virtual file not found: " <> T.pack (show nfp)))
        $ getVirtualFile nfp
    useSiteRanges <-
        if aliasIsShared
        then do
            tcModule <- runActionE "rename.sharedAliasRanges" state $ useE TypeCheck nfp
            pure $ aliasUseSiteRangesDisambiguated tcModule importAlias hsDecls
        else
            pure $ aliasUseSiteRanges importAlias hsDecls
    declEdit <- handleMaybe (PluginInternalError "Alias declaration span is out of range")
        $ rangeToTextEdit virtualFile newNameText aliasDeclRange
    useEdits <- handleMaybe (PluginInternalError "A use site span is out of range")
        $ traverse (rangeToTextEdit virtualFile newNameText) useSiteRanges
    let allEdits = declEdit : useEdits
    verTxtDocId <- liftIO $ runAction "rename.getVersionedTextDoc" state $
        getVersionedTextDoc (TextDocumentIdentifier uri)
    let fileChanges = Just $ M.singleton (verTxtDocId ^. L.uri) allEdits
        -- TODO: Replace 'Nothing' with meaningful details for the workspace edit.
        workspaceEdit = WorkspaceEdit fileChanges Nothing Nothing
    pure $ InL workspaceEdit

-- | Collect the 'CodePointRange' of every qualified use of @importAlias@, such
-- as @L@ in @L.take@, @L.drop@, and so on.
aliasUseSiteRanges :: ImportAlias -> [LHsDecl GhcPs] -> [VFS.CodePointRange]
aliasUseSiteRanges importAlias hsDecls = nubOrd $ do
    let ImportAlias{aliasName} = importAlias
        aliasLength = fromIntegral (moduleNameLength aliasName)
    locatedRdrName :: XRec GhcPs RdrName <- listify (const True) hsDecls
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
    [LHsDecl GhcPs] ->
    [VFS.CodePointRange]
aliasUseSiteRangesDisambiguated tcModule importAlias hsDecls = nubOrd $ do
    let rdrEnv = tcg_rdr_env (tmrTypechecked tcModule)
        ImportAlias{aliasModuleName, aliasName} = importAlias
        aliasLength = fromIntegral (moduleNameLength aliasName)
    locatedRdrName :: XRec GhcPs RdrName <- listify (const True) hsDecls
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

ambiguousAliasErrorMessage :: [ImportAlias] -> T.Text
ambiguousAliasErrorMessage aliases@(alias1 : alias2 : _) =
    let aliasCount = T.pack (show (length aliases))
        aliasText = T.pack (moduleNameString (aliasName alias1))
        module1 = T.pack (moduleNameString (aliasModuleName alias1))
        module2 = T.pack (moduleNameString (aliasModuleName alias2))
        quote t = "‘" <> t <> "’"
    in ("Alias " <> quote aliasText
        <> " is ambiguous (matching " <> aliasCount
        <> " imports, including " <> quote module1 <> " and " <> quote module2
        <> "). Try renaming " <> quote aliasText
        <> " in one of these import declarations directly.")
ambiguousAliasErrorMessage _ = ""

---------------------------------------------------------------------------------------------------
-- Utility functions

-- | Check whether a 'CodePointRange' contains a 'CodePointPosition'
-- (inclusive start, exclusive end).
rangeContainsPosition :: VFS.CodePointRange -> VFS.CodePointPosition -> Bool
rangeContainsPosition
    (VFS.CodePointRange
        (VFS.CodePointPosition startLine startColumn)
        (VFS.CodePointPosition endLine endColumn))
    (VFS.CodePointPosition posLine posColumn)
    =  (posLine > startLine || (posLine == startLine && posColumn >= startColumn))
    && (posLine < endLine   || (posLine == endLine   && posColumn <  endColumn))

-- | Build a 'TextEdit' from a 'VFS.CodePointRange' and replacement text.
-- Returns @Nothing@ if the range is out of bounds in the VFS.
rangeToTextEdit :: VFS.VirtualFile -> T.Text -> VFS.CodePointRange -> Maybe TextEdit
rangeToTextEdit virtualFile newText range = TextEdit
    <$> VFS.codePointRangeToRange virtualFile range
    <*> Just newText

-- | Returns the length in Unicode code points for a 'ModuleName'.
moduleNameLength :: ModuleName -> Int
moduleNameLength = lengthFS . moduleNameFS
