{-# LANGUAGE DataKinds #-}

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
fast, even if the user renames multiple aliases in quick succession.
-}
module Ide.Plugin.Rename.ImportAlias
    ( getParsedModuleStale
    , ImportAlias (..)
    , findImportAliasDeclAtPos
    , findImportAliasUseAtPos
    , resolveAliasAtPos
    , aliasBasedRename
    , importAliasUseSiteSpans
    , importAliasUseSiteEdit
    , importAliasDeclEdit
    , rangeContainsPosition
    ) where

import           Control.Lens                     ((^.))
import           Control.Monad                    (guard)
import           Control.Monad.Except             (ExceptT, throwError)
import           Control.Monad.IO.Class           (MonadIO, liftIO)
import           Data.Generics
import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.Text                        as T
import           Development.IDE                  (realSrcSpanToRange)
import           Development.IDE.Core.FileStore   (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service     hiding (Log)
import           Development.IDE.Core.Shake       hiding (Log)
import           Development.IDE.GHC.Compat
import           Development.IDE.Types.Location
import           Ide.Plugin.Error
import qualified Language.LSP.Protocol.Lens       as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

-- | The module name, alias name, and declaration span for an import alias.
-- For example, @import Data.List as L@ corresponds to
-- @ImportAlias "Data.List" "L" <span of "L">@.
data ImportAlias = ImportAlias
    { aliasModuleName :: ModuleName
    , aliasName       :: ModuleName
    , aliasDeclSpan   :: RealSrcSpan
    }

-- | Fetch the parsed module for a file, accepting a stale result.
-- Returns @Nothing@ if the file has never been indexed.
getParsedModuleStale
    :: MonadIO m
    => IdeState
    -> NormalizedFilePath
    -> m (Maybe ParsedModule)
getParsedModuleStale state nfp =
    liftIO $ fmap fst <$>
        runAction "rename.getParsedModuleStale" state
            (useWithStale GetParsedModule nfp)

-- | Find the 'ImportAlias' for the alias declaration at the cursor, such as
-- @Alias@ in @import Module as Alias@.
findImportAliasDeclAtPos
    :: Position
    -> [LImportDecl GhcPs]
    -> Maybe ImportAlias
findImportAliasDeclAtPos pos imports = listToMaybe
    [ ImportAlias {aliasModuleName, aliasName, aliasDeclSpan}
    | _locatedImport@(L _ decl)      <- imports
    , Just locatedAlias              <- [ideclAs decl]
    , let aliasName = unLoc locatedAlias
    , RealSrcSpan aliasDeclSpan _    <- [locA locatedAlias]
    , rangeContainsPosition (realSrcSpanToRange aliasDeclSpan) pos
    , let aliasModuleName = unLoc (ideclName decl)
    ]

-- | Find the 'ImportAlias' matching the name qualifier at the cursor, such as
-- @Alias@ in @Alias.name@.
-- Returns multiple values if multiple modules share the same alias.
findImportAliasUseAtPos
    :: Position
    -> [LHsDecl GhcPs]
    -> [LImportDecl GhcPs]
    -> [ImportAlias]
findImportAliasUseAtPos pos decls imports =
    case listToMaybe
        [ qualifier
        | L (ann :: Anno RdrName) (Qual qualifier _) <- listify (const True) decls
        , RealSrcSpan useSiteSpan _  <- [locA ann]
        , rangeContainsPosition (realSrcSpanToRange useSiteSpan) pos
        , let qualifierLength = fromIntegral (length (moduleNameString qualifier))
              start           = realSrcSpanStart useSiteSpan
              line            = fromIntegral (srcLocLine start)
              startColumn     = fromIntegral (srcLocCol start)
              qualifierRange  = Range
                  (Position (line - 1) (startColumn - 1))
                  (Position (line - 1) (startColumn - 1 + qualifierLength))
        , rangeContainsPosition qualifierRange pos
        ] of
    Nothing -> []
    Just qualifierAtPos ->
        [ ImportAlias {aliasModuleName, aliasName, aliasDeclSpan}
        | _locatedImport@(L _ decl)   <- imports
        , Just locatedAlias           <- [ideclAs decl]
        , let aliasName = unLoc locatedAlias
        , aliasName == qualifierAtPos
        , RealSrcSpan aliasDeclSpan _ <- [locA locatedAlias]
        , let aliasModuleName = unLoc (ideclName decl)
        ]

-- | Return the module name and declaration span for the alias being renamed at
-- the cursor. The cursor may be on the alias token in an import declaration or
-- on a qualifier at a use site. If multiple imports share the same alias, falls
-- back to the typechecked module's 'GlobalRdrEnv' to disambiguate.
-- Returns @Nothing@ if the cursor is not on any alias declaration or qualifier.
-- HACK: The first argument is `Rename.getNamesAtPos`, parameterized to avoid a
-- circular dependency.
resolveAliasAtPos
    :: MonadIO m
    => (IdeState -> NormalizedFilePath -> Position -> ExceptT PluginError m [Name])
    -> IdeState
    -> NormalizedFilePath
    -> Position
    -> [LHsDecl GhcPs]
    -> [LImportDecl GhcPs]
    -> ExceptT PluginError m (Maybe ImportAlias)
resolveAliasAtPos getNamesAtPosFn state nfp pos decls imports =
    case findImportAliasDeclAtPos pos imports of
        Just result -> pure (Just result)
        Nothing     -> case findImportAliasUseAtPos pos decls imports of
            []       -> pure Nothing
            [result] -> pure (Just result)
            _many    -> do
                namesAtPos <- getNamesAtPosFn state nfp pos
                disambiguateAliasAtPos state nfp namesAtPos imports

-- | Build a 'WorkspaceEdit' renaming an import alias and all its use sites.
aliasBasedRename
    :: MonadIO m
    => IdeState
    -> NormalizedFilePath
    -> Uri
    -> ImportAlias
    -> [LImportDecl GhcPs]
    -> [LHsDecl GhcPs]
    -> T.Text
    -> ExceptT PluginError m (MessageResult Method_TextDocumentRename)
aliasBasedRename state nfp uri importAlias imports decls newNameText = do
    let oldAlias = aliasName importAlias
        declSpan = aliasDeclSpan importAlias
        duplicateAlias =
            length [ ()
                   | L _ decl <- imports
                   , Just locAlias <- [ideclAs decl]
                   , unLoc locAlias == oldAlias
                   ] > 1
    useSiteSpans <-
        if duplicateAlias
        then importAliasUseSiteSpansDisambiguated state nfp importAlias decls
        else pure $ importAliasUseSiteSpans importAlias decls
    let declEdit = importAliasDeclEdit newNameText declSpan
        useEdits = map (importAliasUseSiteEdit oldAlias newNameText) useSiteSpans
        allEdits = declEdit : useEdits
    verTxtDocId <- liftIO $ runAction "rename.getVersionedTextDoc" state $
        getVersionedTextDoc (TextDocumentIdentifier uri)
    let fileChanges = Just $ M.singleton (verTxtDocId ^. L.uri) allEdits
        -- TODO: Replace 'Nothing' with meaningful details for the workspace edit.
        workspaceEdit = WorkspaceEdit fileChanges Nothing Nothing
    pure $ InL workspaceEdit

-- | Collect the 'RealSrcSpan' of every qualified use of @oldAlias@, such as in
-- @oldAlias.foo@, @oldAlias.bar@, and so on.
-- Does not disambiguate if multiple imports share the alias.
importAliasUseSiteSpans
    :: ImportAlias
    -> [LHsDecl GhcPs]
    -> [RealSrcSpan]
importAliasUseSiteSpans importAlias decls =
    [ fullNameSpan
    | L (ann :: Anno RdrName) (Qual moduleAlias _) <- listify (const True) decls
    , moduleAlias == aliasName importAlias
    , RealSrcSpan fullNameSpan _ <- [locA ann]
    ]

-- | Build a 'TextEdit' replacing the qualifier part in a qualified name (like
-- from @Alias.name@ to @NewAlias.name@).
-- (NOTE: GHC uses 1-based positioning; LSP uses 0-based.)
importAliasUseSiteEdit
    :: ModuleName   -- ^ old alias, used to compute the qualifier width
    -> T.Text       -- ^ new alias text
    -> RealSrcSpan  -- ^ span of the full qualified name, such as @Alias.name@
    -> TextEdit
importAliasUseSiteEdit oldAlias newAlias fullNameSpan = TextEdit range newAlias
    where
        start    = realSrcSpanStart fullNameSpan
        line     = fromIntegral (srcLocLine start) - 1
        startCol = fromIntegral (srcLocCol  start) - 1
        endCol   = startCol + fromIntegral (length (moduleNameString oldAlias))
        range    = Range (Position line startCol) (Position line endCol)

-- | Build a 'TextEdit' replacing the alias token in an import declaration (like
-- from @import Module as Alias@ to @import Module as NewAlias@).
importAliasDeclEdit
    :: T.Text       -- ^ new alias text
    -> RealSrcSpan  -- ^ span of @Alias@ in @import Module as Alias@
    -> TextEdit
importAliasDeclEdit newAlias rsp = TextEdit (realSrcSpanToRange rsp) newAlias

-- | Check whether a range contains a position (inclusive start, exclusive end).
rangeContainsPosition :: Range -> Position -> Bool
rangeContainsPosition (Range (Position sl sc) (Position el ec)) (Position l c)
    =  (l > sl || (l == sl && c >= sc))
    && (l < el || (l == el && c <  ec))

---------------------------------------------------------------------------------------------------
-- Internal helpers

-- | Resolve an ambiguous alias use site by consulting the typechecked
-- module's 'GlobalRdrEnv'. Used when multiple imports share the same alias.
-- The caller is responsible for providing the names under the cursor.
-- TODO: Rename it to @disambiguateAliasUseAtPos@.
disambiguateAliasAtPos
    :: MonadIO m
    => IdeState
    -> NormalizedFilePath
    -> [Name]
    -> [LImportDecl GhcPs]
    -> ExceptT PluginError m (Maybe ImportAlias)
disambiguateAliasAtPos state nfp namesAtPos imports = do
    tcModule <- runActionE "rename.disambiguateAlias" state (useE TypeCheck nfp)
    let rdrEnv = tcg_rdr_env (tmrTypechecked tcModule)
    pure $ listToMaybe $ do
        name <- namesAtPos
        gre <- maybeToList (lookupGRE_Name rdrEnv name)
        impSpec <- gre_imp gre
        let declSpec = is_decl impSpec
            specModuleName = moduleName (is_mod declSpec)
            specAlias = is_as declSpec
        L _ decl <- imports
        guard (unLoc (ideclName decl) == specModuleName)
        Just locatedAlias <- [ideclAs decl]
        RealSrcSpan aliasDeclSpan _ <- [locA locatedAlias]
        pure (ImportAlias specModuleName specAlias aliasDeclSpan)

-- | Like 'importAliasUseSiteSpans' but filters to use sites that resolve
-- to names from @actualMod@, using the typechecked module's 'GlobalRdrEnv'.
-- Used when multiple imports share the same alias.
importAliasUseSiteSpansDisambiguated
    :: MonadIO m
    => IdeState
    -> NormalizedFilePath
    -> ImportAlias
    -> [LHsDecl GhcPs]
    -> ExceptT PluginError m [RealSrcSpan]
importAliasUseSiteSpansDisambiguated state nfp importAlias decls = do
    tcModule <- runActionE "rename.useSiteSpans" state (useE TypeCheck nfp)
    let rdrEnv = tcg_rdr_env (tmrTypechecked tcModule)
        allSpans = importAliasUseSiteSpansWithOcc (aliasName importAlias) decls
        ImportAlias actualMod _ _ = importAlias
    pure
        [ rsp
        | (occName, rsp) <- allSpans
        , gre <- lookupGRE rdrEnv $
            LookupRdrName (Qual (aliasName importAlias) occName) AllRelevantGREs
        , impSpec <- gre_imp gre
        , moduleName (is_mod (is_decl impSpec)) == actualMod
        ]

-- | Like 'importAliasUseSiteSpans' but also returns the 'OccName' of each
-- use, needed for 'GlobalRdrEnv' lookup in the disambiguated path.
importAliasUseSiteSpansWithOcc
    :: ModuleName
    -> [LHsDecl GhcPs]
    -> [(OccName, RealSrcSpan)]
importAliasUseSiteSpansWithOcc oldAlias decls =
    [ (occName, rsp)
    | L (ann :: Anno RdrName) (Qual moduleAlias occName) <- listify (const True) decls
    , moduleAlias == oldAlias
    , RealSrcSpan rsp _ <- [locA ann]
    ]
