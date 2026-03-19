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
module Ide.Plugin.Rename.ImportAlias                                                                -- [ ] AI
    ( getParsedModuleStale                                                                          -- [ ] AI
    , ImportAlias (..)                                                                              -- [ ] AI
    , findImportAliasDeclAtPos                                                                      -- [ ] AI
    , findImportAliasUseAtPos                                                                       -- [ ] AI
    , resolveAliasAtPos                                                                             -- [ ] AI
    , aliasBasedRename                                                                              -- [ ] AI
    , importAliasUseSiteSpans                                                                       -- [ ] AI
    , importAliasUseSiteEdit                                                                        -- [ ] AI
    , importAliasDeclEdit                                                                           -- [ ] AI
    , rangeContainsPosition                                                                         -- [ ] AI
    ) where                                                                                         -- [ ] AI

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
getParsedModuleStale                                                                                -- [ ] AI
    :: MonadIO m                                                                                    -- [ ] AI
    => IdeState                                                                                     -- [ ] AI
    -> NormalizedFilePath                                                                           -- [ ] AI
    -> m (Maybe ParsedModule)                                                                       -- [ ] AI
getParsedModuleStale state nfp =                                                                    -- [ ] AI
    liftIO $ fmap fst <$>                                                                           -- [ ] AI
        runAction "rename.getParsedModuleStale" state                                               -- [ ] AI
            (useWithStale GetParsedModule nfp)                                                      -- [ ] AI

-- | Find the 'ImportAlias' for the alias declaration at the cursor, such as
-- @Alias@ in @import Module as Alias@.
findImportAliasDeclAtPos                                                                            -- [ ] AI
    :: Position                                                                                     -- [ ] AI
    -> [LImportDecl GhcPs]                                                                          -- [ ] AI
    -> Maybe ImportAlias                                                                            -- [ ] AI
findImportAliasDeclAtPos pos imports = listToMaybe                                                  -- [ ] AI
    [ ImportAlias {aliasModuleName, aliasName, aliasDeclSpan}                                       -- [ ] AI
    | _locatedImport@(L _ decl)      <- imports                                                     -- [ ] AI
    , Just locatedAlias              <- [ideclAs decl]                                              -- [ ] AI
    , let aliasName = unLoc locatedAlias                                                            -- [ ] AI
    , RealSrcSpan aliasDeclSpan _    <- [locA locatedAlias]                                         -- [ ] AI
    , rangeContainsPosition (realSrcSpanToRange aliasDeclSpan) pos                                  -- [ ] AI
    , let aliasModuleName = unLoc (ideclName decl)                                                  -- [ ] AI
    ]                                                                                               -- [ ] AI

-- | Find the 'ImportAlias' matching the name qualifier at the cursor, such as
-- @Alias@ in @Alias.name@.
-- Returns multiple values if multiple modules share the same alias.
findImportAliasUseAtPos                                                                             -- [ ] AI
    :: Position                                                                                     -- [ ] AI
    -> [LHsDecl GhcPs]                                                                              -- [ ] AI
    -> [LImportDecl GhcPs]                                                                          -- [ ] AI
    -> [ImportAlias]                                                                                -- [ ] AI
findImportAliasUseAtPos pos decls imports =                                                         -- [ ] AI
    case listToMaybe                                                                                -- [ ] AI
        [ qualifier                                                                                 -- [ ] AI
        | L (ann :: Anno RdrName) (Qual qualifier _) <- listify (const True) decls                  -- [ ] AI
        , RealSrcSpan useSiteSpan _  <- [locA ann]                                                  -- [ ] AI
        , rangeContainsPosition (realSrcSpanToRange useSiteSpan) pos                                -- [ ] AI
        , let qualifierLength = fromIntegral (length (moduleNameString qualifier))                  -- [ ] AI
              start           = realSrcSpanStart useSiteSpan                                        -- [ ] AI
              line            = fromIntegral (srcLocLine start)                                     -- [ ] AI
              startColumn     = fromIntegral (srcLocCol start)                                      -- [ ] AI
              qualifierRange  = Range                                                               -- [ ] AI
                  (Position (line - 1) (startColumn - 1))                                           -- [ ] AI
                  (Position (line - 1) (startColumn - 1 + qualifierLength))                         -- [ ] AI
        , rangeContainsPosition qualifierRange pos                                                  -- [ ] AI
        ] of                                                                                        -- [ ] AI
    Nothing -> []                                                                                   -- [ ] AI
    Just qualifierAtPos ->                                                                          -- [ ] AI
        [ ImportAlias {aliasModuleName, aliasName, aliasDeclSpan}                                   -- [ ] AI
        | _locatedImport@(L _ decl)   <- imports                                                    -- [ ] AI
        , Just locatedAlias           <- [ideclAs decl]                                             -- [ ] AI
        , let aliasName = unLoc locatedAlias                                                        -- [ ] AI
        , aliasName == qualifierAtPos                                                               -- [ ] AI
        , RealSrcSpan aliasDeclSpan _ <- [locA locatedAlias]                                        -- [ ] AI
        , let aliasModuleName = unLoc (ideclName decl)
        ]                                                                                           -- [ ] AI

-- | Return the module name and declaration span for the alias being renamed at
-- the cursor. The cursor may be on the alias token in an import declaration or
-- on a qualifier at a use site. If multiple imports share the same alias, falls
-- back to the typechecked module's 'GlobalRdrEnv' to disambiguate.
-- Returns @Nothing@ if the cursor is not on any alias declaration or qualifier.
resolveAliasAtPos                                                                                   -- [ ] AI
    :: MonadIO m                                                                                    -- [ ] AI
    => IdeState                                                                                     -- [ ] AI
    -> NormalizedFilePath                                                                           -- [ ] AI
    -> Position                                                                                     -- [ ] AI
    -> [LHsDecl GhcPs]                                                                              -- [ ] AI
    -> [LImportDecl GhcPs]                                                                          -- [ ] AI
    -> ExceptT PluginError m (Maybe ImportAlias)                                                    -- [ ] AI
resolveAliasAtPos state nfp pos decls imports =                                                     -- [ ] AI
    case findImportAliasDeclAtPos pos imports of                                                    -- [ ] AI
        Just result -> pure (Just result)                                                           -- [ ] AI
        Nothing     -> case findImportAliasUseAtPos pos decls imports of                            -- [ ] AI
            []       -> pure Nothing                                                                -- [ ] AI
            [result] -> pure (Just result)                                                          -- [ ] AI
            _many    -> disambiguateAliasAtPos state nfp pos imports                                -- [ ] AI

-- | Build a 'WorkspaceEdit' renaming an import alias and all its use sites.
aliasBasedRename                                                                                    -- [ ] AI
    :: MonadIO m                                                                                    -- [ ] AI
    => IdeState                                                                                     -- [ ] AI
    -> NormalizedFilePath                                                                           -- [ ] AI
    -> Uri                                                                                          -- [ ] AI
    -> ImportAlias                                                                                  -- [ ] AI
    -> [LImportDecl GhcPs]                                                                          -- [ ] AI
    -> [LHsDecl GhcPs]                                                                              -- [ ] AI
    -> T.Text                                                                                       -- [ ] AI
    -> ExceptT PluginError m (MessageResult Method_TextDocumentRename)                              -- [ ] AI
aliasBasedRename state nfp uri importAlias imports decls newNameText = do                           -- [ ] AI
    let oldAlias = aliasName importAlias                                                            -- [ ] AI
        declSpan = aliasDeclSpan importAlias                                                        -- [ ] AI
        duplicateAlias =                                                                            -- [ ] AI
            length [ ()                                                                             -- [ ] AI
                   | L _ decl <- imports                                                            -- [ ] AI
                   , Just locAlias <- [ideclAs decl]                                                -- [ ] AI
                   , unLoc locAlias == oldAlias                                                     -- [ ] AI
                   ] > 1                                                                            -- [ ] AI
    useSiteSpans <-                                                                                 -- [ ] AI
        if duplicateAlias                                                                           -- [ ] AI
        then importAliasUseSiteSpansDisambiguated state nfp importAlias decls                       -- [ ] AI
        else pure $ importAliasUseSiteSpans importAlias decls                                       -- [ ] AI
    let declEdit = importAliasDeclEdit newNameText declSpan                                         -- [ ] AI
        useEdits = map (importAliasUseSiteEdit oldAlias newNameText) useSiteSpans                   -- [ ] AI
        allEdits = declEdit : useEdits                                                              -- [ ] AI
    verTxtDocId <- liftIO $ runAction "rename.getVersionedTextDoc" state $                          -- [ ] AI
        getVersionedTextDoc (TextDocumentIdentifier uri)                                            -- [ ] AI
    let fileChanges = Just $ M.singleton (verTxtDocId ^. L.uri) allEdits                            -- [ ] AI
        -- TODO: Replace 'Nothing' with meaningful details for the workspace edit.
        workspaceEdit = WorkspaceEdit fileChanges Nothing Nothing                                   -- [ ] AI
    pure $ InL workspaceEdit                                                                        -- [ ] AI

-- | Collect the 'RealSrcSpan' of every qualified use of @oldAlias@, such as in
-- @oldAlias.foo@, @oldAlias.bar@, and so on.
-- Does not disambiguate if multiple imports share the alias.
importAliasUseSiteSpans                                                                             -- [ ] AI
    :: ImportAlias                                                                                  -- [ ] AI
    -> [LHsDecl GhcPs]                                                                              -- [ ] AI
    -> [RealSrcSpan]                                                                                -- [ ] AI
importAliasUseSiteSpans importAlias decls =                                                         -- [ ] AI
    [ fullNameSpan                                                                                  -- [ ] AI
    | L (ann :: Anno RdrName) (Qual moduleAlias _) <- listify (const True) decls                    -- [ ] AI
    , moduleAlias == aliasName importAlias                                                          -- [ ] AI
    , RealSrcSpan fullNameSpan _ <- [locA ann]                                                      -- [ ] AI
    ]                                                                                               -- [ ] AI

-- | Build a 'TextEdit' replacing the qualifier part in a qualified name (like
-- from @Alias.name@ to @NewAlias.name@).
-- (NOTE: GHC uses 1-based positioning; LSP uses 0-based.)
importAliasUseSiteEdit                                                                              -- [ ] AI
    :: ModuleName   -- ^ old alias, used to compute the qualifier width                             -- [ ] AI
    -> T.Text       -- ^ new alias text                                                             -- [ ] AI
    -> RealSrcSpan  -- ^ span of the full qualified name, such as @Alias.name@                      -- [ ] AI
    -> TextEdit                                                                                     -- [ ] AI
importAliasUseSiteEdit oldAlias newAlias fullNameSpan = TextEdit range newAlias                     -- [ ] AI
    where                                                                                           -- [ ] AI
        start    = realSrcSpanStart fullNameSpan                                                    -- [ ] AI
        line     = fromIntegral (srcLocLine start) - 1                                              -- [ ] AI
        startCol = fromIntegral (srcLocCol  start) - 1                                              -- [ ] AI
        endCol   = startCol + fromIntegral (length (moduleNameString oldAlias))                     -- [ ] AI
        range    = Range (Position line startCol) (Position line endCol)                            -- [ ] AI

-- | Build a 'TextEdit' replacing the alias token in an import declaration (like
-- from @import Module as Alias@ to @import Module as NewAlias@).
importAliasDeclEdit                                                                                 -- [ ] AI
    :: T.Text       -- ^ new alias text                                                             -- [ ] AI
    -> RealSrcSpan  -- ^ span of @Alias@ in @import Module as Alias@                                -- [ ] AI
    -> TextEdit                                                                                     -- [ ] AI
importAliasDeclEdit newAlias rsp = TextEdit (realSrcSpanToRange rsp) newAlias                       -- [ ] AI

-- | Check whether a range contains a position (inclusive start, exclusive end).
rangeContainsPosition :: Range -> Position -> Bool                                                  -- [ ] AI
rangeContainsPosition (Range (Position sl sc) (Position el ec)) (Position l c)                      -- [ ] AI
    =  (l > sl || (l == sl && c >= sc))                                                             -- [ ] AI
    && (l < el || (l == el && c <  ec))                                                             -- [ ] AI

---------------------------------------------------------------------------------------------------
-- Internal helpers

-- | Resolve an ambiguous alias use site by consulting the typechecked                              -- [ ] AI
-- module's 'GlobalRdrEnv'. Used when multiple imports share the same alias.                        -- [ ] AI
-- The caller is responsible for providing the names under the cursor.                              -- [ ] AI
-- TODO: Rename it to @disambiguateAliasUseAtPos@.
disambiguateAliasAtPos                                                                              -- [ ] AI
    :: MonadIO m                                                                                    -- [ ] AI
    => IdeState                                                                                     -- [ ] AI
    -> NormalizedFilePath                                                                           -- [ ] AI
    -> Position                                                                                     -- [ ] AI
    -> [LImportDecl GhcPs]                                                                          -- [ ] AI
    -> ExceptT PluginError m (Maybe ImportAlias)                                                    -- [ ] AI
disambiguateAliasAtPos state nfp pos imports = do                                                   -- [ ] AI
    namesAtCursor <- getNamesAtPos state nfp pos                                                    -- [ ] AI
    tcModule <- runActionE "rename.disambiguateAlias" state (useE TypeCheck nfp)                    -- [ ] AI
    let rdrEnv = tcg_rdr_env (tmrTypechecked tcModule)                                              -- [ ] AI
    pure $ listToMaybe $ do                                                                         -- [ ] AI
        name <- namesAtCursor                                                                       -- [ ] AI
        gre <- maybeToList (lookupGRE_Name rdrEnv name)                                             -- [ ] AI
        impSpec <- gre_imp gre                                                                      -- [ ] AI
        let declSpec = is_decl impSpec                                                              -- [ ] AI
            specModuleName = moduleName (is_mod declSpec)                                           -- [ ] AI
            specAlias = is_as declSpec                                                              -- [ ] AI
        L _ decl <- imports                                                                         -- [ ] AI
        guard (unLoc (ideclName decl) == specModuleName)                                            -- [ ] AI
        Just locatedAlias <- [ideclAs decl]                                                         -- [ ] AI
        RealSrcSpan aliasDeclSpan _ <- [locA locatedAlias]                                          -- [ ] AI
        pure (ImportAlias specModuleName specAlias aliasDeclSpan)                                   -- [ ] AI

-- | Like 'importAliasUseSiteSpans' but filters to use sites that resolve                           -- [ ] AI
-- to names from @actualMod@, using the typechecked module's 'GlobalRdrEnv'.                        -- [ ] AI
-- Used when multiple imports share the same alias.                                                 -- [ ] AI
importAliasUseSiteSpansDisambiguated                                                                -- [ ] AI
    :: MonadIO m                                                                                    -- [ ] AI
    => IdeState                                                                                     -- [ ] AI
    -> NormalizedFilePath                                                                           -- [ ] AI
    -> ImportAlias                                                                                  -- [ ] AI
    -> [LHsDecl GhcPs]                                                                              -- [ ] AI
    -> ExceptT PluginError m [RealSrcSpan]                                                          -- [ ] AI
importAliasUseSiteSpansDisambiguated state nfp importAlias decls = do                               -- [ ] AI
    tcModule <- runActionE "rename.useSiteSpans" state (useE TypeCheck nfp)                         -- [ ] AI
    let rdrEnv = tcg_rdr_env (tmrTypechecked tcModule)                                              -- [ ] AI
        allSpans = importAliasUseSiteSpansWithOcc (aliasName importAlias) decls                     -- [ ] AI
        ImportAlias actualMod _ _ = importAlias                                                     -- [ ] AI
    pure                                                                                            -- [ ] AI
        [ rsp                                                                                       -- [ ] AI
        | (occName, rsp) <- allSpans                                                                -- [ ] AI
        , gre <- lookupGRE rdrEnv $                                                                 -- [ ] AI
            LookupRdrName (Qual (aliasName importAlias) occName) AllRelevantGREs                    -- [ ] AI
        , impSpec <- gre_imp gre                                                                    -- [ ] AI
        , moduleName (is_mod (is_decl impSpec)) == actualMod                                        -- [ ] AI
        ]                                                                                           -- [ ] AI

-- | Like 'importAliasUseSiteSpans' but also returns the 'OccName' of each                          -- [ ] AI
-- use, needed for 'GlobalRdrEnv' lookup in the disambiguated path.                                 -- [ ] AI
importAliasUseSiteSpansWithOcc                                                                      -- [ ] AI
    :: ModuleName                                                                                   -- [ ] AI
    -> [LHsDecl GhcPs]                                                                              -- [ ] AI
    -> [(OccName, RealSrcSpan)]                                                                     -- [ ] AI
importAliasUseSiteSpansWithOcc oldAlias decls =                                                     -- [ ] AI
    [ (occName, rsp)                                                                                -- [ ] AI
    | L (ann :: Anno RdrName) (Qual moduleAlias occName) <- listify (const True) decls              -- [ ] AI
    , moduleAlias == oldAlias                                                                       -- [ ] AI
    , RealSrcSpan rsp _ <- [locA ann]                                                               -- [ ] AI
    ]                                                                                               -- [ ] AI
