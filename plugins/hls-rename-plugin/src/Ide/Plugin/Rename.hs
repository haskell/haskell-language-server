{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE LambdaCase        #-}

module Ide.Plugin.Rename (descriptor, Log) where

import           Control.Lens                          ((^.))
import           Control.Monad
import           Control.Monad.Except                  (ExceptT, throwError)
import           Control.Monad.IO.Class                (MonadIO, liftIO)
import           Control.Monad.Trans.Class             (lift)
import           Data.Either                           (rights)
import           Data.Foldable                         (fold)
import           Data.Generics
import           Data.Hashable
import           Data.HashSet                          (HashSet)
import qualified Data.HashSet                          as HS
import           Data.List.NonEmpty                    (NonEmpty ((:|)),
                                                        groupWith)
import qualified Data.Map                              as M
import           Data.Maybe
import           Data.Mod.Word
import qualified Data.Text                             as T
import           Development.IDE                       (Recorder, WithPriority,
                                                        usePropertyAction)
import           Development.IDE.Core.FileStore        (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service          hiding (Log)
import           Development.IDE.Core.Shake            hiding (Log)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.ExactPrint        hiding (Log)
import qualified Development.IDE.GHC.ExactPrint        as E
import           Development.IDE.Plugin.CodeAction
import           Development.IDE.Spans.AtPoint
import           Development.IDE.Types.Location
import           GHC.Iface.Ext.Types                   (HieAST (..),
                                                        HieASTs (..),
                                                        NodeOrigin (..),
                                                        SourcedNodeInfo (..))
import           GHC.Iface.Ext.Utils                   (generateReferencesMap)
import           HieDb                                 ((:.) (..))
import           HieDb.Query
import           HieDb.Types                           (RefRow (refIsGenerated))
import           Ide.Logger                            (Pretty (..),
                                                        cmapWithPrio)
import           Ide.Plugin.Error
import           Ide.Plugin.Properties
import qualified Ide.Plugin.Rename.ModuleName          as ModuleName
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens            as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import GHC.Parser.Annotation (SrcSpanAnnA, LocatedN)

instance Hashable (Mod a) where hash n = hash (unMod n)

data Log
    = LogExactPrint E.Log
    | LogModuleName ModuleName.Log

instance Pretty Log where
    pretty = \ case
        LogExactPrint msg -> pretty msg
        LogModuleName msg -> pretty msg

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId = mkExactprintPluginDescriptor exactPrintRecorder $
    (defaultPluginDescriptor pluginId "Provides renaming of Haskell identifiers and module names")
        { pluginHandlers = mconcat
              [ mkPluginHandler SMethod_TextDocumentRename renameProvider
              , mkPluginHandler SMethod_TextDocumentPrepareRename prepareRenameProvider
              , mkPluginHandler SMethod_TextDocumentCodeLens (ModuleName.codeLens moduleNameRecorder)
              ]
        , pluginCommands = [PluginCommand ModuleName.updateModuleNameCommand "Set name of module to match with file path" (ModuleName.command moduleNameRecorder)]
        , pluginConfigDescriptor = defaultConfigDescriptor
            { configCustomConfig = mkCustomConfig properties }
        }
    where
        exactPrintRecorder = cmapWithPrio LogExactPrint recorder
        moduleNameRecorder = cmapWithPrio LogModuleName recorder

prepareRenameProvider :: PluginMethodHandler IdeState Method_TextDocumentPrepareRename
prepareRenameProvider state _pluginId (PrepareRenameParams (TextDocumentIdentifier uri) pos _progressToken) = do
    nfp <- getNormalizedFilePathE uri
    -- Step 5 (design decision 1): try alias rename first; fall through to                          -- [x] AI
    -- name-based rename if the cursor is not on an alias token.                                    -- [x] AI
    maybeParsed <- getParsedModuleStale state nfp                                                   -- [x] AI
    case maybeParsed of                                                                             -- [x] AI
        Nothing -> throwError $ PluginInternalError                                                 -- [x] AI
            "Cannot rename: HLS has not yet parsed this module. Please wait for indexing to complete and try again."  -- [x] AI
        Just parsed -> do                                                                           -- [x] AI
            let hsModule = unLoc $ pm_parsed_source parsed                                          -- [x] AI
                imports  = hsmodImports hsModule                                                    -- [x] AI
            case findImportAliasDeclAtPos pos imports of                                            -- [x] AI
                Just _ ->                                                                           -- [x] AI
                    -- Step 5 (design decision 2): return defaultBehavior.                          -- [x] AI
                    -- TODO: return an explicit range and placeholder text                          -- [x] AI
                    -- (the InL and InR (InL ...) variants of                                       -- [x] AI
                    -- PrepareRenameResult) so the client highlights exactly                        -- [x] AI
                    -- the alias token and pre-fills the current alias text                         -- [x] AI
                    -- in the rename box, rather than relying on                                    -- [x] AI
                    -- defaultBehavior.                                                             -- [x] AI
                    pure $ InL $ PrepareRenameResult $ InR $ InR $                                  -- [x] AI
                        PrepareRenameDefaultBehavior True                                           -- [x] AI
                Nothing -> do                                                                       -- [x] AI
                    -- Fall through to name-based rename.                                           -- [x] AI
                    --
                    -- When this handler says that rename is invalid, VSCode shows "The element can't be renamed"
                    -- and doesn't even allow you to create full rename request.
                    -- This handler deliberately approximates "things that definitely can't be renamed"
                    -- to mean "there is no Name at given position".
                    --
                    -- In particular it allows some cases through (e.g. cross-module renames),
                    -- so that the full rename handler can give more informative error about them.
                    namesUnderCursor <- getNamesAtPos state nfp pos
                    let renameValid = not $ null namesUnderCursor
                    pure $ InL $ PrepareRenameResult $ InR $ InR $
                        PrepareRenameDefaultBehavior renameValid

renameProvider :: PluginMethodHandler IdeState Method_TextDocumentRename
renameProvider state pluginId (RenameParams _prog (TextDocumentIdentifier uri) pos newNameText) = do
    nfp <- getNormalizedFilePathE uri
    -- Step 5 (design decision 1): try alias rename first; fall through to                          -- [x] AI
    -- name-based rename if the cursor is not on an alias token.                                    -- [x] AI
    maybeParsed <- getParsedModuleStale state nfp                                                   -- [x] AI
    case maybeParsed of                                                                             -- [x] AI
        Nothing -> throwError $ PluginInternalError                                                 -- [x] AI
            "Cannot rename: HLS has not yet parsed this module. Please wait for indexing to complete and try again."  -- [x] AI
        Just parsed -> do                                                                           -- [x] AI
            let hsModule = unLoc $ pm_parsed_source parsed                                          -- [x] AI
                imports  = hsmodImports hsModule                                                    -- [x] AI
                decls    = hsmodDecls hsModule                                                      -- [x] AI
            case findImportAliasDeclAtPos pos imports of                                            -- [x] AI
                Just (oldAlias, aliasSpan) ->                                                       -- [x] AI
                    aliasBasedRename state uri oldAlias aliasSpan decls newNameText                 -- [x] AI
                Nothing ->                                                                          -- [x] AI
                    -- Fall through to name-based rename.                                           -- [x] AI
                    nameBasedRename state pluginId nfp pos newNameText                              -- [x] AI

-- | Alias-based rename: build 'TextEdit's for the import alias declaration                         -- [x] AI
-- and all use sites, then wrap in a 'WorkspaceEdit'.                                               -- [x] AI
aliasBasedRename ::                                                                                 -- [x] AI
    MonadIO m =>                                                                                    -- [x] AI
    IdeState ->                                                                                     -- [x] AI
    Uri ->                                                                                          -- [x] AI
    ModuleName ->                                                                                   -- [x] AI
    RealSrcSpan ->                                                                                  -- [x] AI
    [LHsDecl GhcPs] ->                                                                              -- [x] AI
    T.Text ->                                                                                       -- [x] AI
    ExceptT PluginError m (MessageResult Method_TextDocumentRename)                                 -- [x] AI
aliasBasedRename state uri oldAlias aliasSpan decls newNameText = do                                -- [x] AI
    let useSiteSpans  = importAliasUseSiteSpans oldAlias decls                                      -- [x] AI
        declEdit      = importAliasDeclEdit newNameText aliasSpan                                   -- [x] AI
        useEdits      = map (importAliasUseSiteEdit oldAlias newNameText) useSiteSpans              -- [x] AI
        allEdits      = declEdit : useEdits                                                         -- [x] AI
    verTxtDocId <- liftIO $ runAction "rename: getVersionedTextDoc" state $                         -- [x] AI
        getVersionedTextDoc (TextDocumentIdentifier uri)                                            -- [x] AI
    let fileChanges   = Just $ M.singleton (verTxtDocId ^. L.uri) allEdits                          -- [x] AI
        -- TODO: Replace 'Nothing' with meaningful details for the workspace edit.
        workspaceEdit = WorkspaceEdit fileChanges Nothing Nothing                                   -- [x] AI
    pure $ InL workspaceEdit                                                                        -- [x] AI

-- | Name-based rename: the original renameProvider logic, extracted so the                         -- [x] AI
-- alias branch can fall through to it cleanly.                                                     -- [x] AI
nameBasedRename ::                                                                                  -- [x] AI
    IdeState ->                                                                                     -- [x] AI
    PluginId ->                                                                                     -- [x] AI
    NormalizedFilePath ->                                                                           -- [x] AI
    Position ->                                                                                     -- [x] AI
    T.Text ->                                                                                       -- [x] AI
    ExceptT PluginError (HandlerM config) (MessageResult Method_TextDocumentRename)                 -- [x] AI
nameBasedRename state pluginId nfp pos newNameText = do                                             -- [x] AI
    directOldNames <- getNamesAtPos state nfp pos
    directRefs <- concat <$> mapM (refsAtName state nfp) directOldNames

    {- References in HieDB are not necessarily transitive. With `NamedFieldPuns`, we can have
        indirect references through punned names. To find the transitive closure, we do a pass of
        the direct references to find the references for any punned names.
        See the `IndirectPuns` test for an example. -}
    indirectOldNames <- concat . filter ((>1) . length) <$>
        mapM (uncurry (getNamesAtPos state) <=< locToFilePos) directRefs
    let oldNames = filter matchesDirect indirectOldNames ++ directOldNames
           where
             matchesDirect n = occNameFS (nameOccName n) `elem` directFS
             directFS = map (occNameFS . nameOccName) directOldNames

    case oldNames of
        -- There were no Names at given position (e.g. rename triggered within a comment or on a keyword)
        [] -> throwError $ PluginInvalidParams "No symbol to rename at given position"
        _  -> do
            refs <- HS.fromList . concat <$> mapM (refsAtName state nfp) oldNames

            -- Validate rename
            crossModuleEnabled <- liftIO $ runAction "rename: config" state $ usePropertyAction #crossModule pluginId properties
            unless crossModuleEnabled $ failWhenImportOrExport state nfp refs oldNames
            when (any isBuiltInSyntax oldNames) $ throwError $ PluginInternalError "Invalid rename of built-in syntax"

            -- Perform rename
            let newName = mkTcOcc $ T.unpack newNameText
                filesRefs = collectWith locToUri refs
                getFileEdit (uri, locations) = do
                    verTxtDocId <- liftIO $ runAction "rename: getVersionedTextDoc" state $ getVersionedTextDoc (TextDocumentIdentifier uri)
                    getSrcEdit state verTxtDocId (replaceRefs newName locations)
            fileEdits <- mapM getFileEdit filesRefs
            pure $ InL $ fold fileEdits

-- | Limit renaming across modules.
failWhenImportOrExport ::
    IdeState ->
    NormalizedFilePath ->
    HashSet Location ->
    [Name] ->
    ExceptT PluginError (HandlerM config) ()
failWhenImportOrExport state nfp refLocs names = do
    pm <- runActionE "Rename.GetParsedModule" state
         (useE GetParsedModule nfp)
    let hsMod = unLoc $ pm_parsed_source pm
    case (unLoc <$> hsmodName hsMod, hsmodExports hsMod) of
        (mbModName, _) | not $ any (\n -> nameIsLocalOrFrom (replaceModName n mbModName) n) names
            -> throwError $ PluginInternalError "Renaming of an imported name is unsupported"
        (_, Just (L _ exports)) | any ((`HS.member` refLocs) . unsafeSrcSpanToLoc . getLoc) exports
            -> throwError $ PluginInternalError "Renaming of an exported name is unsupported"
        (Just _, Nothing) -> throwError $ PluginInternalError "Explicit export list required for renaming"
        _ -> pure ()

---------------------------------------------------------------------------------------------------
-- Qualified alias renaming                                                                       -- [x] AI
--                                                                                                -- [x] AI
-- Step 1: fetch the parsed AST via GetParsedModule.                                              -- [x] AI
--                                                                                                -- [x] AI
-- Import aliases (e.g. `import Data.List as L`) survive only in the parsed (`GhcPs`) AST.        -- [x] AI
-- They are erased during resolving, so the HIE AST cannot be used to locate or replace them.     -- [x] AI
-- The helper below fetches the parsed module using `useWithStale` so it never blocks             -- [x] AI
-- the UI while GHC is still loading.                                                             -- [x] AI
--                                                                                                -- [x] AI
-- Steps 2-5 (finding the alias, collecting use sites, building edits) will be                    -- [x] AI
-- added in subsequent iterations.                                                                -- [x] AI

-- | Fetch the parsed module for a file, accepting a possibly stale result.                       -- [x] AI
-- Returns @Nothing@ if the file has not yet been indexed at all.                                 -- [x] AI
-- TODO: Handle the @Nothing@ case.
getParsedModuleStale ::                                                                           -- [x] AI
    MonadIO m =>                                                                                  -- [x] AI
    IdeState ->                                                                                   -- [x] AI
    NormalizedFilePath ->                                                                         -- [x] AI
    m (Maybe ParsedModule)                                                                        -- [x] AI
getParsedModuleStale state nfp =                                                                  -- [x] AI
    liftIO $ fmap fst <$>                                                                         -- [x] AI
        runAction "rename.getParsedModuleStale" state                                             -- [x] AI
            (useWithStale GetParsedModule nfp)                                                    -- [x] AI

-- Step 2: find the import declaration whose alias span contains the cursor.                      -- [x] AI
--                                                                                                -- [x] AI
-- We traverse `hsmodImports` looking for an @import M as Alias@ declaration                      -- [x] AI
-- where the cursor position falls inside the @Alias@ token. If found, we                         -- [x] AI
-- return the alias name and its source span for use in steps 4 and 5.                            -- [x] AI

-- | Given a cursor position that falls on the @Alias@ token in an                                -- [x] AI
-- @import M as Alias@ declaration (not on a use site such as @Alias.foo@),                       -- [x] AI
-- return the alias 'ModuleName' and the 'RealSrcSpan' of that token.                             -- [x] AI
-- Returns 'Nothing' if no import alias covers the cursor position.                               -- [x] AI
-- Multiple imports of the same module with different aliases are handled                         -- [x] AI
-- correctly because we match on the cursor position, not the module name.                        -- [x] AI
findImportAliasAtPos                                                                              -- [x] AI
    :: Position                                                                                   -- [x] AI
    -> [LImportDecl GhcPs]                                                                        -- [x] AI
    -> Maybe (ModuleName, RealSrcSpan)                                                            -- [x] AI
findImportAliasAtPos pos imports = listToMaybe                                                    -- [x] AI
    [ (aliasName, rsp)                                                                            -- [x] AI
    | _locatedImport@(L _ decl) <- imports                                                        -- [x] AI
    , Just locatedAlias         <- [ideclAs decl]                                                 -- [x] AI
    , let aliasName = unLoc locatedAlias                                                          -- [x] AI
    , RealSrcSpan rsp _         <- [locA locatedAlias]                                            -- [x] AI
    , rangeContainsPosition (realSrcSpanToRange rsp) pos                                          -- [x] AI
    ]                                                                                             -- [x] AI

-- | Check whether a 'Range' contains a 'Position'                                                -- [x] AI
-- (inclusive start, exclusive end).                                                              -- [x] AI
rangeContainsPosition :: Range -> Position -> Bool                                                -- [x] AI
rangeContainsPosition (Range (Position sl sc) (Position el ec)) (Position l c)                    -- [x] AI
    =  (l > sl || (l == sl && c >= sc))                                                           -- [x] AI
    && (l < el || (l == el && c <  ec))                                                           -- [x] AI

-- Step 3: collect use-site spans for every `Qual oldAlias _` RdrName.                            -- [x] AI
--                                                                                                -- [x] AI
-- We use SYB's `listify` to collect all located RdrName nodes anywhere in                        -- [x] AI
-- the declaration list, then filter to those whose qualifier matches the                         -- [x] AI
-- target alias. Each matching node contributes its RealSrcSpan.                                  -- [x] AI

-- | Collect the 'RealSrcSpan' of every qualified use of @oldAlias@ in the                        -- [x] AI
-- given declarations, e.g. every occurrence of @L.foo@, @L.bar@, etc.                            -- [x] AI
-- Uses SYB 'listify' to traverse the full 'GhcPs' AST.                                           -- [x] AI
importAliasUseSiteSpans                                                                           -- [x] AI
    :: ModuleName                                                                                 -- [x] AI
    -> [LHsDecl GhcPs]                                                                            -- [x] AI
    -> [RealSrcSpan]                                                                              -- [x] AI
importAliasUseSiteSpans oldAlias decls =                                                          -- [x] AI
    [ rsp                                                                                         -- [x] AI
    | L (ann :: Anno RdrName) (Qual moduleAlias _) <- listify (const True) decls                  -- [x] AI
    , moduleAlias == oldAlias                                                                     -- [x] AI
    , RealSrcSpan rsp _ <- [locA ann]                                                             -- [x] AI
    ]                                                                                             -- [x] AI

-- Step 4: build TextEdits — one for the import alias declaration and one                         -- [x] AI
-- for each qualifier at a use site.                                                              -- [x] AI

-- The two helpers below are kept separate                                                        -- [x] AI
-- because the span arithmetic differs: use-site spans cover the full                             -- [x] AI
-- qualified name (e.g. @L.foo@) so we must truncate to the qualifier width,                      -- [x] AI
-- whereas import spans already cover exactly the alias token.                                    -- [x] AI

-- | Build a 'TextEdit' that replaces the qualifier portion of a use site.                        -- [x] AI
-- The span covers only the alias (e.g. the @L@ in @L.foo@), not the dot                          -- [x] AI
-- or the following name. Column arithmetic uses GHC's 1-based source                             -- [x] AI
-- locations and converts to the 0-based LSP 'Position' convention.                               -- [x] AI
importAliasUseSiteEdit                                                                            -- [x] AI
    :: ModuleName    -- ^ old alias, used to compute the qualifier width                          -- [x] AI
    -> T.Text        -- ^ new alias text                                                          -- [x] AI
    -> RealSrcSpan   -- ^ span of the full qualified name, e.g. @L.foo@                           -- [x] AI
    -> TextEdit                                                                                   -- [x] AI
importAliasUseSiteEdit oldAlias newAlias rsp = TextEdit range newAlias                            -- [x] AI
  where                                                                                           -- [x] AI
    start    = realSrcSpanStart rsp                                                               -- [x] AI
    line     = fromIntegral (srcLocLine start) - 1                                                -- [x] AI
    startCol = fromIntegral (srcLocCol  start) - 1                                                -- [x] AI
    -- The qualifier occupies exactly as many characters as the alias string.                     -- [x] AI
    -- The dot is at startCol + width and is not included in the edit.                            -- [x] AI
    endCol   = startCol + fromIntegral (length (moduleNameString oldAlias))                       -- [x] AI
    range    = Range (Position line startCol) (Position line endCol)                              -- [x] AI

-- | Build a 'TextEdit' that replaces the alias token in an                                       -- [x] AI
-- @import M as Alias@ declaration.                                                               -- [x] AI
-- The span is taken directly from 'ideclAs' and already covers exactly                           -- [x] AI
-- the alias token, so no column arithmetic is needed.                                            -- [x] AI
importAliasDeclEdit                                                                               -- [x] AI
    :: T.Text        -- ^ new alias text                                                          -- [x] AI
    -> RealSrcSpan   -- ^ span of @Alias@ in @import M as Alias@                                  -- [x] AI
    -> TextEdit                                                                                   -- [x] AI
importAliasDeclEdit newAlias rsp = TextEdit (realSrcSpanToRange rsp) newAlias                     -- [x] AI

-- Step 5: assemble WorkspaceEdit and wire into prepareRenameProvider and                         -- [x] AI
-- renameProvider.                                                                                -- [x] AI
--                                                                                                -- [x] AI
-- Both handlers share the same structure: fetch the parsed module, check                         -- [x] AI
-- whether the cursor is on an alias token, and either take the alias path                        -- [x] AI
-- or fall through to name-based rename.                                                          -- [x] AI
--                                                                                                -- [x] AI
-- In renameProvider, the alias path delegates to aliasBasedRename, which                         -- [x] AI
-- collects use-site spans via importAliasUseSiteSpans, builds a TextEdit for the                 -- [x] AI
-- import declaration via importAliasDeclEdit and one per use site via                            -- [x] AI
-- importAliasUseSiteEdit, and wraps them all in a WorkspaceEdit. The name-based                  -- [x] AI
-- fallthrough delegates to nameBasedRename.                                                      -- [x] AI
--                                                                                                -- [x] AI
-- In prepareRenameProvider, the alias path returns                                               -- [x] AI
-- PrepareRenameDefaultBehavior True if an alias is found. If not, the name-based fallthrough     -- [x] AI
-- returns PrepareRenameDefaultBehavior True or False depending on whether                        -- [x] AI
-- getNamesAtPos finds any Names at the cursor.                                                   -- [x] AI
--                                                                                                -- [x] AI
-- Design decision 1 — entry point: a branch inside the existing handlers                         -- [x] AI
-- rather than a separate handler. HLS only registers one rename handler                          -- [x] AI
-- per plugin, so a separate handler is not viable. The alias branch is                           -- [x] AI
-- checked first in both handlers; if the cursor is not on an alias token,                        -- [x] AI
-- we fall through to name-based rename.                                                          -- [x] AI
--                                                                                                -- [x] AI
-- Design decision 2 — prepareRenameProvider return value: the alias branch                       -- [x] AI
-- returns PrepareRenameDefaultBehavior True, consistent with the existing                        -- [x] AI
-- handler. PrepareRenameResult also supports returning an explicit range                         -- [x] AI
-- and placeholder text (the InL and InR (InL ...) variants), which would                         -- [x] AI
-- let the client highlight exactly the alias token and pre-fill the current                      -- [x] AI
-- alias text in the rename box.                                                                  -- [x] AI
-- TODO: use those variants for a better UX.                                                      -- [x] AI
--                                                                                                -- [x] AI
-- Design decision 3 — getParsedModuleStale returns Nothing: fail early                           -- [x] AI
-- with an informative error. Falling through to name-based rename is not a                       -- [x] AI
-- real fallback since that path also requires a typechecked module and will                      -- [x] AI
-- fail anyway. Nothing only occurs if the file has never been successfully                       -- [x] AI
-- parsed.                                                                                        -- [x] AI

---------------------------------------------------------------------------------------------------
-- Source renaming

-- | Apply a function to a `ParsedSource` for a given `Uri` to compute a `WorkspaceEdit`.
getSrcEdit ::
    IdeState ->
    VersionedTextDocumentIdentifier ->
    (ParsedSource -> ParsedSource) ->
    ExceptT PluginError (HandlerM config) WorkspaceEdit
getSrcEdit state verTxtDocId updatePs = do
    ccs <- lift pluginGetClientCapabilities
    nfp <- getNormalizedFilePathE (verTxtDocId ^. L.uri)
    annAst <- runActionE "Rename.GetAnnotatedParsedSource" state
        (useE GetAnnotatedParsedSource nfp)
    let ps = annAst
        src = T.pack $ exactPrint ps
        res = T.pack $ exactPrint (updatePs ps)
    pure $ diffText ccs (verTxtDocId, src) res IncludeDeletions

-- | Replace names at every given `Location` (in a given `ParsedSource`) with a given new name.
replaceRefs ::
    OccName ->
    HashSet Location ->
    ParsedSource ->
    ParsedSource
replaceRefs newName refs = everywhere $
    -- there has to be a better way...
    mkT (replaceLoc @AnnListItem) `extT`
    -- replaceLoc @AnnList `extT` -- not needed
    -- replaceLoc @AnnParen `extT` -- not needed
    -- replaceLoc @AnnPragma `extT` -- not needed
    -- replaceLoc @AnnContext `extT` -- not needed
    -- replaceLoc @NoEpAnns `extT` -- not needed
    replaceLoc @NameAnn
    where
        replaceLoc :: forall an. LocatedAn an RdrName -> LocatedAn an RdrName
        replaceLoc (L srcSpan oldRdrName)
            | isRef (locA srcSpan) = L srcSpan $ replace oldRdrName
        replaceLoc lOldRdrName = lOldRdrName
        replace :: RdrName -> RdrName
        replace (Qual modName _) = Qual modName newName
        replace _                = Unqual newName

        isRef :: SrcSpan -> Bool
        isRef = (`HS.member` refs) . unsafeSrcSpanToLoc

---------------------------------------------------------------------------------------------------
-- Reference finding

-- | Note: We only find exact name occurrences (i.e. type reference "depth" is 0).
refsAtName ::
    MonadIO m =>
    IdeState ->
    NormalizedFilePath ->
    Name ->
    ExceptT PluginError m [Location]
refsAtName state nfp name = do
    ShakeExtras{withHieDb} <- liftIO $ runAction "Rename.HieDb" state getShakeExtras
    ast <- handleGetHieAst state nfp
    dbRefs <- case nameModule_maybe name of
        Nothing -> pure []
        Just mod -> liftIO $ mapMaybe rowToLoc <$> withHieDb (\hieDb ->
            -- See Note [Generated references]
            filter (\(refRow HieDb.:. _) -> refIsGenerated refRow) <$>
            findReferences
                hieDb
                True
                (nameOccName name)
                (Just $ moduleName mod)
                (Just $ moduleUnit mod)
                [fromNormalizedFilePath nfp]
            )
    pure $ nameLocs name ast ++ dbRefs

nameLocs :: Name -> HieAstResult -> [Location]
nameLocs name (HAR _ _ rm _ _) =
    concatMap (map (realSrcSpanToLocation . fst))
              (M.lookup (Right name) rm)

---------------------------------------------------------------------------------------------------
-- Util

getNamesAtPos :: MonadIO m => IdeState -> NormalizedFilePath -> Position -> ExceptT PluginError m [Name]
getNamesAtPos state nfp pos = do
    HAR{hieAst} <- handleGetHieAst state nfp
    pure $ getNamesAtPoint' hieAst pos

handleGetHieAst ::
    MonadIO m =>
    IdeState ->
    NormalizedFilePath ->
    ExceptT PluginError m HieAstResult
handleGetHieAst state nfp =
    -- We explicitly do not want to allow a stale version here - we only want to rename if
    -- the module compiles, otherwise we can't guarantee that we'll rename everything,
    -- which is bad (see https://github.com/haskell/haskell-language-server/issues/3799)
    fmap removeGenerated $ runActionE "Rename.GetHieAst" state $ useE GetHieAst nfp

{- Note [Generated references]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
GHC inserts `Use`s of record constructor everywhere where its record selectors are used,
which leads to record fields being renamed whenever corresponding constructor is renamed.
see https://github.com/haskell/haskell-language-server/issues/2915
To work around this, we filter out compiler-generated references.
-}
removeGenerated :: HieAstResult -> HieAstResult
removeGenerated HAR{..} =
    HAR{hieAst = sourceOnlyAsts, refMap = sourceOnlyRefMap, ..}
  where
    goAsts :: HieASTs a -> HieASTs a
    goAsts (HieASTs asts) = HieASTs (fmap goAst asts)

    goAst :: HieAST a -> HieAST a
    goAst (Node (SourcedNodeInfo sniMap) sp children) =
        let sourceOnlyNodeInfos = SourcedNodeInfo $ M.delete GeneratedInfo sniMap
        in Node sourceOnlyNodeInfos sp $ map goAst children

    sourceOnlyAsts = goAsts hieAst
    -- Also need to regenerate the RefMap, because the one in HAR
    -- is generated from HieASTs containing GeneratedInfo
    sourceOnlyRefMap = generateReferencesMap $ getAsts sourceOnlyAsts

collectWith :: (Hashable a, Eq b) => (a -> b) -> HashSet a -> [(b, HashSet a)]
collectWith f = map (\(a :| as) -> (f a, HS.fromList (a:as))) . groupWith f . HS.toList

-- | A variant 'getNamesAtPoint' that does not expect a 'PositionMapping'
getNamesAtPoint' :: HieASTs a -> Position -> [Name]
getNamesAtPoint' hf pos =
  concat $ pointCommand hf pos (rights . M.keys . getNodeIds)

locToUri :: Location -> Uri
locToUri (Location uri _) = uri

unsafeSrcSpanToLoc :: SrcSpan -> Location
unsafeSrcSpanToLoc srcSpan =
    case srcSpanToLocation srcSpan of
        Nothing       -> error "Invalid conversion from UnhelpfulSpan to Location"
        Just location -> location

locToFilePos :: Monad m => Location -> ExceptT PluginError m (NormalizedFilePath, Position)
locToFilePos (Location uri (Range pos _)) = (,pos) <$> getNormalizedFilePathE uri

replaceModName :: Name -> Maybe ModuleName -> Module
replaceModName name mbModName =
    mkModule (moduleUnit $ nameModule name) (fromMaybe (mkModuleName "Main") mbModName)

---------------------------------------------------------------------------------------------------
-- Config

properties :: Properties '[ 'PropertyKey "crossModule" 'TBoolean]
properties = emptyProperties
  & defineBooleanProperty #crossModule
    "Enable experimental cross-module renaming" True
