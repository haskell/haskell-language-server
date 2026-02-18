{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Ide.Plugin.Rename (descriptor, E.Log) where

import           Control.Lens                          ((^.))
import           Control.Monad
import           Control.Monad.Except                  (ExceptT, MonadError,
                                                        throwError)
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
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.ExactPrint
import qualified Development.IDE.GHC.ExactPrint        as E
import           Development.IDE.Plugin.CodeAction
import           Development.IDE.Spans.AtPoint
import           Development.IDE.Types.Location
import           GHC                                   (isGoodSrcSpan)
import           GHC.Iface.Ext.Types                   (HieAST (..),
                                                        HieASTs (..),
                                                        NodeOrigin (..),
                                                        SourcedNodeInfo (..))
import           GHC.Iface.Ext.Utils                   (generateReferencesMap)
import           HieDb.Query
import           Ide.Plugin.Error
import           Ide.Plugin.Properties
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens            as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
#if MIN_VERSION_ghc(9,8,0)
import qualified GHC.Types.Name.Occurrence             as OccName
#endif
instance Hashable (Mod a) where hash n = hash (unMod n)

descriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId = mkExactprintPluginDescriptor recorder $
    (defaultPluginDescriptor pluginId "Provides renaming of Haskell identifiers")
        { pluginHandlers = mconcat
              [ mkPluginHandler SMethod_TextDocumentRename renameProvider
              , mkPluginHandler SMethod_TextDocumentPrepareRename prepareRenameProvider
              ]
        , pluginConfigDescriptor = defaultConfigDescriptor
            { configCustomConfig = mkCustomConfig properties }
        }

prepareRenameProvider :: PluginMethodHandler IdeState Method_TextDocumentPrepareRename
prepareRenameProvider state _pluginId (PrepareRenameParams (TextDocumentIdentifier uri) pos _progressToken) = do
    nfp <- getNormalizedFilePathE uri
    namesUnderCursor <- getNamesAtPos state nfp pos
    -- When this handler says that rename is invalid, VSCode shows "The element can't be renamed"
    -- and doesn't even allow you to create full rename request.
    -- This handler deliberately approximates "things that definitely can't be renamed"
    -- to mean "there is no Name at given position".
    --
    -- In particular it allows some cases through (e.g. cross-module renames),
    -- so that the full rename handler can give more informative error about them.
    let renameValid = not $ null namesUnderCursor
    pure $ InL $ PrepareRenameResult $ InR $ InR $ PrepareRenameDefaultBehavior renameValid

renameProvider :: PluginMethodHandler IdeState Method_TextDocumentRename
renameProvider state pluginId (RenameParams _prog (TextDocumentIdentifier uri) pos newNameText) = do
    nfp <- getNormalizedFilePathE uri
    crossModuleEnabled <- liftIO $ runAction "rename: config" state $ usePropertyAction #crossModule pluginId properties
    pm <- runActionE "Rename.GetParsedModule" state (useE GetParsedModule nfp)
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
            refs' <- HS.fromList . concat <$> mapM (refsAtName state nfp) oldNames
            exportRefs <- exportNameLocs pm oldNames
            isExported <- or <$> mapM (isNameExplicitExported pm) oldNames
            let refs = HS.union refs' (HS.fromList exportRefs)
                currentModule = fmap unLoc $ hsmodName $ unLoc $ pm_parsed_source pm
                isLocallyDefined name =
                    case (nameModule_maybe name, currentModule) of
                        (Just nameModule, Just curMod) -> moduleName nameModule == curMod
                        -- No module means local
                        (Nothing, _) -> True
                        -- Has module but current has none = not local
                        (Just _, Nothing) -> False
                renamingLocalDeclaration = not (null directOldNames) && not (null oldNames) && all isLocallyDefined oldNames

            -- We have to show CrossModule Disabled error ONLY when
            -- 1. CrossModule is Disabled
            -- 2. User Tries to rename Exported variable
            -- We still allow local variable renaming in Disabled CrossModule mode.
            when (not crossModuleEnabled && ((not renamingLocalDeclaration) || isExported)) $ throwError $ PluginInternalError "Cross-module rename is disabled."

            -- if CrossModule renaming requires Explicit Export list
            -- if variable is imported somewhere else && No explicit export => ERROR
            -- if variable is locally used => No ERROR
            let hasExplicitExportList = isJust (hsmodExports (unLoc $ pm_parsed_source pm))
            refFiles <- forM (HS.toList refs) $ \loc -> do
                (file, _) <- locToFilePos loc
                pure file
            let hasExternalRefs = any (/= nfp) refFiles
            when ( crossModuleEnabled && not hasExplicitExportList && hasExternalRefs && renamingLocalDeclaration ) $ throwError $ PluginInvalidParams
                "Cannot rename symbol: module has no explicit export list and the symbol is referenced from other modules."

            -- Validate rename
            -- Indirect names are assumed safe once the direct ones are
            when (any isBuiltInSyntax oldNames) $ throwError $ PluginInternalError "Invalid rename of built-in syntax"

            -- Perform rename
            let newName = mkTcOcc $ T.unpack newNameText
                filesRefs = collectWith locToUri refs
#if MIN_VERSION_ghc(9,8,0)
            -- GHC 9.8+ stores field labels and their selectors in different OccName
            -- namespaces, breaking equality checks. Expand to cover both variants.
                oldOccNames = HS.fromList $ concatMap (expandOcc . nameOccName) oldNames
                  where
                    expandOcc occ
                        | occNameSpace occ == OccName.varName       = [occ, mkOccNameFS (fieldName (occNameFS occ)) (occNameFS occ)]
                        | isFieldNameSpace (occNameSpace occ)       = [occ, mkOccNameFS OccName.varName (occNameFS occ)]
                        | otherwise                                 = [occ]
#else
                oldOccNames = HS.fromList $ map nameOccName oldNames
#endif
                getFileEdit (uri, locations) = do
                    verTxtDocId <- liftIO $ runAction "rename: getVersionedTextDoc" state $ getVersionedTextDoc (TextDocumentIdentifier uri)
                    getSrcEdit state verTxtDocId (replaceRefs newName locations oldOccNames)
            fileEdits <- mapM getFileEdit filesRefs
            pure $ InL $ fold fileEdits

-- | Check if a name is exported from the module
-- Crossmodule Renaming happens only if names are Explicit Exported
isNameExplicitExported ::
    Monad m =>
    ParsedModule ->
    Name ->
    ExceptT PluginError m Bool
isNameExplicitExported pm name = do
    let hsMod = unLoc $ pm_parsed_source pm

    case hsmodExports hsMod of
        Nothing -> pure False
        Just exports -> do
            let exportedOccNames = getExportedOccNames exports
                nameOcc = nameOccName name
            pure $ nameOcc `elem` exportedOccNames

-- | Extract all OccNames from an export list
getExportedOccNames ::
    XRec GhcPs [LIE GhcPs] ->
    [OccName]
getExportedOccNames exports =
    concatMap extractFromExport (unLoc exports)
  where
    extractFromExport ::
        LIE GhcPs ->
        [OccName]
    extractFromExport lie = case unLocA lie of
#if MIN_VERSION_ghc(9,10,0)
        IEVar _ ieWrapped _           -> handle ieWrapped
        IEThingAbs _ ieWrapped _      -> handle ieWrapped
        IEThingAll _ ieWrapped _      -> handle ieWrapped
        IEThingWith _ ieWrapped _ _ _ -> handle ieWrapped
#else
        IEVar _ ieWrapped             -> handle ieWrapped
        IEThingAbs _ ieWrapped        -> handle ieWrapped
        IEThingAll _ ieWrapped        -> handle ieWrapped
        IEThingWith _ ieWrapped _ _   -> handle ieWrapped
#endif
        IEModuleContents{}            -> []
        _                             -> []
        where
            handle ieWrapped = maybeToList $ fmap rdrNameOcc $ unwrapIEWrappedName (unLoc ieWrapped)
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
    HashSet OccName ->
    ParsedSource ->
    ParsedSource
replaceRefs newName refs oldOccNames = everywhere $
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
            | isRef (locA srcSpan)
            , isTarget oldRdrName
            = L srcSpan $ replace oldRdrName
        replaceLoc lOldRdrName = lOldRdrName

        replace :: RdrName -> RdrName
        replace (Qual modName _) = Qual modName newName
        replace _                = Unqual newName

        isRef :: SrcSpan -> Bool
        isRef srcSpan = case srcSpanToLocation srcSpan of
            Just loc -> loc `HS.member` refs
            Nothing  -> False

        -- Only replace RdrNames whose OccName matches a rename target, preventing
        -- co-located field selectors from being incorrectly renamed.
        isTarget :: RdrName -> Bool
        isTarget rdrName = rdrNameOcc rdrName `HS.member` oldOccNames
---------------------------------------------------------------------------------------------------
-- Reference finding

-- | Note: We only find exact name occurrences (i.e. type reference "depth" is 0).
refsAtName ::
    MonadIO m =>
    IdeState ->
    NormalizedFilePath ->
    Name ->
    ExceptT PluginError m [Location]
refsAtName state nfp targetName = do
    HAR{refMap} <- handleGetHieAst state nfp

    let localRefs =
            case M.lookup (Right targetName) refMap of
            Nothing    -> []
            Just spans -> [ realSrcSpanToLocation sp | (sp, _) <- spans]

    let defLoc = nameSrcSpan targetName
        defLocations = case srcSpanToLocation defLoc of
            Just loc | isGoodSrcSpan defLoc -> [loc]
            _                               -> []

    -- Only query HieDb if it's a global name
    globalRefs <-
        case nameModule_maybe targetName of
            Nothing -> pure []
            Just mod -> do
                ShakeExtras{withHieDb} <- liftIO $ runAction "Rename.HieDb" state getShakeExtras
                liftIO $ withHieDb $ \hieDb ->
                    fmap (mapMaybe rowToLoc) $ findReferences hieDb True (nameOccName targetName) (Just $ moduleName mod) (Just $ moduleUnit mod) []

    pure (defLocations ++ localRefs ++ globalRefs)
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

srcSpanToLocE :: MonadError PluginError m => SrcSpan -> m Location
srcSpanToLocE srcSpan =
    case srcSpanToLocation srcSpan of
        Nothing  -> throwError $ PluginInternalError "Invalid SrcSpan conversion"
        Just loc -> pure loc

locToFilePos :: Monad m => Location -> ExceptT PluginError m (NormalizedFilePath, Position)
locToFilePos (Location uri (Range pos _)) = (,pos) <$> getNormalizedFilePathE uri

-- | Collect locations of simple exported identifiers (IEVar / IEName).
-- Only supports variable exports; complex export forms are rejected.
exportNameLocs ::
    ParsedModule ->
    [Name] ->
    ExceptT PluginError (HandlerM config) [Location]
exportNameLocs pm names = do
    let hsMod = unLoc $ pm_parsed_source pm

    case hsmodExports hsMod of
        Nothing -> pure []
        Just exports ->
            fmap concat $ forM (unLoc exports) $ \export ->
                case unLocA export of
#if MIN_VERSION_ghc(9,10,0)
                    IEVar _ ieWrapped _ -> matchWrapped (getLoc export) ieWrapped
#else
                    IEVar _ ieWrapped  -> matchWrapped (getLoc export) ieWrapped
#endif
                    IEThingAll{}       -> unsupported
                    IEThingWith{}      -> unsupported
                    IEModuleContents{} -> unsupported
                    IEThingAbs{}       -> unsupported
                    IEGroup{}          -> unsupported
                    IEDoc{}            -> unsupported
                    IEDocNamed{}       -> unsupported
  where
    unsupported = throwError $ PluginInternalError "Renaming is unsupported for complex export forms"

    matchWrapped :: SrcSpan -> LIEWrappedName GhcPs -> ExceptT PluginError (HandlerM config) [Location]
    matchWrapped l ieWrapped =
        case unwrapIEWrappedName (unLoc ieWrapped) of
            Just rdr
              | any (matchesRdr rdr) names
              -> do
                loc <- srcSpanToLocE l
                pure [loc]
            _ -> pure []

    matchesRdr rdr name = occNameFS (rdrNameOcc rdr) == occNameFS (nameOccName name)

-- | Extract a RdrName from an IEWrappedName when possible.
unwrapIEWrappedName :: IEWrappedName GhcPs -> Maybe RdrName
unwrapIEWrappedName ie =
    case ie of
        IEName _ (L _ rdr) -> Just rdr
        _                  -> Nothing
---------------------------------------------------------------------------------------------------
-- Config

properties :: Properties '[ 'PropertyKey "crossModule" 'TBoolean]
properties = emptyProperties
  & defineBooleanProperty #crossModule
    "Enable experimental cross-module renaming" True
