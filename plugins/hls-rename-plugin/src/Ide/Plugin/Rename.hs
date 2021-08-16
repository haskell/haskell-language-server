{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ide.Plugin.Rename (descriptor) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import qualified Data.Bifunctor
import           Data.Char
import           Data.Containers.ListUtils
import qualified Data.HashMap.Strict                  as HM
import           Data.List
import qualified Data.Map                             as M
import           Data.Maybe
import qualified Data.Text                            as T
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint
import           Development.IDE.Spans.AtPoint
import           HieDb.Query
import           Ide.Plugin.Config
import           Ide.Plugin.Retrie                    hiding (descriptor)
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types                   hiding (_changes, _range)
import           Retrie                               hiding (HasSrcSpan, HsModule, getLoc)
import           Retrie.SYB

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId) {
    pluginHandlers = mkPluginHandler STextDocumentRename renameProvider
}

renameProvider :: PluginMethodHandler IdeState TextDocumentRename
renameProvider state pluginId (RenameParams (TextDocumentIdentifier uri) pos _prog newNameText) = response $ do
    nfp <- safeGetNfp uri
    oldName <- (handleMaybe "error: could not find name at pos" . listToMaybe) =<<
        getNamesAtPos state pos nfp
    refs <- refsAtName state nfp oldName
    refFiles <- mapM safeGetNfp (nub [uri | Location uri _ <- refs])
    let newNameStr = T.unpack newNameText
        newRdrName = mkRdrUnqual $ mkTcOcc newNameStr

    -- Rename Imports / Export
    let updateIe = updateExports refs newRdrName . updateImports refs newRdrName
    ieFileEdits <- mapMToSnd (getSrcEdits state updateIe) refFiles

    -- Rename left-hand sides (declarations)
    filesDeclEdits <- mapMToSnd (getSrcEdits state (updateLhsDecls refs newRdrName)) refFiles
    declEdits@(originNfp, _) <- handleMaybe "error: could not find name declaration" $
        find (\(_, List xs) -> not $ null xs) filesDeclEdits

    -- Rename right-hand sides (using retrie)
    rhsEditMap <- foldl1 (HM.unionWith (<>)) <$>
        mapM (getRhsEdits state refs oldName newNameStr originNfp) refFiles

    -- combine edits
    let insertEdits (nfp, edits) = HM.insertWith (<>) (nfpToUri nfp) edits
        edits = foldr insertEdits rhsEditMap (declEdits : ieFileEdits)

    pure $ WorkspaceEdit (Just edits) Nothing Nothing

-------------------------------------------------------------------------------
-- Source renaming

getSrcEdits ::
    IdeState
    -> (HsModule GhcPs -> HsModule GhcPs)
    -> NormalizedFilePath
    -> ExceptT String (LspT Config IO) (List TextEdit)
getSrcEdits state updateMod nfp = do
    annPs <- handleMaybeM "error: parsed source" $
                liftIO $ runAction
                    "Rename.GetAnnotatedParsedModule"
                    state
                    (use GetAnnotatedParsedSource nfp)
    let src = T.pack $ printA annPs
        res = T.pack $ printA $ (fmap . fmap) updateMod annPs
    pure $ makeDiffTextEdit src res

updateExports :: [Location] -> RdrName -> HsModule GhcPs -> HsModule GhcPs
updateExports refs newRdrName ps@HsModule{hsmodExports} =
    ps {hsmodExports = (fmap . fmap) (map (fmap $ renameIE refs newRdrName)) hsmodExports}

updateImports :: [Location] -> RdrName -> HsModule GhcPs -> HsModule GhcPs
updateImports refs newRdrName ps@HsModule{hsmodImports} =
    ps {hsmodImports = map (fmap renameImport) hsmodImports}
    where
        renameImport :: ImportDecl GhcPs -> ImportDecl GhcPs
        renameImport importDecl@ImportDecl{ideclHiding = Just (isHiding, names)} =
            importDecl {
                ideclHiding =
                    Just (isHiding, fmap (map (fmap $ renameIE refs newRdrName)) names)
                }
        renameImport importDecl = importDecl

-- TODO: implement explicit type import/export
renameIE :: [Location] -> RdrName -> IE GhcPs -> IE GhcPs
renameIE refs newRdrName (IEVar xVar ieName)
    | isRef refs ieName =
        IEVar xVar (replaceLWrappedName ieName newRdrName)
renameIE refs newRdrName (IEThingAbs xThing ieName)
    | isRef refs ieName =
        IEThingAbs xThing (replaceLWrappedName ieName newRdrName)
renameIE refs newRdrName (IEThingAll xThingAll ieName)
    | isRef refs ieName =
        IEThingAll xThingAll (replaceLWrappedName ieName newRdrName)
renameIE refs newRdrName IEThingWith{}
    = error "not implemented explicit type import/export renames yet"
renameIE _ _ export = export

-- TODO: data constructor renames
updateLhsDecls :: [Location] -> RdrName -> HsModule GhcPs -> HsModule GhcPs
updateLhsDecls refs newRdrName ps@HsModule{hsmodDecls} =
    ps {hsmodDecls = map (fmap renameLhsDecl) hsmodDecls}
    where
        renameLhsDecl :: HsDecl GhcPs -> HsDecl GhcPs
        renameLhsDecl (SigD xSig (TypeSig xTySig sigNames wc)) =
            SigD xSig $ TypeSig xTySig (map renameRdrName' sigNames) wc
        renameLhsDecl (ValD xVal funBind@FunBind{fun_id, fun_matches = fun_matches@MG{mg_alts}})
            = ValD xVal $ funBind {
                    fun_id = renameRdrName' fun_id,
                    fun_matches = fun_matches {mg_alts = fmap (map (fmap $ renameLhsMatch newRdrName)) mg_alts}
                }
        renameLhsDecl (TyClD xTy dataDecl@DataDecl{tcdLName, tcdDataDefn = hsDataDefn@HsDataDefn{dd_cons}})
            = TyClD xTy $ dataDecl {
                    tcdLName = renameRdrName' tcdLName
                }
        renameLhsDecl (TyClD xTy synDecl@SynDecl{tcdLName})
            = TyClD xTy $ synDecl {
                    tcdLName = renameRdrName' tcdLName
                }
        renameLhsDecl decl = decl

        renameRdrName' :: Located RdrName -> Located RdrName
        renameRdrName' = renameRdrName refs newRdrName

        renameLhsMatch :: RdrName -> Match GhcPs (LHsExpr GhcPs) -> Match GhcPs (LHsExpr GhcPs)
        renameLhsMatch newRdrName match@Match{m_ctxt = funRhs@FunRhs{mc_fun}} =
            match{m_ctxt = funRhs{mc_fun = renameRdrName refs newRdrName mc_fun}}
        renameLhsMatch _ _ = error "Expected function match"


renameRdrName :: [Location] -> RdrName -> Located RdrName -> Located RdrName
renameRdrName refs newRdrName oldRdrName
    | isRef refs oldRdrName = fmap (const newRdrName) oldRdrName
    | otherwise = oldRdrName

-------------------------------------------------------------------------------
-- retrie

getRhsEdits ::
    IdeState
    -> [Location]
    -> Name
    -> String
    -> NormalizedFilePath
    -> NormalizedFilePath
    -> ExceptT String (LspT Config IO) WorkspaceEditMap
getRhsEdits state refs oldName newNameStr originNfp nfp = do
    rewriteSpecs <- getRewriteSpecs state (getOccString oldName) newNameStr originNfp nfp
    (session, _) <- handleMaybeM "error: session deps" $
        liftIO $ runAction "Rename.GhcSessionDeps" state (useWithStale GhcSessionDeps nfp)
    (errors, WorkspaceEdit{_changes = edits}) <-
        liftIO $ callRetrieWithTransformerAndUpdates
            (referenceTransformer refs)
            contextUpdater
            state
            (hscEnv session)
            (map Right rewriteSpecs)
            nfp
            True
    lift $ sendRetrieErrors errors
    handleMaybe "error: retrie" edits

getRewriteSpecs ::
    IdeState
    -> String
    -> String
    -> NormalizedFilePath
    -> NormalizedFilePath
    -> ExceptT String (LspT Config IO) [RewriteSpec]
getRewriteSpecs state oldNameStr newNameStr originNfp nfp = do
    ParsedModule{pm_parsed_source = L _ HsModule{hsmodName = mbOriginModule}} <-
         handleMaybeM "error: parsed source" $
             liftIO $ runAction
                "Rename.GetAnnotatedParsedModule"
                state
                (use GetParsedModule originNfp)
    ParsedModule{pm_parsed_source = L _ HsModule{hsmodImports}} <-
        handleMaybeM "error: parsed source" $
            liftIO $ runAction
                "Rename.GetAnnotatedParsedModule"
                state
                (use GetParsedModule nfp)
    let getNameImport (L _ originModule) = find ((==originModule) . unLoc . ideclName) (map unLoc hsmodImports)
        mkRewriteSpec qualStr = (if isUpper $ head oldNameStr then AdhocType else Adhoc) $
            qualStr ++ oldNameStr ++ " = " ++ qualStr ++ newNameStr
        mkQualRewrite = mkRewriteSpec . getQualifierStr
        unQualRewrite = mkRewriteSpec ""
    pure $ case getNameImport =<< mbOriginModule of
        Just nameImport ->
            if isQualifiedImport nameImport
                then [mkQualRewrite nameImport]
                else [unQualRewrite, mkQualRewrite nameImport]
        Nothing -> [unQualRewrite]

getQualifierStr :: ImportDecl pass -> String
getQualifierStr ImportDecl{ideclAs, ideclName} =
    moduleNameString (unLoc (fromMaybe ideclName ideclAs)) ++ "."
getQualifierStr _ = ""

-- limits matches to reference locations
referenceTransformer :: [Location] -> MatchResultTransformer
referenceTransformer refs Context{ctxtBinders} match
    | MatchResult _sub template <- match
    , any (containsRef . getRdrLoc) ctxtBinders = pure match
    | otherwise = pure NoMatch
    where
        containsRef srcSpan = any (flip isSubspanOf srcSpan . locToSpan) refs
        getRdrLoc (Exact name) = nameSrcSpan name
        getRdrLoc _            = error "Expected exact name"

-- Hacky use of ctxtBinders to track match spans
contextUpdater :: (Typeable b, Monad f) => Context -> Int -> b -> f Context
contextUpdater c@Context{ctxtBinders} i = const (pure c)
    `extQ` (return . updType)
    `extQ` (return . updExpr)
    `extQ` (return . updTyDecl)
    `extQ` (return . updMatch)
    where
        -- Todo: add statement matches
        updType :: LHsType GhcPs -> Context
        updType (L _ (HsAppTy _ (L matchSpan _) _)) =
            c {ctxtBinders = makeName matchSpan : ctxtBinders}
        updType (L matchSpan ty) =
            c {ctxtBinders = makeName matchSpan : ctxtBinders}

        updExpr :: LHsExpr GhcPs -> Context
        updExpr (L _ (HsApp _ (L matchSpan a) _)) =
            c {ctxtBinders = makeName matchSpan : ctxtBinders}
        updExpr (L matchSpan _) =
            c {ctxtBinders = makeName matchSpan : ctxtBinders}

        updTyDecl :: TyClDecl GhcPs -> Context
        updTyDecl SynDecl{tcdRhs} =
            c {ctxtBinders = makeName (getLoc tcdRhs) : ctxtBinders}
        updTyDecl _ = c

        updMatch :: LMatch GhcPs (LHsExpr GhcPs) -> Context
        updMatch (L matchSpan Match{m_ctxt = FunRhs{mc_fun = L funNameSpan _}}) =
            c {ctxtBinders = makeName (matchSpan `subtractSrcSpans` funNameSpan) : ctxtBinders}
        updMatch (L matchSpan _) = c {ctxtBinders = makeName matchSpan : ctxtBinders}

        makeName = Exact . mkInternalName initTyVarUnique (mkVarOcc "")

-------------------------------------------------------------------------------
-- reference finding


refsAtName :: IdeState -> NormalizedFilePath -> Name -> ExceptT [Char] (LspT Config IO) [Location]
refsAtName state nfp name = do
    ShakeExtras{hiedb} <- liftIO $ runAction "Rename.HieDb" state getShakeExtras
    ast <- handleMaybeM "error: ast" $ liftIO $ runAction "" state $ useWithStale GetHieAst nfp
    fileRefs <- handleMaybe "error: name references" $ getNameAstLocations name ast
    mod <- handleMaybe "error: module name" $ nameModule_maybe name
    dbRefs <- liftIO $ mapMaybe rowToLoc <$> findReferences
        hiedb
        True
        (nameOccName name)
        (Just $ moduleName mod)
        (Just $ moduleUnitId mod)
        [fromNormalizedFilePath nfp]
    pure $ nubOrd $ fileRefs ++ dbRefs

getNameAstLocations :: Name -> (HieAstResult, PositionMapping) -> Maybe [Location]
getNameAstLocations name (HAR _ _ rm _ _, mapping) =
    mapMaybe (toCurrentLocation mapping . realSrcSpanToLocation . fst) <$> M.lookup (Right name) rm

-------------------------------------------------------------------------------
-- util

nfpToUri :: NormalizedFilePath -> Uri
nfpToUri = filePathToUri . fromNormalizedFilePath

safeGetNfp :: (Monad m) => Uri -> ExceptT String m NormalizedFilePath
safeGetNfp uri = handleMaybe "error: uri" $ toNormalizedFilePath <$> uriToFilePath uri

isRef :: HasSrcSpan a => [Location] -> a -> Bool
isRef refs = (`elem` refs) . fromJust . srcSpanToLocation . getLoc

locToSpan :: Location -> SrcSpan
locToSpan (Location uri (Range (Position l c) (Position l' c'))) =
    mkSrcSpan (mkSrcLoc' uri (succ l) (succ c)) (mkSrcLoc' uri (succ l') (succ c'))
    where
        mkSrcLoc' = mkSrcLoc . mkFastString . fromJust . uriToFilePath

getNamesAtPos :: IdeState -> Position -> NormalizedFilePath -> ExceptT String (LspT Config IO) [Name]
getNamesAtPos state pos nfp = do
    (HAR{hieAst}, mapping) <- handleMaybeM "error: ast" $ liftIO $
        runAction "Rename.GetHieAst" state $ useWithStale GetHieAst nfp
    pure $ getAstNamesAtPoint hieAst pos mapping

subtractSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
subtractSrcSpans minuend (RealSrcSpan subtrahend)
    = mkSrcSpan startLoc endLoc
    where
        startLoc = mkSrcLoc (srcSpanFile subtrahend) (srcSpanStartLine subtrahend) (srcSpanEndCol subtrahend)
        endLoc = srcSpanEnd minuend
subtractSrcSpans _ _ = error "Expected real SrcSpan"

mapMToSnd :: Monad f => (a -> f b) -> [a] -> f [(a, b)]
mapMToSnd = liftM2 (<$>) zip . mapM
