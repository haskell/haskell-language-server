{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Rename (descriptor) where

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
import           GhcPlugins                           hiding (getLoc, (<>))
import           HieDb.Query
import           Ide.Plugin.Config
import           Ide.Plugin.Retrie                    hiding (descriptor)
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types                   hiding (_changes, _range)
import           Retrie                               hiding (HsModule, getLoc)
import           Retrie.SYB
import Control.Monad

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId) {
    pluginHandlers = mkPluginHandler STextDocumentRename renameProvider
}

renameProvider :: PluginMethodHandler IdeState TextDocumentRename
renameProvider state pluginId (RenameParams (TextDocumentIdentifier uri) pos _prog newNameText) = response $ do
    -- get reference locations
    nfp <- forceGetNfp uri
    oldName <- head <$> getNamesAtPos state pos nfp
    refs <- liftIO $ runAction "Rename.references" state (refsAtName nfp oldName)
    refFiles <- mapM forceGetNfp (nub [uri | Location uri _ <- refs])

    edits <- mapM (renameFile state refs (getOccString oldName) (T.unpack newNameText)) refFiles
    pure $ WorkspaceEdit (Just $ foldl1 (HM.unionWith (<>)) edits) Nothing Nothing

renameFile :: IdeState
    -> [Location]
    -> String
    -> String
    -> NormalizedFilePath
    -> ExceptT String (LspT Config IO) WorkspaceEditMap
renameFile state refs oldNameStr newNameStr nfp = do
    -- Rename LHS declarations / imports / exports
    src <- handleMaybeM "parsed source" $
        liftIO $ runAction
            "Rename.GetAnnotatedParsedModule"
            state
            (use GetAnnotatedParsedSource nfp)
    let sourceEdits = getSourceEdits refs (mkRdrUnqual $ mkTcOcc newNameStr) src

    -- Rename RHS with retrie
    rhsEdits <- getRhsEdits state refs (getRewrite oldNameStr newNameStr) nfp

    pure $ HM.insertWith (<>) (nfpToUri nfp) sourceEdits rhsEdits

getRewrite :: String -> String -> RewriteSpec
getRewrite oldNameStr newNameStr =
    (if isType then AdhocType else Adhoc) (oldNameStr ++ " = " ++ newNameStr)
    where
        isType = isUpper $ head oldNameStr

-------------------------------------------------------------------------------
-- Source renaming

getSourceEdits :: [Location] -> RdrName -> Annotated ParsedSource -> List TextEdit
getSourceEdits refs newRdrName annPs = do
    makeDiffTextEdit src res
    where
        src = T.pack $ printA annPs
        updateMod =
            updateImports refs newRdrName .
            updateExports refs newRdrName .
            updateLhsDecls refs newRdrName
        res = T.pack $ printA $ (fmap . fmap) updateMod annPs

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

updateExports :: [Location] -> RdrName -> HsModule GhcPs -> HsModule GhcPs
updateExports refs newRdrName ps@HsModule{hsmodExports} =
    ps {hsmodExports = (fmap . fmap) (map (fmap $ renameIE refs newRdrName)) hsmodExports}

-- TODO: implement explicit type import/export renames
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

updateLhsDecls :: [Location] -> RdrName -> HsModule GhcPs -> HsModule GhcPs
updateLhsDecls refs newRdrName ps@HsModule{hsmodDecls} =
    ps {hsmodDecls = map (fmap renameLhsDecl) hsmodDecls}
    where
        renameLhsDecl :: HsDecl GhcPs -> HsDecl GhcPs
        renameLhsDecl (SigD xSig (TypeSig xTySig sigNames wc)) =
            SigD xSig $ TypeSig xTySig (map renameRdrName sigNames) wc
        renameLhsDecl (ValD xVal funBind@FunBind{fun_id, fun_matches = fun_matches@MG{mg_alts}})
            | isRef refs fun_id = ValD xVal $ funBind {
                    fun_id = fmap (const newRdrName) fun_id,
                    fun_matches = fun_matches {mg_alts = fmap (map (fmap $ renameLhsMatch newRdrName)) mg_alts}
                }
        renameLhsDecl (TyClD xTy dataDecl@DataDecl{tcdLName, tcdDataDefn = hsDataDefn@HsDataDefn{dd_cons}})
            | isRef refs tcdLName = TyClD xTy $ dataDecl {
                    tcdLName = fmap (const newRdrName) tcdLName,
                    tcdDataDefn = hsDataDefn {dd_cons = map (fmap renameCon) dd_cons}
                }
        renameLhsDecl (TyClD xTy synDecl@SynDecl{tcdLName})
            | isRef refs tcdLName = TyClD xTy $ synDecl {
                    tcdLName = fmap (const newRdrName) tcdLName
                }
        renameLhsDecl decl = decl

        renameCon :: ConDecl GhcPs -> ConDecl GhcPs
        renameCon conDecl = case conDecl of
            cdGast@ConDeclGADT{con_args} -> cdGast {con_args = renameConArgs con_args}
            cdH98@ConDeclH98{con_args} -> cdH98 {con_args = renameConArgs con_args}
            xCd@(XConDecl _) -> xCd

        renameConArgs :: HsConDeclDetails GhcPs -> HsConDeclDetails GhcPs
        renameConArgs (PrefixCon args) = PrefixCon $ map (fmap renameBang) args
        renameConArgs (InfixCon a1 a2) = InfixCon (fmap renameBang a1) (fmap renameBang a2)
        renameConArgs (RecCon record) = RecCon $ fmap (map (fmap renameField)) record

        renameBang :: BangType GhcPs -> BangType GhcPs
        renameBang (HsTyVar xTyVar p name) = HsTyVar xTyVar p $ renameRdrName name
        renameBang _ = error "Expected type var"

        renameField :: ConDeclField GhcPs -> ConDeclField GhcPs
        renameField conDeclField@ConDeclField{cd_fld_type} =
            conDeclField {cd_fld_type = fmap renameBang cd_fld_type}
        renameField _ = error "Expected constructor declaration field"

        renameRdrName :: Located RdrName -> Located RdrName
        renameRdrName rdrName
            | isRef refs rdrName = fmap (const newRdrName) rdrName
            | otherwise = rdrName

renameLhsMatch :: RdrName -> Match GhcPs (LHsExpr GhcPs) -> Match GhcPs (LHsExpr GhcPs)
renameLhsMatch newRdrName match@Match{m_ctxt = funRhs@FunRhs{mc_fun}} =
    match{m_ctxt = funRhs{mc_fun = fmap (const newRdrName) mc_fun}}
renameLhsMatch _ _ = error "Expected function match"

-------------------------------------------------------------------------------
-- retrie

getRhsEdits :: IdeState -> [Location] -> RewriteSpec -> NormalizedFilePath -> ExceptT String (LspT Config IO) WorkspaceEditMap
getRhsEdits state refs rewriteSpec nfp = do
    (session, _) <- handleMaybeM "session deps" $ liftIO $ runAction "Rename.GhcSessionDeps" state (useWithStale GhcSessionDeps nfp)
    (errors, WorkspaceEdit{_changes = edits}) <-
        liftIO $ callRetrieWithTransformerAndUpdates
            (referenceTransformer refs)
            contextUpdater
            state
            (hscEnv session)
            [Right rewriteSpec]
            nfp
            True
    lift $ sendRetrieErrors errors
    handleMaybe "retrie" edits

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

refsAtName :: NormalizedFilePath -> Name -> Action [Location]
refsAtName nfp name = do
    ShakeExtras{hiedb} <- getShakeExtras
    fois <- HM.keys <$> getFilesOfInterestUntracked
    Just asts <- sequence <$> usesWithStale GetHieAst fois
    let foiRefs = concat $ mapMaybe (getNameAstLocations name) asts
        Just mod = nameModule_maybe name
    refs <- liftIO $ mapMaybe rowToLoc <$> findReferences
        hiedb
        True
        (nameOccName name)
        (Just $ moduleName mod)
        (Just $ moduleUnitId mod)
        (map fromNormalizedFilePath fois)
    pure $ nubOrd $ foiRefs ++ refs

getNameAstLocations :: Name -> (HieAstResult, PositionMapping) -> Maybe [Location]
getNameAstLocations name (HAR _ _ rm _ _, mapping) =
    mapMaybe (toCurrentLocation mapping . realSrcSpanToLocation . fst) <$> M.lookup (Right name) rm

-------------------------------------------------------------------------------
-- util

nfpToUri :: NormalizedFilePath -> Uri
nfpToUri = filePathToUri . fromNormalizedFilePath

forceGetNfp :: (Monad m) => Uri -> ExceptT String m NormalizedFilePath
forceGetNfp uri = handleMaybe "uri" $ toNormalizedFilePath <$> uriToFilePath uri

isRef :: Retrie.HasSrcSpan a => [Location] -> a -> Bool
isRef refs = (`elem` refs) . fromJust . srcSpanToLocation . getLoc

locToSpan :: Location -> SrcSpan
locToSpan (Location uri (Range (Position l c) (Position l' c'))) =
    mkSrcSpan (mkSrcLoc' uri (succ l) (succ c)) (mkSrcLoc' uri (succ l') (succ c'))
    where
        mkSrcLoc' = mkSrcLoc . mkFastString . fromJust . uriToFilePath

longerThan :: Location -> Int -> Bool
longerThan (Location _ (Range Position{_character = start} Position{_character = end})) n =
     end - start > n

getNamesAtPos :: IdeState -> Position -> NormalizedFilePath -> ExceptT String (LspT Config IO) [Name]
getNamesAtPos state pos nfp = do
    (HAR{hieAst}, mapping) <- handleMaybeM "ast" . liftIO . runAction "Rename.GetHieAst" state $ useWithStale GetHieAst nfp
    let oldName = getAstNamesAtPoint hieAst pos mapping
    pure oldName

subtractSrcSpans :: SrcSpan -> SrcSpan -> SrcSpan
subtractSrcSpans span1 (RealSrcSpan span2)
    = mkSrcSpan startLoc endLoc
    where
        startLoc = mkSrcLoc (srcSpanFile span2) (srcSpanStartLine span2) (srcSpanEndCol span2)
        endLoc = srcSpanEnd span1
subtractSrcSpans _ _ = error ""
