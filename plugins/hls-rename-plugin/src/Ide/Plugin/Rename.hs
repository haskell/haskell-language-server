{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Rename (descriptor) where

import           Control.Monad.IO.Class
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
import           Ide.Plugin.Retrie                    hiding (descriptor)
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Types                   hiding (_changes)
import           Name
import           Retrie                               hiding (HsModule, getLoc)

instance Show RdrName where
    show = occNameString . rdrNameOcc

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId) {
    pluginHandlers = mkPluginHandler STextDocumentRename renameProvider
}

renameProvider :: PluginMethodHandler IdeState TextDocumentRename
renameProvider state pluginId (RenameParams tdi@(TextDocumentIdentifier uri) pos _progToken newRdrNameStr) = response $ do
    nfp <- forceGetNfp uri
    (HAR _ asts _ _ _, mapping) <- handleMaybeM "ast" $ liftIO $ runAction "Rename.GetHieAst" state (useWithStale GetHieAst nfp)
    let oldName = head $ getNamesAtPoint asts pos mapping
        oldNameStr = getOccString oldName
    refs <- liftIO $ runAction "Rename.references" state (refsAtName nfp oldName)

    -- rename LHS declarations / imports / exports
    let refUris = nub [refFile | Location refFile _ <- refs]
        newRdrName = mkRdrUnqual $ mkTcOcc $ T.unpack newRdrNameStr
    refFiles <- mapM forceGetNfp refUris
    sources <- mapM (handleMaybe "parsed source") =<< liftIO (runAction
        "Rename.GetAnnotatedParsedModule"
        state
        (uses GetAnnotatedParsedSource refFiles))
    let declEdits = filter
            (not . isListEmpty . snd)
            (zip refUris $ map (lhsDeclEdits newRdrName refs) sources)

    -- use retrie to rename right-hand sides
    (session, _) <- handleMaybeM "session deps" . liftIO $ runAction "Rename.GhcSessionDeps" state (useWithStale GhcSessionDeps nfp)
    let emptyContextUpdater c i = const $ pure c
        isType = isUpper $ head oldNameStr
        rewrite = (if isType then AdhocType else Adhoc) (oldNameStr ++ " = " ++ T.unpack newRdrNameStr)
    (_errors, retrieEdit@WorkspaceEdit{_changes}) <- liftIO $ callRetrieWithTransformerAndUpdates
        (referenceTransformer refs)
        emptyContextUpdater
        state
        (hscEnv session)
        [Right rewrite]
        nfp
        False

    pure $ case declEdits of
        [] -> WorkspaceEdit Nothing Nothing Nothing
        declEdits' -> retrieEdit {
            _changes = foldl1 (.) (map (uncurry $ HM.insertWith (<>)) declEdits') <$> _changes
            }

-------------------------------------------------------------------------------
-- Source renaming

lhsDeclEdits :: RdrName -> [Location] -> Annotated ParsedSource -> List TextEdit
lhsDeclEdits newRdrName refs annPs = makeDiffTextEdit src res
    where
        src = T.pack $ printA annPs
        updateMod =
            updateImports newRdrName refs .
            updateExports newRdrName refs .
            updateLhsDecls newRdrName refs
        res = T.pack $ printA $ (fmap . fmap) updateMod annPs

updateImports :: RdrName -> [Location] -> HsModule GhcPs -> HsModule GhcPs
updateImports newRdrName refs ps@HsModule{hsmodImports} =
    ps {hsmodImports = map (fmap renameImport) hsmodImports}
    where
        renameImport :: ImportDecl GhcPs -> ImportDecl GhcPs
        renameImport importDecl@ImportDecl{ideclHiding = Just (isHiding, names)} =
            importDecl {
                ideclHiding =
                    Just (isHiding, fmap (map (fmap $ renameIE refs newRdrName)) names)
                }
        renameImport importDecl = importDecl

updateExports :: RdrName -> [Location] -> HsModule GhcPs -> HsModule GhcPs
updateExports newRdrName refs ps@HsModule{hsmodExports} =
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

updateLhsDecls :: RdrName -> [Location] -> HsModule GhcPs -> HsModule GhcPs
updateLhsDecls newRdrName refs ps@HsModule{hsmodDecls} =
    ps {hsmodDecls = map (fmap renameLhsDecl) hsmodDecls}
    where
        renameLhsDecl :: HsDecl GhcPs -> HsDecl GhcPs
        renameLhsDecl (SigD xSig (TypeSig xTySig sigNames wc)) =
            SigD xSig $ TypeSig xTySig (map renameRdrName sigNames) wc
        renameLhsDecl (ValD xVal funBind@FunBind{fun_id, fun_matches = fun_matches@MG{mg_alts}})
            | isRef refs fun_id = ValD xVal $ funBind {
                    fun_id = fmap (const newRdrName) fun_id,
                    fun_matches = fun_matches {mg_alts = fmap ((: []) . fmap (renameLhsMatch newRdrName) . head) mg_alts}
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

-- limits matches to reference locations
referenceTransformer :: [Location] -> MatchResultTransformer
referenceTransformer refs _ctxt match
  | MatchResult _sub template <- match
  , srcSpan <- getOrigin $ astA $ tTemplate template -- Bug: incorrect loc
--   , isRef refs srcSpan
    = pure match
  | otherwise = pure NoMatch

-------------------------------------------------------------------------------
-- reference finding

refsAtName :: NormalizedFilePath -> Name -> Action [Location]
refsAtName nfp name = do
    fois <- HM.keys <$> getFilesOfInterestUntracked
    Just asts <- sequence <$> usesWithStale GetHieAst fois
    let foiRefs = concat $ mapMaybe (getNameAstLocations name) asts
    refs <- nameDbRefs fois name
    pure $ nubOrd $ foiRefs ++ refs

nameDbRefs :: [NormalizedFilePath] -> Name -> Action [Location]
nameDbRefs fois name = do
    ShakeExtras{hiedb} <- getShakeExtras
    case nameModule_maybe name of
        Nothing -> pure []
        Just mod -> do
            let exclude = map fromNormalizedFilePath fois
            rows <- liftIO $ findReferences
                hiedb
                True
                (nameOccName name)
                (Just $ moduleName mod)
                (Just $ moduleUnitId mod)
                exclude
            pure $ mapMaybe rowToLoc rows

getNameAstLocations :: Name -> (HieAstResult, PositionMapping) -> Maybe [Location]
getNameAstLocations name (HAR _ _ rm _ _, mapping) =
    mapMaybe (toCurrentLocation mapping . realSrcSpanToLocation . fst) <$> M.lookup (Right name) rm

-------------------------------------------------------------------------------
-- util

forceGetNfp :: (Monad m) => Uri -> ExceptT String m NormalizedFilePath
forceGetNfp nfp = handleMaybe "uri" $ toNormalizedFilePath <$> uriToFilePath nfp

isTopLevelSpan :: SrcSpan -> Bool
isTopLevelSpan (RealSrcSpan srcSpan) = srcSpanStartCol srcSpan == 1
isTopLevelSpan _                     = False

isListEmpty :: List a -> Bool
isListEmpty (List xs) = null xs

isRef :: Retrie.HasSrcSpan a => [Location] -> a -> Bool
isRef refs = (`elem` refs) . fromJust . srcSpanToLocation . getLoc
