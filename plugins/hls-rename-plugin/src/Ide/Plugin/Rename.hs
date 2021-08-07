{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Rename (descriptor) where

import           Control.Monad.IO.Class
import qualified Data.Bifunctor
import           Data.Char
import           Data.Containers.ListUtils
import qualified Data.HashMap.Strict                  as HM
import qualified Data.Map                             as M
import           Data.Maybe
import qualified Data.Text                            as T
import           Debug.Trace
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

-- TODO: update srcSpans to newName length
-- TODO: import lists

renameProvider :: PluginMethodHandler IdeState TextDocumentRename
renameProvider state pluginId (RenameParams tdi@(TextDocumentIdentifier uri) pos _progToken newNameStr) = response $ do
-- Todo: handle errors correctly (remove fromJust)
    let Just nfp = uriToNormalizedFilePath $ toNormalizedUri uri -- TODO: nfp should be nfp of ref file
    (HAR _ asts _ _ _, mapping) <- liftIO $ fromJust <$> runAction "Rename.GetHieAst" state (useWithStale GetHieAst nfp)
    session <- liftIO $ runAction "Rename.GhcSessionDeps" state (useWithStale GhcSessionDeps nfp)
    let oldName = head $ getNamesAtPoint asts pos mapping
        oldNameStr = getOccString oldName
    refs <- liftIO $ runAction "Rename.references" state (refsAtName nfp oldName)

    -- rename LHS declarations
    annPs <- liftIO $ fromJust <$> runAction "Rename.GetAnnotatedParsedModule" state (use GetAnnotatedParsedSource  nfp)
    let newRdrName = mkRdrUnqual $ mkTcOcc $ T.unpack newNameStr
        src = printA annPs
        res = printA $ (fmap . fmap)
            (updateExports newRdrName oldNameStr . renameLhsModDecls newRdrName oldNameStr)
            annPs
        declEdits = makeDiffTextEdit (T.pack src) (T.pack res)

    -- use retrie to rename right-hand sides
    let emptyContextUpdater c i = const $ pure c
        isType = isUpper $ head oldNameStr
        rewrite = (if isType then AdhocType else Adhoc) (oldNameStr ++ " = " ++ T.unpack newNameStr)
    (_errors, edits@WorkspaceEdit{_changes}) <-
        case declEdits of
            List [] -> pure ([], WorkspaceEdit Nothing Nothing Nothing)
            _ -> liftIO $ callRetrieWithTransformerAndUpdates
                    (referenceTransformer refs)
                    emptyContextUpdater
                    state
                    (hscEnv $ fst $ fromJust session)
                    [Right rewrite]
                    nfp
                    True

    pure $ edits {_changes = HM.insertWith (<>) uri declEdits <$> _changes}

updateExports :: RdrName -> String -> HsModule GhcPs -> HsModule GhcPs
updateExports newName oldNameStr ps@HsModule{hsmodExports} =
    ps {hsmodExports = (fmap . fmap) (map (fmap renameExport)) hsmodExports}
    where
        -- TODO: implement explicit type export renames
        renameExport :: IE GhcPs -> IE GhcPs
        renameExport (IEVar xVar ieName)
            | show ieName == oldNameStr =
                IEVar xVar (replaceLWrappedName ieName newName)
        renameExport (IEThingAbs xThing ieName)
            | show ieName == oldNameStr =
                IEThingAbs xThing (replaceLWrappedName ieName newName)
        renameExport (IEThingAll xThingAll ieName)
            | show ieName == oldNameStr =
                IEThingAll xThingAll (replaceLWrappedName ieName newName)
        renameExport export = export

renameLhsModDecls :: RdrName -> String -> HsModule GhcPs -> HsModule GhcPs
renameLhsModDecls newName oldNameStr ps@HsModule{hsmodDecls} =
-- TODO: pattern syn type sig?
-- TODO: restructure renameLhsModDecls
    ps {hsmodDecls = map (fmap renameLhsDecl) hsmodDecls}
    where
        renameLhsDecl :: HsDecl GhcPs -> HsDecl GhcPs
        renameLhsDecl (SigD xSig (TypeSig xTySig sigNames wc)) =
            SigD xSig $ TypeSig
                xTySig
                (map (fmap renameRdrname) sigNames)
                wc
        renameLhsDecl (ValD xVal funBind@FunBind{fun_id = L srcSpan funName, fun_matches = fun_matches@MG{mg_alts}})
            | show funName == oldNameStr =
                ValD xVal $ funBind {
                    fun_id = L srcSpan newName,
                    fun_matches = fun_matches {mg_alts = fmap ((: []) . (fmap (renameLhsMatch newName) . head)) mg_alts}
                }
        renameLhsDecl (TyClD xTy dataDecl@DataDecl{tcdLName = L srcSpan typeName, tcdDataDefn = hsDataDefn@HsDataDefn{dd_cons}})
                 | show typeName == oldNameStr =
                     TyClD xTy $ dataDecl {
                         tcdLName = L srcSpan newName,
                         tcdDataDefn = hsDataDefn {dd_cons = map (fmap renameCon) dd_cons}
                     }
        renameLhsDecl (TyClD xTy synDecl@SynDecl{tcdLName = L srcSpan typeName})
            | show typeName == oldNameStr =
                TyClD xTy $ synDecl {
                    tcdLName = L srcSpan newName
                }
        renameLhsDecl decl = decl

        renameRdrname :: RdrName -> RdrName
        renameRdrname rdrName = if show rdrName == oldNameStr then newName else rdrName

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
        renameBang (HsTyVar a b name) = HsTyVar a b $ fmap renameRdrname name
        renameBang _                  = error "Expected type var"

        renameField :: ConDeclField GhcPs -> ConDeclField GhcPs
        renameField conDeclField@ConDeclField{cd_fld_type} =
            conDeclField {cd_fld_type = fmap renameBang cd_fld_type}
        renameField _ = error "Expected constructor declaration field"

renameLhsMatch :: RdrName -> Match GhcPs (LHsExpr GhcPs) -> Match GhcPs (LHsExpr GhcPs)
renameLhsMatch newName match@Match{m_ctxt = funRhs@FunRhs{mc_fun}} =
    match{m_ctxt = funRhs{mc_fun = fmap (const newName) mc_fun}}
renameLhsMatch _ _ = error "Expected function match"

referenceTransformer :: [Location] -> MatchResultTransformer
referenceTransformer refs _ctxt match
  | MatchResult _sub template <- match
  , Just loc <- srcSpanToLocation $ getOrigin $ astA $ tTemplate template -- Bug: incorrect loc
  -- , loc `elem` refs
    = pure match
  | otherwise = pure NoMatch

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
            rows <- liftIO $ findReferences hiedb True (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnitId mod) exclude
            pure $ mapMaybe rowToLoc rows

getNameAstLocations :: Name -> (HieAstResult, PositionMapping) -> Maybe [Location]
getNameAstLocations name (HAR _ _ rm _ _, mapping) =
    mapMaybe (toCurrentLocation mapping . realSrcSpanToLocation . fst) <$> M.lookup (Right name) rm
