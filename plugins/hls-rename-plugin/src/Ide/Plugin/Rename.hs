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

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId) {
    pluginHandlers = mkPluginHandler STextDocumentRename renameProvider
}

-- Todo: handle errors correctly (remove fromJust)
renameProvider :: PluginMethodHandler IdeState TextDocumentRename
renameProvider state pluginId (RenameParams tdi@(TextDocumentIdentifier uri) pos _progToken newNameStr) = response $ do
    let Just nfp = uriToNormalizedFilePath $ toNormalizedUri uri -- TODO: nfp should be nfp of ref file
    (HAR _ asts _ _ _, mapping) <- liftIO $ fromJust <$> runAction "Rename.GetHieAst" state (useWithStale GetHieAst nfp)
    session <- liftIO $ runAction "Rename.GhcSessionDeps" state (useWithStale GhcSessionDeps nfp)
    let oldName = head $ getNamesAtPoint asts pos mapping
        oldNameStr = getOccString oldName
    refs <- liftIO $ runAction "Rename.references" state (refsAtName nfp oldName)

    -- rename LHS declarations
    annPs <- liftIO $ fromJust <$> runAction "Rename.GetAnnotatedParsedModule" state (use GetAnnotatedParsedSource  nfp)
    let src = printA annPs
        applyDeclRenames = renameLhsModDecls (mkRdrUnqual $ mkTcOcc $ T.unpack newNameStr) oldNameStr
        res = printA $ (fmap . fmap) applyDeclRenames annPs
        declEdits = makeDiffTextEdit (T.pack src) (T.pack res)

    -- use retrie to rename right-hand sides
    let emptyContextUpdater c i = const (return c)
        isType = isUpper $ head oldNameStr
        rewrite = (if isType then AdhocType else Adhoc) (oldNameStr ++ " = " ++ T.unpack newNameStr)
    (_errors, edits@WorkspaceEdit{_changes}) <- liftIO $
        callRetrieWithTransformerAndUpdates
            (referenceTransformer refs)
            emptyContextUpdater
            state
            (hscEnv $ fst $ fromJust session)
            [Right rewrite]
            nfp
            True

    return (edits {_changes = HM.update (Just . (<> declEdits)) uri <$> _changes})

-- TODO: rename lhs for signature declarations
-- TODO: rename lhs for type decls
-- TODO: export/import lists
-- TODO: update srcSpans to newName length
-- TODO: pattern syn type sig
-- TODO: restructure renameLhsModDecls
renameLhsModDecls :: RdrName -> String -> HsModule GhcPs -> HsModule GhcPs
renameLhsModDecls newName oldNameStr ps@HsModule{hsmodDecls} =
    ps {hsmodDecls = map (fmap renameLhsDecl) hsmodDecls}
        where
            renameLhsDecl :: HsDecl GhcPs -> HsDecl GhcPs
            renameLhsDecl (SigD xSig (TypeSig xTySig sigNames wc)) =
                SigD xSig $ TypeSig
                    xTySig
                    (map (renameLhsSig newName oldNameStr <$>) sigNames)
                    wc
            renameLhsDecl (ValD xVal funBind@FunBind{fun_id = L srcSpan funName, fun_matches = fun_matches@MG{mg_alts}})
                | getRdrString funName == oldNameStr =
                    ValD xVal $ funBind {
                        fun_id = L srcSpan newName
                        , fun_matches = fun_matches {mg_alts = fmap ((: []) . (fmap (renameLhsMatch newName) . head)) mg_alts}
                    }
            renameLhsDecl (TyClD xTy dataDecl@DataDecl{tcdLName = L srcSpan typeName})
                | getRdrString typeName == oldNameStr =
                    TyClD xTy $ dataDecl {
                        tcdLName = L srcSpan newName
                    }
            renameLhsDecl (TyClD xTy synDecl@SynDecl{tcdLName = L srcSpan typeName})
                | getRdrString typeName == oldNameStr =
                    TyClD xTy $ synDecl {
                        tcdLName = L srcSpan newName
                    }
            renameLhsDecl decl = decl

renameLhsSig :: RdrName -> String -> RdrName -> RdrName
renameLhsSig newName oldNameStr sigName
    | getRdrString sigName == oldNameStr = newName
renameLhsSig _ _ sigName = sigName

renameLhsMatch :: RdrName -> Match GhcPs (LHsExpr GhcPs) -> Match GhcPs (LHsExpr GhcPs)
renameLhsMatch newName match@Match{m_ctxt = funRhs@FunRhs{mc_fun}} =
    match{m_ctxt = funRhs{mc_fun = fmap (const newName) mc_fun}}
renameLhsMatch _ _ = error "Expected function match"

getRdrString :: RdrName -> String
getRdrString = occNameString . rdrNameOcc

referenceTransformer :: [Location] -> MatchResultTransformer
referenceTransformer refs _ctxt match
  | MatchResult _sub template <- match
  , Just loc <- srcSpanToLocation $ getOrigin $ astA $ tTemplate template -- Bug: incorrect loc
  -- , loc `elem` refs
    = return match
  | otherwise = return NoMatch

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
