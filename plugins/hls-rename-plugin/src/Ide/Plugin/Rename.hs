{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ide.Plugin.Rename (descriptor) where
import           Control.Monad
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Except
import qualified Data.Bifunctor
import           Data.Containers.ListUtils
import           Data.Generics
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
import           Language.LSP.Types
import           Retrie                               hiding (HasSrcSpan,
                                                       HsModule, getLoc)

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
        newOccName = mkTcOcc newNameStr

    nfpEdits <- mapMToSnd (getSrcEdits state (renameRefs refs (mkTcOcc newNameStr))) refFiles
    let uriEdits = HM.fromList $ map (Data.Bifunctor.first nfpToUri) nfpEdits

    pure $ WorkspaceEdit (Just uriEdits) Nothing Nothing

-------------------------------------------------------------------------------
-- Source renaming

getSrcEdits ::
    IdeState
#if MIN_VERSION_ghc(9,0,1)
    -> (HsModule -> HsModule)
#else
    -> (HsModule GhcPs -> HsModule GhcPs)
#endif
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

renameRefs ::
    [Location]
    -> OccName
#if MIN_VERSION_ghc(9,0,1)
    -> HsModule
    -> HsModule
#else
    -> HsModule GhcPs
    -> HsModule GhcPs
#endif
renameRefs refs newOccName = everywhere $ mkT replace
    where
        replace :: Located RdrName -> Located RdrName
        replace lOldRdrName
            | isRef refs lOldRdrName = fmap (const newRdrName) lOldRdrName
            | otherwise = lOldRdrName
            where
                newRdrName =
                    case unLoc lOldRdrName of
                        Qual modName _ -> Qual modName newOccName
                        _              -> Unqual newOccName

        isRef :: HasSrcSpan a => [Location] -> a -> Bool
        isRef refs = (`elem` refs) . fromJust . srcSpanToLocation . getLoc

------------------------------------------------------------
-- reference finding

refsAtName :: IdeState -> NormalizedFilePath -> Name -> ExceptT [Char] (LspT Config IO) [Location]
refsAtName state nfp name = do
    ShakeExtras{hiedb} <- liftIO $ runAction "Rename.HieDb" state getShakeExtras
    ast <- handleMaybeM "error: ast" $ liftIO $ runAction "" state $ useWithStale GetHieAst nfp
    fileRefs <- handleMaybe "error: name references" $ getNameAstLocations name ast
    let mod = nameModule_maybe name
    dbRefs <- liftIO $ mapMaybe rowToLoc <$> findReferences
        hiedb
        True
        (nameOccName name)
        (moduleName <$> mod)
        (moduleUnitId <$> mod)
        [fromNormalizedFilePath nfp]
    pure $ nubOrd $ fileRefs ++ dbRefs

getNameAstLocations :: Name -> (HieAstResult, PositionMapping) -> Maybe [Location]
getNameAstLocations name (HAR _ _ rm _ _, mapping) =
    mapMaybe (toCurrentLocation mapping . realSrcSpanToLocation . fst) <$> M.lookup (Right name) rm

-------------------------------------------------------------------------------
-- util

safeGetNfp :: (Monad m) => Uri -> ExceptT String m NormalizedFilePath
safeGetNfp uri = handleMaybe "error: uri" $ toNormalizedFilePath <$> uriToFilePath uri

getNamesAtPos :: IdeState -> Position -> NormalizedFilePath -> ExceptT String (LspT Config IO) [Name]
getNamesAtPos state pos nfp = do
    (HAR{hieAst}, mapping) <- handleMaybeM "error: ast" $ liftIO $
        runAction "Rename.GetHieAst" state $ useWithStale GetHieAst nfp
    pure $ getAstNamesAtPoint hieAst pos mapping

mapMToSnd :: Monad f => (a -> f b) -> [a] -> f [(a, b)]
mapMToSnd = liftM2 (<$>) zip . mapM

nfpToUri :: NormalizedFilePath -> Uri
nfpToUri = filePathToUri . fromNormalizedFilePath
