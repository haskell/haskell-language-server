{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ide.Plugin.Rename (descriptor) where

import           Control.Monad
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Containers.ListUtils
import           Data.Generics
import           Data.List.Extra                      hiding (nubOrd)
import qualified Data.Map                             as M
import           Data.Maybe
import qualified Data.Text                            as T
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.AtPoint
#if MIN_VERSION_ghc(9,0,1)
import           GHC.Types.Name
#else
import           Name
#endif
import           HieDb.Query
import           Ide.Plugin.Config
import           Ide.PluginUtils
import           Ide.Types
import           Language.Haskell.GHC.ExactPrint
import           Language.LSP.Server
import           Language.LSP.Types

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId) {
    pluginHandlers = mkPluginHandler STextDocumentRename renameProvider
}

renameProvider :: PluginMethodHandler IdeState TextDocumentRename
renameProvider state pluginId (RenameParams (TextDocumentIdentifier uri) pos _prog newNameText) =
    response $ do
        nfp <- safeUriToNfp uri
        oldName <- getNameAtPos state nfp pos
        workspaceRefs <- refsAtName state nfp oldName
        let filesRefs = groupOn locToUri workspaceRefs
            getFileEdits = ap (getSrcEdits state . renameModRefs newNameText) (locToUri . head)

        fileEdits <- mapM getFileEdits filesRefs
        pure $ foldl1 (<>) fileEdits

-------------------------------------------------------------------------------
-- Source renaming

-- | Compute a `WorkspaceEdit` by applying a given function to the `ParsedModule` for a given `Uri`.
getSrcEdits ::
    (MonadLsp config m) =>
    IdeState ->
#if MIN_VERSION_ghc(9,0,1)
    (HsModule -> HsModule) ->
#else
    (HsModule GhcPs -> HsModule GhcPs) ->
#endif
    Uri ->
    ExceptT String m WorkspaceEdit
getSrcEdits state updateMod uri = do
    ccs <- lift getClientCapabilities
    nfp <- safeUriToNfp uri
    ParsedModule{pm_parsed_source = ps, pm_annotations = apiAnns} <-
        handleMaybeM "Error: could not get parsed source" $ liftIO $ runAction
            "Rename.GetParsedModuleWithComments"
            state
            (use GetParsedModuleWithComments nfp)

    let anns = relativiseApiAnns ps apiAnns
        src = T.pack $ exactPrint ps anns
        res = T.pack $ exactPrint (updateMod <$> ps) anns

    pure $ diffText ccs (uri, src) res IncludeDeletions

-- | Replace a name at every given `Location` (in a given `HsModule`) with a given new name.
renameModRefs ::
    T.Text ->
    [Location] ->
#if MIN_VERSION_ghc(9,0,1)
    HsModule
    -> HsModule
#else
    HsModule GhcPs
    -> HsModule GhcPs
#endif
renameModRefs newNameText refs = everywhere $ mkT replace
    where
        replace :: Located RdrName -> Located RdrName
        replace (L srcSpan oldRdrName)
            | isRef srcSpan = L srcSpan $ newRdrName oldRdrName
        replace lOldRdrName = lOldRdrName

        newRdrName :: RdrName -> RdrName
        newRdrName oldRdrName = case oldRdrName of
            Qual modName _ -> Qual modName newOccName
            _              -> Unqual newOccName

        newOccName = mkTcOcc $ T.unpack newNameText

        isRef :: SrcSpan -> Bool
        isRef = (`elem` refs) . fromJust . srcSpanToLocation

-------------------------------------------------------------------------------
-- Reference finding

-- | Note: We only find exact name occurences (i.e. type reference "depth" is 0).
refsAtName :: IdeState -> NormalizedFilePath -> Name -> ExceptT [Char] (LspT Config IO) [Location]
refsAtName state nfp name = do
    ShakeExtras{hiedb} <- liftIO $ runAction "Rename.HieDb" state getShakeExtras
    ast <- safeGetHieAst state nfp
    astRefs <- handleMaybe "Error: Could not get name AST references" $ getNameAstLocations name ast
    dbRefs <- case nameModule_maybe name of
        Nothing -> pure []
        Just mod -> liftIO $ mapMaybe rowToLoc <$>
            findReferences
                hiedb
                True
                (nameOccName name)
                (Just $ moduleName mod)
                (Just $ moduleUnit mod)
                [fromNormalizedFilePath nfp]
    pure $ nubOrd $ astRefs ++ dbRefs

getNameAstLocations :: Name -> (HieAstResult, PositionMapping) -> Maybe [Location]
getNameAstLocations name (HAR _ _ rm _ _, mapping) =
    mapMaybe (toCurrentLocation mapping . realSrcSpanToLocation . fst) <$> M.lookup (Right name) rm

-------------------------------------------------------------------------------
-- Util

getNameAtPos :: IdeState -> NormalizedFilePath -> Position -> ExceptT String (LspT Config IO) Name
getNameAtPos state nfp pos = do
    (HAR{hieAst}, mapping) <- safeGetHieAst state nfp
    handleMaybe "Error: could not find name at position" $ listToMaybe $
        getAstNamesAtPoint hieAst pos mapping

nfpToUri :: NormalizedFilePath -> Uri
nfpToUri = filePathToUri . fromNormalizedFilePath

safeUriToNfp :: (Monad m) => Uri -> ExceptT String m NormalizedFilePath
safeUriToNfp = handleMaybe "Error: Could not get uri" . fmap toNormalizedFilePath . uriToFilePath

safeGetHieAst ::
    MonadIO m =>
    IdeState ->
    NormalizedFilePath ->
    ExceptT String m (HieAstResult, PositionMapping)
safeGetHieAst state = handleMaybeM "Error: Could not get AST" . liftIO .
    runAction "Rename.GetHieAst" state . useWithStale GetHieAst

locToUri :: Location -> Uri
locToUri (Location uri _) = uri
