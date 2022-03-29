{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ide.Plugin.Rename (descriptor) where

#if MIN_VERSION_ghc(9,2,1)
import           GHC.Parser.Annotation
#endif

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Generics
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as HS
import           Data.Hashable
import           Data.List.Extra
import qualified Data.Map                             as M
import           Data.Maybe
import           Data.Mod.Word
import qualified Data.Text                            as T
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint
import           Development.IDE.Spans.AtPoint
import           HieDb.Query
import           Ide.Plugin.Config
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import Control.Monad

instance Hashable Location
instance Hashable Range
instance Hashable Position
instance Hashable UInt
instance Hashable (Mod a) where hash n = hash (unMod n)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor pluginId = (defaultPluginDescriptor pluginId) {
    pluginHandlers = mkPluginHandler STextDocumentRename renameProvider
}

renameProvider :: PluginMethodHandler IdeState TextDocumentRename
renameProvider state pluginId (RenameParams (TextDocumentIdentifier uri) pos _prog newNameText) =
    response $ do
        nfp <- safeUriToNfp uri
        oldName <- getNameAtPos state nfp pos
        refLocs <- refsAtName state nfp oldName
        let newName = mkTcOcc $ T.unpack newNameText
            filesRefs = collectWith locToUri refLocs
            getFileEdit = flip $ getSrcEdit state . renameRefs newName
        fileEdits <- mapM (uncurry getFileEdit) filesRefs
        pure $ foldl' (<>) mempty fileEdits

---------------------------------------------------------------------------------------------------
-- Source renaming

-- | Apply a function to a `ParsedSource` for a given `Uri` to compute a `WorkspaceEdit`.
getSrcEdit ::
    (MonadLsp config m) =>
    IdeState ->
    (ParsedSource -> ParsedSource) ->
    Uri ->
    ExceptT String m WorkspaceEdit
getSrcEdit state updatePs uri = do
    ccs <- lift getClientCapabilities
    nfp <- safeUriToNfp uri
    annAst <- handleMaybeM ("No parsed source for: " ++ show nfp) $ liftIO $ runAction
        "Rename.GetAnnotatedParsedSource"
        state
        (use GetAnnotatedParsedSource nfp)
    let (ps, anns) = (astA annAst, annsA annAst)
#if !MIN_VERSION_ghc(9,2,1)
    let src = T.pack $ exactPrint ps anns
        res = T.pack $ exactPrint (updatePs ps) anns
#else
    let src = T.pack $ exactPrint ps
        res = T.pack $ exactPrint (updatePs ps)
#endif
    pure $ diffText ccs (uri, src) res IncludeDeletions

-- | Replace names at every given `Location` (in a given `ParsedSource`) with a given new name.
renameRefs ::
    OccName ->
    HashSet Location ->
    ParsedSource ->
    ParsedSource
#if MIN_VERSION_ghc(9,2,1)
renameRefs newName refs = everywhere $
    -- there has to be a better way...
    mkT (replaceLoc @AnnListItem) `extT`
    -- replaceLoc @AnnList `extT` -- not needed
    -- replaceLoc @AnnParen `extT`   -- not needed
    -- replaceLoc @AnnPragma `extT` -- not needed
    -- replaceLoc @AnnContext `extT` -- not needed
    -- replaceLoc @NoEpAnns `extT` -- not needed
    replaceLoc @NameAnn
    where
        replaceLoc :: forall an. Typeable an => LocatedAn an RdrName -> LocatedAn an RdrName
        replaceLoc (L srcSpan oldRdrName)
            | isRef (locA srcSpan) = L srcSpan $ replace oldRdrName
        replaceLoc lOldRdrName = lOldRdrName
#else
renameRefs newName refs = everywhere $ mkT replaceLoc
    where
        replaceLoc :: Located RdrName -> Located RdrName
        replaceLoc (L srcSpan oldRdrName)
            | isRef srcSpan = L srcSpan $ replace oldRdrName
        replaceLoc lOldRdrName = lOldRdrName
#endif

        replace :: RdrName -> RdrName
        replace (Qual modName _) = Qual modName newName
        replace _                = Unqual newName

        isRef :: SrcSpan -> Bool
        isRef = (`elem` refs) . fromJust . srcSpanToLocation

---------------------------------------------------------------------------------------------------
-- Reference finding

-- | Note: We only find exact name occurences (i.e. type reference "depth" is 0).
refsAtName ::
    MonadIO m =>
    IdeState ->
    NormalizedFilePath ->
    Name ->
    ExceptT String m (HashSet Location)
refsAtName state nfp name = do
    ShakeExtras{withHieDb} <- liftIO $ runAction "Rename.HieDb" state getShakeExtras
    ast <- safeGetHieAst state nfp
    dbRefs <- case nameModule_maybe name of
        Nothing -> pure []
        Just mod -> liftIO $ mapMaybe rowToLoc <$> withHieDb (\hieDb ->
            findReferences
                hieDb
                True
                (nameOccName name)
                (Just $ moduleName mod)
                (Just $ moduleUnit mod)
                [fromNormalizedFilePath nfp]
            )
    pure $ HS.fromList $ getNameLocs name ast ++ dbRefs

getNameLocs :: Name -> (HieAstResult, PositionMapping) -> [Location]
getNameLocs name (HAR _ _ rm _ _, pm) =
    mapMaybe (toCurrentLocation pm . realSrcSpanToLocation . fst)
             (concat $ M.lookup (Right name) rm)

---------------------------------------------------------------------------------------------------
-- Util

getNameAtPos :: IdeState -> NormalizedFilePath -> Position -> ExceptT String (LspT Config IO) Name
getNameAtPos state nfp pos = do
    (HAR{hieAst}, pm) <- safeGetHieAst state nfp
    handleMaybe ("No name at position: " ++ show pos) $ listToMaybe $ getNamesAtPoint hieAst pos pm

safeGetHieAst ::
    MonadIO m =>
    IdeState ->
    NormalizedFilePath ->
    ExceptT String m (HieAstResult, PositionMapping)
safeGetHieAst state nfp = handleMaybeM
    ("No AST for file: " ++ show nfp)
    (liftIO $ runAction "Rename.GetHieAst" state $ useWithStale GetHieAst nfp)

safeUriToNfp :: (Monad m) => Uri -> ExceptT String m NormalizedFilePath
safeUriToNfp uri = handleMaybe
    ("No filepath for uri: " ++ show uri)
    (toNormalizedFilePath <$> uriToFilePath uri)

-- Head is safe since groups are non-empty
collectWith :: (Hashable a, Eq a, Eq b) => (a -> b) -> HashSet a -> [(b, HashSet a)]
collectWith f = map (\a -> (f $ head a, HS.fromList a)) . groupOn f . HS.toList

locToUri :: Location -> Uri
locToUri (Location uri _) = uri

nfpToUri :: NormalizedFilePath -> Uri
nfpToUri = filePathToUri . fromNormalizedFilePath
