{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Ide.Plugin.Rename (descriptor, E.Log) where

#if MIN_VERSION_ghc(9,2,1)
import           GHC.Parser.Annotation                 (AnnContext, AnnList,
                                                        AnnParen, AnnPragma)
#endif

import           Compat.HieTypes
import           Control.Lens                          ((^.))
import           Compat.HieTypes
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Bifunctor                        (first)
import           Data.Generics
import           Data.Hashable
import           Data.HashSet                          (HashSet)
import qualified Data.HashSet                          as HS
import           Data.List.Extra                       hiding (length)
import qualified Data.Map                              as M
import           Data.Maybe
import           Data.Mod.Word
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import           Development.IDE                       (Recorder, WithPriority,
                                                        usePropertyAction)
import qualified Development.IDE.Core.PluginUtils      as PluginUtils
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.Compat.Parser
import           Development.IDE.GHC.Compat.Units
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.ExactPrint
import qualified Development.IDE.GHC.ExactPrint        as E
import           Development.IDE.Plugin.CodeAction
import           Development.IDE.Spans.AtPoint
import           Development.IDE.Types.Location
import           HieDb.Query
import           Ide.Plugin.Properties
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Server
import           Language.LSP.Types
import qualified Language.LSP.Types.Lens               as LSP
import           Compat.HieTypes

instance Hashable (Mod a) where hash n = hash (unMod n)

descriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder pluginId = mkExactprintPluginDescriptor recorder $ (defaultPluginDescriptor pluginId)
    { pluginHandlers = mkPluginHandler STextDocumentRename renameProvider
    , pluginConfigDescriptor = defaultConfigDescriptor
        { configCustomConfig = mkCustomConfig properties }
    }

renameProvider :: PluginMethodHandler IdeState TextDocumentRename
renameProvider state pluginId (RenameParams docId@(TextDocumentIdentifier uri) pos _prog newNameText) =
    PluginUtils.pluginResponse $ do
        nfp <- handleUriToNfp uri
        directOldNames <- getNamesAtPos state nfp pos
        directRefs <- concat <$> mapM (refsAtName state nfp) directOldNames

        {- References in HieDB are not necessarily transitive. With `NamedFieldPuns`, we can have
           indirect references through punned names. To find the transitive closure, we do a pass of
           the direct references to find the references for any punned names.
           See the `IndirectPuns` test for an example. -}
        indirectOldNames <- concat . filter ((>1) . Prelude.length) <$>
            mapM (uncurry (getNamesAtPos state) . locToFilePos) directRefs
        let oldNames = filter matchesDirect indirectOldNames ++ directOldNames
            matchesDirect n = occNameFS (nameOccName n) `elem` directFS
              where
                directFS = map (occNameFS. nameOccName) directOldNames
        refs <- HS.fromList . concat <$> mapM (refsAtName state nfp) oldNames

        -- Validate rename
        crossModuleEnabled <- liftIO $ runAction "rename: config" state $ usePropertyAction #crossModule pluginId properties
        unless crossModuleEnabled $ failWhenImportOrExport state nfp refs oldNames
        when (any isBuiltInSyntax oldNames) $ throwE $ PluginUtils.mkPluginErrorMessage "Invalid rename of built-in syntax"

        -- Perform rename
        let newName = mkTcOcc $ T.unpack newNameText
            filesRefs = collectWith locToUri refs
            getFileEdit (uri, locations) = do
              verTxtDocId <- lift $ getVersionedTextDoc (TextDocumentIdentifier uri)
              getSrcEdit state verTxtDocId (replaceRefs newName locations)
        fileEdits <- mapM getFileEdit filesRefs
        pure $ foldl' (<>) mempty fileEdits

-- | Limit renaming across modules.
failWhenImportOrExport ::
    (MonadLsp config m) =>
    IdeState ->
    NormalizedFilePath ->
    HashSet Location ->
    [Name] ->
    ExceptT PluginUtils.GhcidePluginError m ()
failWhenImportOrExport state nfp refLocs names = do
    pm <- PluginUtils.runAction "Rename.GetParsedModule" state
         (PluginUtils.use GetParsedModule nfp)
    let hsMod = unLoc $ pm_parsed_source pm
    case (unLoc <$> hsmodName hsMod, hsmodExports hsMod) of
        (mbModName, _) | not $ any (\n -> nameIsLocalOrFrom (replaceModName n mbModName) n) names
            -> throwE $ PluginUtils.mkPluginErrorMessage "Renaming of an imported name is unsupported"
        (_, Just (L _ exports)) | any ((`HS.member` refLocs) . unsafeSrcSpanToLoc . getLoc) exports
            -> throwE $ PluginUtils.mkPluginErrorMessage "Renaming of an exported name is unsupported"
        (Just _, Nothing) -> throwE $ PluginUtils.mkPluginErrorMessage "Explicit export list required for renaming"
        _ -> pure ()

---------------------------------------------------------------------------------------------------
-- Source renaming

-- | Apply a function to a `ParsedSource` for a given `Uri` to compute a `WorkspaceEdit`.
getSrcEdit ::
    (MonadLsp config m) =>
    IdeState ->
    VersionedTextDocumentIdentifier ->
    (ParsedSource -> ParsedSource) ->
    ExceptT PluginUtils.GhcidePluginError m WorkspaceEdit
getSrcEdit state verTxtDocId updatePs = do
    ccs <- lift getClientCapabilities
    nfp <- handleUriToNfp (verTxtDocId ^. LSP.uri)
    annAst <- PluginUtils.runAction "Rename.GetAnnotatedParsedSource" state
        (PluginUtils.use GetAnnotatedParsedSource nfp)
    let (ps, anns) = (astA annAst, annsA annAst)
#if !MIN_VERSION_ghc(9,2,1)
    let src = T.pack $ exactPrint ps anns
        res = T.pack $ exactPrint (updatePs ps) anns
#else
    let src = T.pack $ exactPrint ps
        res = T.pack $ exactPrint (updatePs ps)
#endif
    pure $ diffText ccs (verTxtDocId, src) res IncludeDeletions

-- | Replace names at every given `Location` (in a given `ParsedSource`) with a given new name.
replaceRefs ::
    OccName ->
    HashSet Location ->
    ParsedSource ->
    ParsedSource
#if MIN_VERSION_ghc(9,2,1)
replaceRefs newName refs = everywhere $
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
replaceRefs newName refs = everywhere $ mkT replaceLoc
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
        isRef = (`HS.member` refs) . unsafeSrcSpanToLoc

---------------------------------------------------------------------------------------------------
-- Reference finding

-- | Note: We only find exact name occurrences (i.e. type reference "depth" is 0).
refsAtName ::
    MonadIO m =>
    IdeState ->
    NormalizedFilePath ->
    Name ->
    ExceptT PluginUtils.GhcidePluginError m [Location]
refsAtName state nfp name = do
    ShakeExtras{withHieDb} <- liftIO $ runAction "Rename.HieDb" state getShakeExtras
    ast <- handleGetHieAst state nfp
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
    pure $ nameLocs name ast ++ dbRefs

nameLocs :: Name -> (HieAstResult, PositionMapping) -> [Location]
nameLocs name (HAR _ _ rm _ _, pm) =
    mapMaybe (toCurrentLocation pm . realSrcSpanToLocation . fst)
             (concat $ M.lookup (Right name) rm)

---------------------------------------------------------------------------------------------------
-- Util

getNamesAtPos :: MonadIO m => IdeState -> NormalizedFilePath -> Position -> ExceptT PluginUtils.GhcidePluginError m [Name]
getNamesAtPos state nfp pos = do
    (HAR{hieAst}, pm) <- handleGetHieAst state nfp
    pure $ getNamesAtPoint hieAst pos pm

handleGetHieAst ::
    MonadIO m =>
    IdeState ->
    NormalizedFilePath ->
    ExceptT PluginUtils.GhcidePluginError m (HieAstResult, PositionMapping)
handleGetHieAst state nfp =
    fmap (first removeGenerated) $ PluginUtils.runAction "Rename.GetHieAst" state $ PluginUtils.useWithStale GetHieAst nfp

-- | We don't want to rename in code generated by GHC as this gives false positives.
-- So we restrict the HIE file to remove all the generated code.
removeGenerated :: HieAstResult -> HieAstResult
removeGenerated HAR{..} = HAR{hieAst = go hieAst,..}
  where
    go :: HieASTs a -> HieASTs a
    go hf =
#if MIN_VERSION_ghc(9,2,1)
      HieASTs (fmap goAst (getAsts hf))
    goAst (Node nsi sp xs) = Node (SourcedNodeInfo $ M.restrictKeys (getSourcedNodeInfo nsi) (S.singleton SourceInfo)) sp (map goAst xs)
#else
      hf
#endif

handleUriToNfp :: (Monad m) => Uri -> ExceptT PluginUtils.GhcidePluginError m NormalizedFilePath
handleUriToNfp uri = PluginUtils.withPluginError $ getNormalizedFilePath uri

-- head is safe since groups are non-empty
collectWith :: (Hashable a, Eq a, Eq b) => (a -> b) -> HashSet a -> [(b, HashSet a)]
collectWith f = map (\a -> (f $ head a, HS.fromList a)) . groupOn f . HS.toList

locToUri :: Location -> Uri
locToUri (Location uri _) = uri

nfpToUri :: NormalizedFilePath -> Uri
nfpToUri = filePathToUri . fromNormalizedFilePath

showName :: Name -> String
showName = occNameString . getOccName

unsafeSrcSpanToLoc :: SrcSpan -> Location
unsafeSrcSpanToLoc srcSpan =
    case srcSpanToLocation srcSpan of
        Nothing       -> error "Invalid conversion from UnhelpfulSpan to Location"
        Just location -> location

locToFilePos :: Location -> (NormalizedFilePath, Position)
locToFilePos (Location uri (Range pos _)) = (nfp, pos)
    where
        Just nfp = (uriToNormalizedFilePath . toNormalizedUri) uri

replaceModName :: Name -> Maybe ModuleName -> Module
replaceModName name mbModName =
    mkModule (moduleUnit $ nameModule name) (fromMaybe (mkModuleName "Main") mbModName)

---------------------------------------------------------------------------------------------------
-- Config

properties :: Properties '[ 'PropertyKey "crossModule" 'TBoolean]
properties = emptyProperties
  & defineBooleanProperty #crossModule
    "Enable experimental cross-module renaming" False
