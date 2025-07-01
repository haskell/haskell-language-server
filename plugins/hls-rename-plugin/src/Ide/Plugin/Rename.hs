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
import           Control.Monad.Except                  (ExceptT, throwError)
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
import           GHC.Iface.Ext.Types                   (HieAST (..),
                                                        HieASTs (..),
                                                        NodeOrigin (..),
                                                        SourcedNodeInfo (..))
import           GHC.Iface.Ext.Utils                   (generateReferencesMap)
import           HieDb                                 ((:.) (..))
import           HieDb.Query
import           HieDb.Types                           (RefRow (refIsGenerated))
import           Ide.Plugin.Error
import           Ide.Plugin.Properties
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens            as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types

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
            refs <- HS.fromList . concat <$> mapM (refsAtName state nfp) oldNames

            -- Validate rename
            crossModuleEnabled <- liftIO $ runAction "rename: config" state $ usePropertyAction #crossModule pluginId properties
            unless crossModuleEnabled $ failWhenImportOrExport state nfp refs oldNames
            when (any isBuiltInSyntax oldNames) $ throwError $ PluginInternalError "Invalid rename of built-in syntax"

            -- Perform rename
            let newName = mkTcOcc $ T.unpack newNameText
                filesRefs = collectWith locToUri refs
                getFileEdit (uri, locations) = do
                    verTxtDocId <- liftIO $ runAction "rename: getVersionedTextDoc" state $ getVersionedTextDoc (TextDocumentIdentifier uri)
                    getSrcEdit state verTxtDocId (replaceRefs newName locations)
            fileEdits <- mapM getFileEdit filesRefs
            pure $ InL $ fold fileEdits

-- | Limit renaming across modules.
failWhenImportOrExport ::
    IdeState ->
    NormalizedFilePath ->
    HashSet Location ->
    [Name] ->
    ExceptT PluginError (HandlerM config) ()
failWhenImportOrExport state nfp refLocs names = do
    pm <- runActionE "Rename.GetParsedModule" state
         (useE GetParsedModule nfp)
    let hsMod = unLoc $ pm_parsed_source pm
    case (unLoc <$> hsmodName hsMod, hsmodExports hsMod) of
        (mbModName, _) | not $ any (\n -> nameIsLocalOrFrom (replaceModName n mbModName) n) names
            -> throwError $ PluginInternalError "Renaming of an imported name is unsupported"
        (_, Just (L _ exports)) | any ((`HS.member` refLocs) . unsafeSrcSpanToLoc . getLoc) exports
            -> throwError $ PluginInternalError "Renaming of an exported name is unsupported"
        (Just _, Nothing) -> throwError $ PluginInternalError "Explicit export list required for renaming"
        _ -> pure ()

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
    ParsedSource ->
    ParsedSource
replaceRefs newName refs = everywhere $
    -- there has to be a better way...
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
            | isRef (locA srcSpan) = L srcSpan $ replace oldRdrName
        replaceLoc lOldRdrName = lOldRdrName
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
    ExceptT PluginError m [Location]
refsAtName state nfp name = do
    ShakeExtras{withHieDb} <- liftIO $ runAction "Rename.HieDb" state getShakeExtras
    ast <- handleGetHieAst state nfp
    dbRefs <- case nameModule_maybe name of
        Nothing -> pure []
        Just mod -> liftIO $ mapMaybe rowToLoc <$> withHieDb (\hieDb ->
            -- See Note [Generated references]
            filter (\(refRow HieDb.:. _) -> refIsGenerated refRow) <$>
            findReferences
                hieDb
                True
                (nameOccName name)
                (Just $ moduleName mod)
                (Just $ moduleUnit mod)
                [fromNormalizedFilePath nfp]
            )
    pure $ nameLocs name ast ++ dbRefs

nameLocs :: Name -> HieAstResult -> [Location]
nameLocs name (HAR _ _ rm _ _) =
    concatMap (map (realSrcSpanToLocation . fst))
              (M.lookup (Right name) rm)

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

unsafeSrcSpanToLoc :: SrcSpan -> Location
unsafeSrcSpanToLoc srcSpan =
    case srcSpanToLocation srcSpan of
        Nothing       -> error "Invalid conversion from UnhelpfulSpan to Location"
        Just location -> location

locToFilePos :: Monad m => Location -> ExceptT PluginError m (NormalizedFilePath, Position)
locToFilePos (Location uri (Range pos _)) = (,pos) <$> getNormalizedFilePathE uri

replaceModName :: Name -> Maybe ModuleName -> Module
replaceModName name mbModName =
    mkModule (moduleUnit $ nameModule name) (fromMaybe (mkModuleName "Main") mbModName)

---------------------------------------------------------------------------------------------------
-- Config

properties :: Properties '[ 'PropertyKey "crossModule" 'TBoolean]
properties = emptyProperties
  & defineBooleanProperty #crossModule
    "Enable experimental cross-module renaming" False
