-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP   #-}
{-# LANGUAGE GADTs #-}

-- | Gives information about symbols at a given point in DAML files.
-- These are all pure functions that should execute quickly.
module Development.IDE.Spans.AtPoint (
    atPoint
  , gotoDefinition
  , gotoTypeDefinition
  , documentHighlight
  , pointCommand
  , referencesAtPoint
  , computeTypeReferences
  , FOIReferences(..)
  , defRowToSymbolInfo
  , getNamesAtPoint
  , toCurrentLocation
  , rowToLoc
  , nameToLocation
  , LookupModule
  ) where

import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Orphans          ()
import           Development.IDE.Types.Location
import           Language.LSP.Protocol.Types          hiding
                                                      (SemanticTokenAbsolute (..))
import           Prelude                              hiding (mod)

-- compiler and infrastructure
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat.Util      as Util
import           Development.IDE.GHC.Util             (printOutputable)
import           Development.IDE.Spans.Common
import           Development.IDE.Types.Options

import           Control.Applicative
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Data.Coerce                          (coerce)
import qualified Data.HashMap.Strict                  as HM
import qualified Data.Map.Strict                      as M
import           Data.Maybe
import qualified Data.Text                            as T

import qualified Data.Array                           as A
import           Data.Either
import           Data.List                            (isSuffixOf)
import           Data.List.Extra                      (dropEnd1, nubOrd)

import           Data.Version                         (showVersion)
import           Development.IDE.GHC.Compat           (getSourceNodeIds)
import           Development.IDE.Types.Shake          (WithHieDb)
import           HieDb                                hiding (pointCommand,
                                                       withHieDb)
import           System.Directory                     (doesFileExist)

-- | Gives a Uri for the module, given the .hie file location and the the module info
-- The Bool denotes if it is a boot module
type LookupModule m = FilePath -> ModuleName -> Unit -> Bool -> MaybeT m Uri

-- | HieFileResult for files of interest, along with the position mappings
newtype FOIReferences = FOIReferences (HM.HashMap NormalizedFilePath (HieAstResult, PositionMapping))

computeTypeReferences :: Foldable f => f (HieAST Type) -> M.Map Name [Span]
computeTypeReferences = foldr (\ast m -> M.unionWith (++) (go ast) m) M.empty
  where
    go ast = M.unionsWith (++) (this : map go (nodeChildren ast))
      where
        this = M.fromListWith (++)
          $ map (, [nodeSpan ast])
          $ concatMap namesInType
          $ mapMaybe (\x -> guard (not $ all isOccurrence $ identInfo x) *> identType x)
          $ M.elems
          $ nodeIdentifiers $ nodeInfo ast

-- | Given a file and position, return the names at a point, the references for
-- those names in the FOIs, and a list of file paths we already searched through
foiReferencesAtPoint
  :: NormalizedFilePath
  -> Position
  -> FOIReferences
  -> ([Name],[Location],[FilePath])
foiReferencesAtPoint file pos (FOIReferences asts) =
  case HM.lookup file asts of
    Nothing -> ([],[],[])
    Just (HAR _ hf _ _ _,mapping) ->
      let names = getNamesAtPoint hf pos mapping
          adjustedLocs = HM.foldr go [] asts
          go (HAR _ _ rf tr _, goMapping) xs = refs ++ typerefs ++ xs
            where
              refs = concatMap (mapMaybe (toCurrentLocation goMapping . realSrcSpanToLocation . fst))
                               (mapMaybe (\n -> M.lookup (Right n) rf) names)
              typerefs = concatMap (mapMaybe (toCurrentLocation goMapping . realSrcSpanToLocation))
                                   (mapMaybe (`M.lookup` tr) names)
        in (names, adjustedLocs,map fromNormalizedFilePath $ HM.keys asts)

getNamesAtPoint :: HieASTs a -> Position -> PositionMapping -> [Name]
getNamesAtPoint hf pos mapping =
  concat $ pointCommand hf posFile (rights . M.keys . getNodeIds)
    where
      posFile = fromMaybe pos $ fromCurrentPosition mapping pos

toCurrentLocation :: PositionMapping -> Location -> Maybe Location
toCurrentLocation mapping (Location uri range) =
  Location uri <$> toCurrentRange mapping range

referencesAtPoint
  :: MonadIO m
  => WithHieDb
  -> NormalizedFilePath -- ^ The file the cursor is in
  -> Position -- ^ position in the file
  -> FOIReferences -- ^ references data for FOIs
  -> m [Location]
referencesAtPoint withHieDb nfp pos refs = do
  -- The database doesn't have up2date references data for the FOIs so we must collect those
  -- from the Shake graph.
  let (names, foiRefs, exclude) = foiReferencesAtPoint nfp pos refs
  nonFOIRefs <- forM names $ \name ->
    case nameModule_maybe name of
      Nothing -> pure []
      Just mod -> do
         -- Look for references (strictly in project files, not dependencies),
         -- excluding the files in the FOIs (since those are in foiRefs)
         rows <- liftIO $ withHieDb (\hieDb -> findReferences hieDb True (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnit mod) exclude)
         pure $ mapMaybe rowToLoc rows
  typeRefs <- forM names $ \name ->
    case nameModule_maybe name of
      Just mod | isTcClsNameSpace (occNameSpace $ nameOccName name) -> do
        refs' <- liftIO $ withHieDb (\hieDb -> findTypeRefs hieDb True (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnit mod) exclude)
        pure $ mapMaybe typeRowToLoc refs'
      _ -> pure []
  pure $ nubOrd $ foiRefs ++ concat nonFOIRefs ++ concat typeRefs

rowToLoc :: Res RefRow -> Maybe Location
rowToLoc (row:.info) = flip Location range <$> mfile
  where
    range = Range start end
    start = Position (fromIntegral $ refSLine row - 1) (fromIntegral $ refSCol row -1)
    end = Position (fromIntegral $ refELine row - 1) (fromIntegral $ refECol row -1)
    mfile = case modInfoSrcFile info of
      Just f  -> Just $ toUri f
      Nothing -> Nothing

typeRowToLoc :: Res TypeRef -> Maybe Location
typeRowToLoc (row:.info) = do
  file <- modInfoSrcFile info
  pure $ Location (toUri file) range
  where
    range = Range start end
    start = Position (fromIntegral $ typeRefSLine row - 1) (fromIntegral $ typeRefSCol row -1)
    end = Position (fromIntegral $ typeRefELine row - 1) (fromIntegral $ typeRefECol row -1)

documentHighlight
  :: Monad m
  => HieASTs a
  -> RefMap a
  -> Position
  -> MaybeT m [DocumentHighlight]
documentHighlight hf rf pos = pure highlights
  where
    -- We don't want to show document highlights for evidence variables, which are supposed to be invisible
    notEvidence = not . any isEvidenceContext . identInfo
    ns = concat $ pointCommand hf pos (rights . M.keys . M.filter notEvidence . getSourceNodeIds)
    highlights = do
      n <- ns
      ref <- fromMaybe [] (M.lookup (Right n) rf)
      pure $ makeHighlight ref
    makeHighlight (sp,dets) =
      DocumentHighlight (realSrcSpanToRange sp) (Just $ highlightType $ identInfo dets)
    highlightType s =
      if any (isJust . getScopeFromContext) s
        then DocumentHighlightKind_Write
        else DocumentHighlightKind_Read

gotoTypeDefinition
  :: MonadIO m
  => WithHieDb
  -> LookupModule m
  -> IdeOptions
  -> HieAstResult
  -> Position
  -> MaybeT m [Location]
gotoTypeDefinition withHieDb lookupModule ideOpts srcSpans pos
  = lift $ typeLocationsAtPoint withHieDb lookupModule ideOpts pos srcSpans

-- | Locate the definition of the name at a given position.
gotoDefinition
  :: MonadIO m
  => WithHieDb
  -> LookupModule m
  -> IdeOptions
  -> M.Map ModuleName NormalizedFilePath
  -> HieASTs a
  -> Position
  -> MaybeT m [Location]
gotoDefinition withHieDb getHieFile ideOpts imports srcSpans pos
  = lift $ locationsAtPoint withHieDb getHieFile ideOpts imports pos srcSpans

-- | Synopsis for the name at a given position.
atPoint
  :: IdeOptions
  -> HieAstResult
  -> DocAndTyThingMap
  -> HscEnv
  -> Position
  -> IO (Maybe (Maybe Range, [T.Text]))
atPoint IdeOptions{} (HAR _ hf _ _ (kind :: HieKind hietype)) (DKMap dm km) env pos =
    listToMaybe <$> sequence (pointCommand hf pos hoverInfo)
  where
    -- Hover info for values/data
    hoverInfo :: HieAST hietype -> IO (Maybe Range, [T.Text])
    hoverInfo ast = do
        prettyNames <- mapM prettyName filteredNames
        pure (Just range, prettyNames ++ pTypes)
      where
        pTypes :: [T.Text]
        pTypes
          | Prelude.length names == 1 = dropEnd1 $ map wrapHaskell prettyTypes
          | otherwise = map wrapHaskell prettyTypes

        range :: Range
        range = realSrcSpanToRange $ nodeSpan ast

        wrapHaskell :: T.Text -> T.Text
        wrapHaskell x = "\n```haskell\n"<>x<>"\n```\n"

        info :: NodeInfo hietype
        info = nodeInfoH kind ast

        names :: [(Identifier, IdentifierDetails hietype)]
        names = M.assocs $ nodeIdentifiers info

        -- Check for evidence bindings
        isInternal :: (Identifier, IdentifierDetails a) -> Bool
        isInternal (Right _, dets) =
          any isEvidenceContext $ identInfo dets
        isInternal (Left _, _) = False

        filteredNames :: [(Identifier, IdentifierDetails hietype)]
        filteredNames = filter (not . isInternal) names

        prettyName :: (Either ModuleName Name, IdentifierDetails hietype) -> IO T.Text
        prettyName (Right n, dets) = pure $ T.unlines $
          wrapHaskell (printOutputable n <> maybe "" (" :: " <>) ((prettyType <$> identType dets) <|> maybeKind))
          : maybeToList (pretty (definedAt n) (prettyPackageName n))
          ++ catMaybes [ T.unlines . spanDocToMarkdown <$> lookupNameEnv dm n
                       ]
          where maybeKind = fmap printOutputable $ safeTyThingType =<< lookupNameEnv km n
                pretty Nothing Nothing = Nothing
                pretty (Just define) Nothing = Just $ define <> "\n"
                pretty Nothing (Just pkgName) = Just $ pkgName <> "\n"
                pretty (Just define) (Just pkgName) = Just $ define <> " " <> pkgName <> "\n"
        prettyName (Left m,_) = packageNameForImportStatement m

        prettyPackageName :: Name -> Maybe T.Text
        prettyPackageName n = do
          m <- nameModule_maybe n
          pkgTxt <- packageNameWithVersion m
          pure $ "*(" <> pkgTxt <> ")*"

        -- Return the module text itself and
        -- the package(with version) this `ModuleName` belongs to.
        packageNameForImportStatement :: ModuleName -> IO T.Text
        packageNameForImportStatement mod = do
          mpkg <- findImportedModule env mod :: IO (Maybe Module)
          let moduleName = printOutputable mod
          case mpkg >>= packageNameWithVersion of
            Nothing             -> pure moduleName
            Just pkgWithVersion -> pure $ moduleName <> "\n\n" <> pkgWithVersion

        -- Return the package name and version of a module.
        -- For example, given module `Data.List`, it should return something like `base-4.x`.
        packageNameWithVersion :: Module -> Maybe T.Text
        packageNameWithVersion m = do
          let pid = moduleUnit m
          conf <- lookupUnit env pid
          let pkgName = T.pack $ unitPackageNameString conf
              version = T.pack $ showVersion (unitPackageVersion conf)
          pure $ pkgName <> "-" <> version

        -- Type info for the current node, it may contains several symbols
        -- for one range, like wildcard
        types :: [hietype]
        types = nodeType info

        prettyTypes :: [T.Text]
        prettyTypes = map (("_ :: "<>) . prettyType) types

        prettyType :: hietype -> T.Text
        prettyType t = case kind of
          HieFresh -> printOutputable t
          HieFromDisk full_file -> printOutputable $ hieTypeToIface $ recoverFullType t (hie_types full_file)

        definedAt :: Name -> Maybe T.Text
        definedAt name =
          -- do not show "at <no location info>" and similar messages
          -- see the code of 'pprNameDefnLoc' for more information
          case nameSrcLoc name of
            UnhelpfulLoc {} | isInternalName name || isSystemName name -> Nothing
            _ -> Just $ "*Defined " <> printOutputable (pprNameDefnLoc name) <> "*"

typeLocationsAtPoint
  :: forall m
   . MonadIO m
  => WithHieDb
  -> LookupModule m
  -> IdeOptions
  -> Position
  -> HieAstResult
  -> m [Location]
typeLocationsAtPoint withHieDb lookupModule _ideOptions pos (HAR _ ast _ _ hieKind) =
  case hieKind of
    HieFromDisk hf ->
      let arr = hie_types hf
          ts = concat $ pointCommand ast pos getts
          unfold = map (arr A.!)
          getts x = nodeType ni  ++ (mapMaybe identType $ M.elems $ nodeIdentifiers ni)
            where ni = nodeInfo' x
          getTypes' ts' = flip concatMap (unfold ts') $ \case
            HTyVarTy n -> [n]
            HAppTy a (HieArgs xs) -> getTypes' (a : map snd xs)
            HTyConApp tc (HieArgs xs) -> ifaceTyConName tc : getTypes' (map snd xs)
            HForAllTy _ a -> getTypes' [a]
            HFunTy a b c -> getTypes' [a,b,c]
            HQualTy a b -> getTypes' [a,b]
            HCastTy a -> getTypes' [a]
            _ -> []
        in fmap nubOrd $ concatMapM (fmap (fromMaybe []) . nameToLocation withHieDb lookupModule) (getTypes' ts)
    HieFresh ->
      let ts = concat $ pointCommand ast pos getts
          getts x = nodeType ni  ++ (mapMaybe identType $ M.elems $ nodeIdentifiers ni)
            where ni = nodeInfo x
        in fmap nubOrd $ concatMapM (fmap (fromMaybe []) . nameToLocation withHieDb lookupModule) (getTypes ts)

namesInType :: Type -> [Name]
namesInType (TyVarTy n)      = [varName n]
namesInType (AppTy a b)      = getTypes [a,b]
namesInType (TyConApp tc ts) = tyConName tc : getTypes ts
namesInType (ForAllTy b t)   = varName (binderVar b) : namesInType t
namesInType (FunTy _ a b)    = getTypes [a,b]
namesInType (CastTy t _)     = namesInType t
namesInType (LitTy _)        = []
namesInType _                = []

getTypes :: [Type] -> [Name]
getTypes ts = concatMap namesInType ts

locationsAtPoint
  :: forall m a
   . MonadIO m
  => WithHieDb
  -> LookupModule m
  -> IdeOptions
  -> M.Map ModuleName NormalizedFilePath
  -> Position
  -> HieASTs a
  -> m [Location]
locationsAtPoint withHieDb lookupModule _ideOptions imports pos ast =
  let ns = concat $ pointCommand ast pos (M.keys . getNodeIds)
      zeroPos = Position 0 0
      zeroRange = Range zeroPos zeroPos
      modToLocation m = fmap (\fs -> pure $ Location (fromNormalizedUri $ filePathToUri' fs) zeroRange) $ M.lookup m imports
    in fmap (nubOrd . concat) $ mapMaybeM (either (pure . modToLocation) $ nameToLocation withHieDb lookupModule) ns

-- | Given a 'Name' attempt to find the location where it is defined.
nameToLocation :: MonadIO m => WithHieDb -> LookupModule m -> Name -> m (Maybe [Location])
nameToLocation withHieDb lookupModule name = runMaybeT $
  case nameSrcSpan name of
    sp@(RealSrcSpan rsp _)
      -- Lookup in the db if we got a location in a boot file
      | fs <- Util.unpackFS (srcSpanFile rsp)
      , not $ "boot" `isSuffixOf` fs
      -> do
          itExists <- liftIO $ doesFileExist fs
          if itExists
              then MaybeT $ pure $ fmap pure $ srcSpanToLocation sp
              -- When reusing .hie files from a cloud cache,
              -- the paths may not match the local file system.
              -- Let's fall back to the hiedb in case it contains local paths
              else fallbackToDb sp
    sp -> fallbackToDb sp
  where
    fallbackToDb sp = do
      guard (sp /= wiredInSrcSpan)
      -- This case usually arises when the definition is in an external package.
      -- In this case the interface files contain garbage source spans
      -- so we instead read the .hie files to get useful source spans.
      mod <- MaybeT $ return $ nameModule_maybe name
      erow <- liftIO $ withHieDb (\hieDb -> findDef hieDb (nameOccName name) (Just $ moduleName mod) (Just $ moduleUnit mod))
      case erow of
        [] -> do
          -- If the lookup failed, try again without specifying a unit-id.
          -- This is a hack to make find definition work better with ghcide's nascent multi-component support,
          -- where names from a component that has been indexed in a previous session but not loaded in this
          -- session may end up with different unit ids
          erow' <- liftIO $ withHieDb (\hieDb -> findDef hieDb (nameOccName name) (Just $ moduleName mod) Nothing)
          case erow' of
            [] -> MaybeT $ pure Nothing
            xs -> lift $ mapMaybeM (runMaybeT . defRowToLocation lookupModule) xs
        xs -> lift $ mapMaybeM (runMaybeT . defRowToLocation lookupModule) xs

defRowToLocation :: Monad m => LookupModule m -> Res DefRow -> MaybeT m Location
defRowToLocation lookupModule (row:.info) = do
  let start = Position (fromIntegral $ defSLine row - 1) (fromIntegral $ defSCol row - 1)
      end   = Position (fromIntegral $ defELine row - 1) (fromIntegral $ defECol row - 1)
      range = Range start end
  file <- case modInfoSrcFile info of
    Just src -> pure $ toUri src
    Nothing -> lookupModule (defSrc row) (modInfoName info) (modInfoUnit info) (modInfoIsBoot info)
  pure $ Location file range

toUri :: FilePath -> Uri
toUri = fromNormalizedUri . filePathToUri' . toNormalizedFilePath'

defRowToSymbolInfo :: Res DefRow -> Maybe SymbolInformation
defRowToSymbolInfo (DefRow{..}:.(modInfoSrcFile -> Just srcFile))
  = Just $ SymbolInformation (printOutputable defNameOcc) kind Nothing Nothing Nothing loc
  where
    kind
      | isVarOcc defNameOcc = SymbolKind_Variable
      | isDataOcc defNameOcc = SymbolKind_Constructor
      | isTcOcc defNameOcc = SymbolKind_Struct
        -- This used to be (SkUnknown 1), buth there is no SymbolKind_Unknown.
        -- Changing this to File, as that is enum representation of 1
      | otherwise = SymbolKind_File
    loc   = Location file range
    file  = fromNormalizedUri . filePathToUri' . toNormalizedFilePath' $ srcFile
    range = Range start end
    start = Position (fromIntegral $ defSLine - 1) (fromIntegral $ defSCol - 1)
    end   = Position (fromIntegral $ defELine - 1) (fromIntegral $ defECol - 1)
defRowToSymbolInfo _ = Nothing

pointCommand :: HieASTs t -> Position -> (HieAST t -> a) -> [a]
pointCommand hf pos k =
    M.elems $ flip M.mapMaybeWithKey (getAsts hf) $ \fs ast ->
      -- Since GHC 9.2:
      -- getAsts :: Map HiePath (HieAst a)
      -- type HiePath = LexicalFastString
      --
      -- but before:
      -- getAsts :: Map HiePath (HieAst a)
      -- type HiePath = FastString
      --
      -- 'coerce' here to avoid an additional function for maintaining
      -- backwards compatibility.
      case selectSmallestContaining (sp $ coerce fs) ast of
        Nothing   -> Nothing
        Just ast' -> Just $ k ast'
 where
   sloc fs = mkRealSrcLoc fs (fromIntegral $ line+1) (fromIntegral $ cha+1)
   sp fs = mkRealSrcSpan (sloc fs) (sloc fs)
   line :: UInt
   line = _line pos
   cha = _character pos

-- In ghc9, nodeInfo is monomorphic, so we need a case split here
nodeInfoH :: HieKind a -> HieAST a -> NodeInfo a
nodeInfoH (HieFromDisk _) = nodeInfo'
nodeInfoH HieFresh        = nodeInfo
