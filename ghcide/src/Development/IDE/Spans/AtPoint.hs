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
  , gotoImplementation
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


import           GHC.Data.FastString                  (lengthFS)
import qualified GHC.Utils.Outputable                 as O

import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Orphans          ()
import           Development.IDE.Types.Location
import           Language.LSP.Protocol.Types          hiding
                                                      (SemanticTokenAbsolute (..))
import           Prelude                              hiding (mod)

-- compiler and infrastructure
import           Development.IDE.Core.Compile         (setNonHomeFCHook)
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.RuleTypes
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat.Util      as Util
import           Development.IDE.GHC.Util             (printOutputable)
import           Development.IDE.Spans.Common
import           Development.IDE.Types.Options

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
import           Data.List.Extra                      (dropEnd1, nubOrd)


import           Control.Lens                         ((^.))
import           Data.Either.Extra                    (eitherToMaybe)
import           Data.List                            (isSuffixOf, sortOn)
import           Data.Set                             (Set)
import qualified Data.Set                             as S
import           Data.Tree
import qualified Data.Tree                            as T
import           Data.Version                         (showVersion)
import           Development.IDE.Core.LookupMod       (LookupModule, lookupMod)
import           Development.IDE.Core.Shake           (ShakeExtras (..),
                                                       runIdeAction)
import           Development.IDE.Types.Shake          (WithHieDb)
import           GHC.Iface.Ext.Types                  (EvVarSource (..),
                                                       HieAST (..),
                                                       HieASTs (..),
                                                       HieArgs (..),
                                                       HieType (..),
                                                       HieTypeFix (..),
                                                       Identifier,
                                                       IdentifierDetails (..),
                                                       NodeInfo (..), Scope,
                                                       Span)
import           GHC.Iface.Ext.Utils                  (EvidenceInfo (..),
                                                       RefMap, getEvidenceTree,
                                                       getScopeFromContext,
                                                       hieTypeToIface,
                                                       isEvidenceContext,
                                                       isEvidenceUse,
                                                       isOccurrence, nodeInfo,
                                                       recoverFullType,
                                                       selectSmallestContaining)
import           HieDb                                hiding (pointCommand,
                                                       withHieDb)
import qualified Language.LSP.Protocol.Lens           as L
import           System.Directory                     (doesFileExist)

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
  concat $ pointCommand hf posFile (rights . M.keys . getSourceNodeIds)
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
      maybeToList (makeHighlight n ref)
    makeHighlight n (sp,dets)
      | isTvNameSpace (nameNameSpace n) && isBadSpan n sp = Nothing
      | otherwise = Just $ DocumentHighlight (realSrcSpanToRange sp) (Just $ highlightType $ identInfo dets)
    highlightType s =
      if any (isJust . getScopeFromContext) s
        then DocumentHighlightKind_Write
        else DocumentHighlightKind_Read

    isBadSpan :: Name -> RealSrcSpan -> Bool
    isBadSpan n sp = srcSpanStartLine sp /= srcSpanEndLine sp || (srcSpanEndCol sp - srcSpanStartCol sp > lengthFS (occNameFS $ nameOccName n))

-- | Locate the type definition of the name at a given position.
gotoTypeDefinition
  :: MonadIO m
  => WithHieDb
  -> LookupModule m
  -> IdeOptions
  -> HieAstResult
  -> Position
  -> MaybeT m [(Location, Identifier)]
gotoTypeDefinition withHieDb lookupModule ideOpts srcSpans pos
  = lift $ typeLocationsAtPoint withHieDb lookupModule ideOpts pos srcSpans

-- | Locate the definition of the name at a given position.
gotoDefinition
  :: MonadIO m
  => WithHieDb
  -> LookupModule m
  -> IdeOptions
  -> M.Map ModuleName NormalizedFilePath
  -> HieAstResult
  -> Position
  -> MaybeT m [(Location, Identifier)]
gotoDefinition withHieDb getHieFile ideOpts imports srcSpans pos
  = lift $ locationsAtPoint withHieDb getHieFile ideOpts imports pos srcSpans

-- | Locate the implementation definition of the name at a given position.
-- Goto Implementation for an overloaded function.
gotoImplementation
  :: MonadIO m
  => WithHieDb
  -> LookupModule m
  -> IdeOptions
  -> HieAstResult
  -> Position
  -> MaybeT m [Location]
gotoImplementation withHieDb getHieFile ideOpts srcSpans pos
  = lift $ instanceLocationsAtPoint withHieDb getHieFile ideOpts pos srcSpans

-- | Synopsis for the name at a given position.
atPoint
  :: IdeOptions
  -> ShakeExtras
  -> HieAstResult
  -> DocAndTyThingMap
  -> HscEnv
  -> Position
  -> IO (Maybe (Maybe Range, [T.Text]))
atPoint opts@IdeOptions{} shakeExtras@ShakeExtras{ withHieDb, hiedbWriter } har@(HAR _ (hf :: HieASTs a) rf _ (kind :: HieKind hietype)) (DKMap dm km _am) env pos =
    listToMaybe <$> sequence (pointCommand hf pos hoverInfo)
  where
    -- Hover info for values/data
    hoverInfo :: HieAST hietype -> IO (Maybe Range, [T.Text])
    hoverInfo ast = do
        locationsWithIdentifier <- runIdeAction "TypeCheck" shakeExtras $ do
          runMaybeT $ gotoTypeDefinition withHieDb (lookupMod hiedbWriter) opts har pos

        let locationsMap = M.fromList $ mapMaybe (\(loc, identifier) -> case identifier of
              Right typeName ->
                -- Filter out type variables (polymorphic names like 'a', 'b', etc.)
                if isTyVarName typeName
                  then Nothing
                  else Just (typeName, loc)
              Left _moduleName -> Nothing) $ fromMaybe [] locationsWithIdentifier

        prettyNames <- mapM (prettyName locationsMap) names
        pure (Just range, prettyNames ++ pTypes locationsMap)
      where
        pTypes :: M.Map Name Location -> [T.Text]
        pTypes locationsMap =
          case names of
            [_singleName] -> dropEnd1 $ prettyTypes Nothing locationsMap
            _             -> prettyTypes Nothing locationsMap

        range :: Range
        range = realSrcSpanToRange $ nodeSpan ast

        info :: NodeInfo hietype
        info = nodeInfoH kind ast

        -- We want evidence variables to be displayed last.
        -- Evidence trees contain information of secondary relevance.
        names :: [(Identifier, IdentifierDetails hietype)]
        names = sortOn (any isEvidenceUse . identInfo . snd) $ M.assocs $ nodeIdentifiers info

        prettyName :: M.Map Name Location -> (Either ModuleName Name, IdentifierDetails hietype) -> IO T.Text
        prettyName locationsMap (Right n, dets)
          -- We want to print evidence variable using a readable tree structure.
          -- Evidence variables contain information why a particular instance or
          -- type equality was chosen, paired with location information.
          | any isEvidenceUse (identInfo dets) =
            let
              -- The evidence tree may not be present for some reason, e.g., the 'Name' is not
              -- present in the tree.
              -- Thus, we need to handle it here, but in practice, this should never be 'Nothing'.
              evidenceTree = maybe "" (printOutputable . renderEvidenceTree) (getEvidenceTree rf n)
            in
              pure $ evidenceTree <> "\n"
          -- Identifier details that are not evidence variables are used to display type information and
          -- documentation of that name.
          | otherwise = do
            let
              typeSig = case identType dets of
                Just t -> prettyType (Just n) locationsMap t
                Nothing -> case safeTyThingType =<< lookupNameEnv km n of
                  Just kind -> prettyTypeFromType (Just n) locationsMap kind
                  Nothing   -> wrapHaskell (printOutputable n)
              definitionLoc = maybeToList (pretty (definedAt n) (prettyPackageName n))
              docs = maybeToList (T.unlines . spanDocToMarkdown <$> lookupNameEnv dm n)

            pure $ T.unlines $ [typeSig] ++ definitionLoc ++ docs
          where
                pretty Nothing Nothing = Nothing
                pretty (Just define) Nothing = Just $ define <> "\n"
                pretty Nothing (Just pkgName) = Just $ pkgName <> "\n"
                pretty (Just define) (Just pkgName) = Just $ define <> " " <> pkgName <> "\n"
        prettyName _locationsMap (Left m,_) = packageNameForImportStatement m

        prettyPackageName :: Name -> Maybe T.Text
        prettyPackageName n = do
          m <- nameModule_maybe n
          pkgTxt <- packageNameWithVersion m
          pure $ "*(" <> pkgTxt <> ")*"

        -- Return the module text itself and
        -- the package(with version) this `ModuleName` belongs to.
        packageNameForImportStatement :: ModuleName -> IO T.Text
        packageNameForImportStatement mod = do
          mpkg <- findImportedModule (setNonHomeFCHook env) mod :: IO (Maybe Module)
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

        -- Type info for the current node, it may contain several symbols
        -- for one range, like wildcard
        types :: [hietype]
        types = take maxHoverTypes $ nodeType info

        maxHoverTypes :: Int
        maxHoverTypes = 10

        prettyTypes :: Maybe Name -> M.Map Name Location -> [T.Text]
        prettyTypes boundNameMay locationsMap =
          map (prettyType boundNameMay locationsMap) types

        prettyTypeFromType :: Maybe Name -> M.Map Name Location -> Type -> T.Text
        prettyTypeFromType boundNameMay locationsMap ty =
          prettyTypeCommon boundNameMay locationsMap (S.fromList $ namesInType ty) (printOutputable ty)

        prettyType :: Maybe Name -> M.Map Name Location -> hietype -> T.Text
        prettyType boundNameMay locationsMap t =
          prettyTypeCommon boundNameMay locationsMap (typeNames t) (printOutputable . expandType $ t)

        prettyTypeCommon :: Maybe Name -> M.Map Name Location -> Set Name -> T.Text -> T.Text
        prettyTypeCommon boundNameMay locationsMap names expandedType =
          let nameToUse = case boundNameMay of
                Just n  -> printOutputable n
                Nothing -> "_"
              expandedWithName = nameToUse <> " :: " <> expandedType
              codeBlock = wrapHaskell expandedWithName
              links = case boundNameMay of
                Just _  -> generateLinksList locationsMap names
                -- This is so we don't get flooded with links, e.g:
                -- foo :: forall a. MyType a -> a
                -- Go to MyType
                -- _ :: forall a. MyType a -> a
                -- Go to MyType -- <- we don't want this as it's already present
                Nothing -> ""
          in codeBlock <> links

        generateLinksList :: M.Map Name Location -> Set Name -> T.Text
        generateLinksList locationsMap (S.toList -> names) =
          if null generated
            then ""
            else "\n" <> "Go to " <> T.intercalate " | " generated <> "\n"
          where
            generated = mapMaybe generateLink names

            generateLink name = do
              case M.lookup name locationsMap of
                Just (Location uri range) ->
                  let nameText = printOutputable name
                      link = "[" <> nameText <> "](" <> getUriText uri <> "#L" <>
                             T.pack (show (range ^. L.start . L.line + 1)) <> ")"
                  in Just link
                Nothing -> Nothing

        wrapHaskell :: T.Text -> T.Text
        wrapHaskell x = "\n```haskell\n"<>x<>"\n```\n"

        getUriText :: Uri -> T.Text
        getUriText (Uri t) = t

        typeNames :: a -> Set Name
        typeNames t = S.fromList $ case kind of
          HieFresh -> namesInType t
          HieFromDisk full_file -> do
            namesInHieTypeFix $ recoverFullType t (hie_types full_file)

        expandType :: a -> SDoc
        expandType t = case kind of
          HieFresh -> ppr t
          HieFromDisk full_file -> ppr $ hieTypeToIface $ recoverFullType t (hie_types full_file)

        definedAt :: Name -> Maybe T.Text
        definedAt name =
          -- do not show "at <no location info>" and similar messages
          -- see the code of 'pprNameDefnLoc' for more information
          case nameSrcLoc name of
            UnhelpfulLoc {} | isInternalName name || isSystemName name -> Nothing
            _ -> Just $ "*Defined " <> printOutputable (pprNameDefnLoc name) <> "*"

        -- We want to render the root constraint even if it is a let,
        -- but we don't want to render any subsequent lets
        renderEvidenceTree :: Tree (EvidenceInfo a) -> SDoc
        -- However, if the root constraint is simply a<n indirection (via let) to a single other constraint,
        -- we can still skip rendering it
        -- The evidence ghc generates is made up of a few primitives, like @WpLet@ (let bindings),
        -- @WpEvLam@ (lambda abstractions) and so on.
        -- The let binding refers to these lets.
        --
        -- For example, evidence for @Show ([Int], Bool)@ might look like:
        --
        -- @
        --   $dShow,[]IntBool = $fShow,[]IntBool
        --   -- indirection, we don't gain anything by printing this
        --   $fShow,[]IntBool = $dShow, $fShow[]Int $fShowBool
        --   -- This is the root "let" we render as a tree
        --   $fShow[]Int = $dShow[] $fShowInt
        --   -- second level let, collapse it into its parent $fShow,[]IntBool
        --   $fShowInt = base:Data.Int.$dShowInt
        --   -- indirection, remove it
        --   $fShowBool = base:Data.Bool.$dShowBool
        --   -- indirection, remove it
        --
        --   in $dShow,[]IntBool
        -- @
        --
        -- On doing this we end up with the tree @Show ([Int], Bool) -> (Show (,), Show [], Show Int, Show Bool)@
        --
        -- It is also quite helpful to look at the @.hie@ file directly to see how the
        -- evidence information is presented on disk. @hiedb dump <mod.hie>@
        renderEvidenceTree (T.Node (EvidenceInfo{evidenceDetails=Just (EvLetBind _,_,_)}) [x])
          = renderEvidenceTree x
        renderEvidenceTree (T.Node (EvidenceInfo{evidenceDetails=Just (EvLetBind _,_,_), ..}) xs)
          = hang (text "Evidence of constraint `" O.<> expandType evidenceType O.<> "`") 2 $
                 vcat $ text "constructed using:" : map renderEvidenceTree' xs
        renderEvidenceTree (T.Node (EvidenceInfo{..}) _)
          = hang (text "Evidence of constraint `" O.<> expandType evidenceType O.<> "`") 2 $
                 vcat $ printDets evidenceSpan evidenceDetails : map (text . T.unpack) (maybeToList $ definedAt evidenceVar)

        -- renderEvidenceTree' skips let bound evidence variables and prints the children directly
        renderEvidenceTree' (T.Node (EvidenceInfo{evidenceDetails=Just (EvLetBind _,_,_)}) xs)
          = vcat (map renderEvidenceTree' xs)
        renderEvidenceTree' (T.Node (EvidenceInfo{..}) _)
          = hang (text "- `" O.<> expandType evidenceType O.<> "`") 2 $
              vcat $
                printDets evidenceSpan evidenceDetails : map (text . T.unpack) (maybeToList $ definedAt evidenceVar)

        printDets :: RealSrcSpan -> Maybe (EvVarSource, Scope, Maybe Span) -> SDoc
        printDets _    Nothing = text "using an external instance"
        printDets ospn (Just (src,_,mspn)) = pprSrc
                                      $$ text "at" <+> text (T.unpack $ srcSpanToMdLink location)
          where
            location = realSrcSpanToLocation spn
            -- Use the bind span if we have one, else use the occurrence span
            spn = fromMaybe ospn mspn
            pprSrc = case src of
              -- Users don't know what HsWrappers are
              EvWrapperBind -> "bound by type signature or pattern"
              _             -> ppr src

-- | Find 'Location's of type definition at a specific point and return them along with their 'Identifier's.
typeLocationsAtPoint
  :: forall m
   . MonadIO m
  => WithHieDb
  -> LookupModule m
  -> IdeOptions
  -> Position
  -> HieAstResult
  -> m [(Location, Identifier)]
typeLocationsAtPoint withHieDb lookupModule _ideOptions pos (HAR _ ast _ _ hieKind) =
  case hieKind of
    HieFromDisk hf ->
      let arr = hie_types hf
          ts = concat $ pointCommand ast pos getts
          unfold = map (arr A.!)
          getts x = nodeType ni  ++ mapMaybe identType (M.elems $ nodeIdentifiers ni)
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
        in fmap nubOrd $ concatMapM (\n -> fmap (maybe [] (fmap (,Right n))) (nameToLocation withHieDb lookupModule n)) (getTypes' ts)
    HieFresh ->
      let ts = concat $ pointCommand ast pos getts
          getts x = nodeType ni  ++ mapMaybe identType (M.elems $ nodeIdentifiers ni)
            where ni = nodeInfo x
        in fmap nubOrd $ concatMapM (\n -> fmap (maybe [] (fmap (,Right n))) (nameToLocation withHieDb lookupModule n)) (getTypes ts)

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
getTypes = concatMap namesInType

namesInHieTypeFix :: HieTypeFix -> [Name]
namesInHieTypeFix (Roll hieType) = namesInHieType hieType

namesInHieType :: HieType HieTypeFix -> [Name]
namesInHieType (HTyVarTy n)         = [n]
namesInHieType (HAppTy a (HieArgs args)) = namesInHieTypeFix a ++ concatMap (namesInHieTypeFix . snd) args
namesInHieType (HTyConApp tc (HieArgs args)) = ifaceTyConName tc : concatMap (namesInHieTypeFix . snd) args
namesInHieType (HForAllTy ((binder, constraint), _) body) = binder : namesInHieTypeFix constraint ++ namesInHieTypeFix body
namesInHieType (HFunTy mult arg res) = namesInHieTypeFix mult ++ namesInHieTypeFix arg ++ namesInHieTypeFix res
namesInHieType (HQualTy constraint body) = namesInHieTypeFix constraint ++ namesInHieTypeFix body
namesInHieType (HLitTy _)           = []
namesInHieType (HCastTy a)          = namesInHieTypeFix a
namesInHieType HCoercionTy          = []

-- | Find 'Location's of definition at a specific point and return them along with their 'Identifier's.
locationsAtPoint
  :: forall m
   . MonadIO m
  => WithHieDb
  -> LookupModule m
  -> IdeOptions
  -> M.Map ModuleName NormalizedFilePath
  -> Position
  -> HieAstResult
  -> m [(Location, Identifier)]
locationsAtPoint withHieDb lookupModule _ideOptions imports pos (HAR _ ast _rm _ _) =
  let ns = concat $ pointCommand ast pos (M.keys . getNodeIds)
      zeroPos = Position 0 0
      zeroRange = Range zeroPos zeroPos
      modToLocation m = fmap (\fs -> pure (Location (fromNormalizedUri $ filePathToUri' fs) zeroRange)) $ M.lookup m imports
   in fmap (nubOrd . concat) $ mapMaybeM
        (either (\m -> pure ((fmap $ fmap (,Left m)) (modToLocation m)))
                (\n -> fmap (fmap $ fmap (,Right n)) (nameToLocation withHieDb lookupModule n)))
        ns

-- | Find 'Location's of a implementation definition at a specific point.
instanceLocationsAtPoint
  :: forall m
   . MonadIO m
  => WithHieDb
  -> LookupModule m
  -> IdeOptions
  -> Position
  -> HieAstResult
  -> m [Location]
instanceLocationsAtPoint withHieDb lookupModule _ideOptions pos (HAR _ ast _rm _ _) =
  let ns = concat $ pointCommand ast pos (M.keys . getNodeIds)
      evTrees = mapMaybe (eitherToMaybe >=> getEvidenceTree _rm) ns
      evNs = concatMap (map evidenceVar . T.flatten) evTrees
   in fmap (nubOrd . concat) $ mapMaybeM
        (nameToLocation withHieDb lookupModule)
        evNs

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
