{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs        #-}
module Development.IDE.Plugin.CodeAction.ExactPrint (
  Rewrite (..),
  rewriteToEdit,
  rewriteToWEdit,
#if !MIN_VERSION_ghc(9,2,0)
  transferAnn,
#endif

  -- * Utilities
  appendConstraint,
  removeConstraint,
  extendImport,
  hideSymbol,
  liftParseAST,

  wildCardSymbol
) where

import           Control.Monad
import           Control.Monad.Trans
import           Data.Char                       (isAlphaNum)
import           Data.Data                       (Data)
import           Data.Generics                   (listify)
import qualified Data.Text                       as T
import           Development.IDE.GHC.Compat      hiding (Annotation)
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.ExactPrint
import           Development.IDE.GHC.Util
import           Development.IDE.Spans.Common
import           GHC.Exts                        (IsList (fromList))
import           GHC.Stack                       (HasCallStack)
import           Language.Haskell.GHC.ExactPrint
import           Language.LSP.Types

import           Development.IDE.Plugin.CodeAction.Util

-- GHC version specific imports. For any supported GHC version, make sure there is no warning in imports.
#if MIN_VERSION_ghc(9,2,0)
import           Control.Lens   (_head, _last, over)
import           Data.Bifunctor (first)
import           Data.Default   (Default (..))
import           Data.Maybe     (fromJust, fromMaybe, mapMaybe)
import           GHC            (AddEpAnn (..), AnnContext (..), AnnList (..),
                                 AnnParen (..), DeltaPos (SameLine), EpAnn (..),
                                 EpaLocation (EpaDelta),
                                 IsUnicodeSyntax (NormalSyntax),
                                 NameAdornment (NameParens),
                                 TrailingAnn (AddCommaAnn), addAnns, ann,
                                 emptyComments, noSrcSpanA, reAnnL)
import Language.Haskell.GHC.ExactPrint.ExactPrint      (makeDeltaAst, showAst)
#else
import           Control.Applicative                   (Alternative ((<|>)))
import           Control.Monad.Extra                   (whenJust)
import           Data.Foldable                         (find)
import           Data.Functor                          (($>))
import qualified Data.Map.Strict                       as Map
import           Data.Maybe                            (fromJust, isJust,
                                                        isNothing, mapMaybe)
import qualified Development.IDE.GHC.Compat.Util       as Util
import           Language.Haskell.GHC.ExactPrint.Types (DeltaPos (DP),
                                                        KeywordId (G), mkAnnKey)
#endif


------------------------------------------------------------------------------

-- | Construct a 'Rewrite', replacing the node at the given 'SrcSpan' with the
--   given 'ast'.
data Rewrite where
  Rewrite ::
#if !MIN_VERSION_ghc(9,2,0)
    Annotate ast =>
#else
    (ExactPrint (GenLocated (Anno ast) ast), ResetEntryDP (Anno ast), Outputable (GenLocated (Anno ast) ast), Data (GenLocated (Anno ast) ast)) =>
#endif
    -- | The 'SrcSpan' that we want to rewrite
    SrcSpan ->
    -- | The ast that we want to graft
#if !MIN_VERSION_ghc(9,2,0)
    (DynFlags -> TransformT (Either String) (Located ast)) ->
#else
    (DynFlags -> TransformT (Either String) (GenLocated (Anno ast) ast)) ->
#endif
    Rewrite

------------------------------------------------------------------------------
#if MIN_VERSION_ghc(9,2,0)
class ResetEntryDP ann where
    resetEntryDP :: GenLocated ann ast -> GenLocated ann ast
instance {-# OVERLAPPING #-} Default an => ResetEntryDP (SrcAnn an) where
    -- resetEntryDP = flip setEntryDP (SameLine 0)
    resetEntryDP (L srcAnn x) = setEntryDP (L srcAnn{ann=EpAnnNotUsed} x) (SameLine 0)
instance {-# OVERLAPPABLE #-} ResetEntryDP fallback where
    resetEntryDP = id
#endif

-- | Convert a 'Rewrite' into a list of '[TextEdit]'.
rewriteToEdit :: HasCallStack =>
  DynFlags ->
#if !MIN_VERSION_ghc(9,2,0)
  Anns ->
#endif
  Rewrite ->
  Either String [TextEdit]
rewriteToEdit dflags
#if !MIN_VERSION_ghc(9,2,0)
              anns
#endif
              (Rewrite dst f) = do
  (ast, anns , _) <- runTransformT
#if !MIN_VERSION_ghc(9,2,0)
                            anns
#endif
                          $ do
    ast <- f dflags
#if !MIN_VERSION_ghc(9,2,0)
    ast <$ setEntryDPT ast (DP (0, 0))
#else
    pure $ traceAst "REWRITE_result" $ resetEntryDP ast
#endif
  let editMap =
        [ TextEdit (fromJust $ srcSpanToRange dst) $
            T.pack $ exactPrint ast
#if !MIN_VERSION_ghc(9,2,0)
                       (fst anns)
#endif
        ]
  pure editMap

-- | Convert a 'Rewrite' into a 'WorkspaceEdit'
rewriteToWEdit :: DynFlags
               -> Uri
#if !MIN_VERSION_ghc(9,2,0)
               -> Anns
#endif
               -> Rewrite
               -> Either String WorkspaceEdit
rewriteToWEdit dflags uri
#if !MIN_VERSION_ghc(9,2,0)
               anns
#endif
               r = do
  edits <- rewriteToEdit dflags
#if !MIN_VERSION_ghc(9,2,0)
                         anns
#endif
                         r
  return $
    WorkspaceEdit
      { _changes = Just (fromList [(uri, List edits)])
      , _documentChanges = Nothing
      , _changeAnnotations = Nothing
      }

------------------------------------------------------------------------------

#if !MIN_VERSION_ghc(9,2,0)
-- | Fix the parentheses around a type context
fixParens ::
  (Monad m, Data (HsType pass), pass ~ GhcPass p0) =>
  Maybe DeltaPos ->
  Maybe DeltaPos ->
  LHsContext pass ->
  TransformT m [LHsType pass]
fixParens
          openDP closeDP
          ctxt@(L _ elems) = do
  -- Paren annotation for type contexts are usually quite screwed up
  -- we remove duplicates and fix negative DPs
  let parens = Map.fromList [(G AnnOpenP, dp00), (G AnnCloseP, dp00)]
  modifyAnnsT $
    Map.adjust
      ( \x ->
          let annsMap = Map.fromList (annsDP x)
           in x
                { annsDP =
                    Map.toList $
                      Map.alter (\_ -> openDP <|> Just dp00) (G AnnOpenP) $
                        Map.alter (\_ -> closeDP <|> Just dp00) (G AnnCloseP) $
                          annsMap <> parens
                }
      )
      (mkAnnKey ctxt)
  return $ map dropHsParTy elems
#endif

dropHsParTy :: LHsType (GhcPass pass) -> LHsType (GhcPass pass)
dropHsParTy (L _ (HsParTy _ ty)) = ty
dropHsParTy other                = other

removeConstraint ::
  -- | Predicate: Which context to drop.
  (LHsType GhcPs -> Bool) ->
  LHsType GhcPs ->
  Rewrite
removeConstraint toRemove = go . traceAst "REMOVE_CONSTRAINT_input"
  where
    go :: LHsType GhcPs -> Rewrite
#if MIN_VERSION_ghc(9,2,0) && !MIN_VERSION_ghc(9,4,0)
    go (L l it@HsQualTy{hst_ctxt = Just (L l' ctxt), hst_body}) = Rewrite (locA l) $ \_ -> do
#else
    go (L l it@HsQualTy{hst_ctxt = L l' ctxt, hst_body}) = Rewrite (locA l) $ \_ -> do
#endif
      let ctxt' = filter (not . toRemove) ctxt
          removeStuff = (toRemove <$> headMaybe ctxt) == Just True
#if MIN_VERSION_ghc(9,2,0)
      let hst_body' = if removeStuff then resetEntryDP hst_body else hst_body
      return $ case ctxt' of
          [] -> hst_body'
          _ -> do
            let ctxt'' = over _last (first removeComma) ctxt'
#if MIN_VERSION_ghc(9,4,0)
            L l $ it{ hst_ctxt = L l' ctxt''
#else
            L l $ it{ hst_ctxt = Just $ L l' ctxt''
#endif
                    , hst_body = hst_body'
                    }
#else
      when removeStuff  $
        setEntryDPT hst_body (DP (0, 0))
      return $ L l $ it{hst_ctxt =  L l' ctxt'}
#endif
    go (L _ (HsParTy _ ty)) = go ty
    go (L _ HsForAllTy{hst_body}) = go hst_body
    go (L l other) = Rewrite (locA l) $ \_ -> return $ L l other

-- | Append a constraint at the end of a type context.
--   If no context is present, a new one will be created.
appendConstraint ::
  -- | The new constraint to append
  String ->
  -- | The type signature where the constraint is to be inserted, also assuming annotated
  LHsType GhcPs ->
  Rewrite
appendConstraint constraintT = go . traceAst "appendConstraint"
 where
#if MIN_VERSION_ghc(9,4,0)
  go (L l it@HsQualTy{hst_ctxt = L l' ctxt}) = Rewrite (locA l) $ \df -> do
#elif MIN_VERSION_ghc(9,2,0)
  go (L l it@HsQualTy{hst_ctxt = Just (L l' ctxt)}) = Rewrite (locA l) $ \df -> do
#else
  go (L l it@HsQualTy{hst_ctxt = L l' ctxt}) = Rewrite (locA l) $ \df -> do
#endif
    constraint <- liftParseAST df constraintT
#if !MIN_VERSION_ghc(9,2,0)
    setEntryDPT constraint (DP (0, 1))

    -- Paren annotations are usually attached to the first and last constraints,
    -- rather than to the constraint list itself, so to preserve them we need to reposition them
    closeParenDP <- lookupAnn (G AnnCloseP) `mapM` lastMaybe ctxt
    openParenDP <- lookupAnn (G AnnOpenP) `mapM` headMaybe ctxt
    ctxt' <- fixParens
                (join openParenDP) (join closeParenDP)
                (L l' ctxt)
    addTrailingCommaT (last ctxt')
    return $ L l $ it{hst_ctxt = L l' $ ctxt' ++ [constraint]}
#else
    constraint <- pure $ setEntryDP constraint (SameLine 1)
    let l'' = (fmap.fmap) (addParensToCtxt close_dp) l'
    -- For singleton constraints, the close Paren DP is attached to an HsPar wrapping the constraint
    -- we have to reposition it manually into the AnnContext
        close_dp = case ctxt of
            [L _ (HsParTy EpAnn{anns=AnnParen{ap_close}} _)] -> Just ap_close
            _ -> Nothing
        ctxt' = over _last (first addComma) $ map dropHsParTy ctxt
#if MIN_VERSION_ghc(9,4,0)
    return $ L l $ it{hst_ctxt = L l'' $ ctxt' ++ [constraint]}
#else
    return $ L l $ it{hst_ctxt = Just $ L l'' $ ctxt' ++ [constraint]}
#endif
#endif
  go (L _ HsForAllTy{hst_body}) = go hst_body
  go (L _ (HsParTy _ ty)) = go ty
  go ast@(L l _) = Rewrite (locA l) $ \df -> do
    -- there isn't a context, so we must create one
    constraint <- liftParseAST df constraintT
    lContext <- uniqueSrcSpanT
    lTop <- uniqueSrcSpanT
#if MIN_VERSION_ghc(9,2,0)
#if MIN_VERSION_ghc(9,4,0)
    let context = reAnnL annCtxt emptyComments $ L lContext [resetEntryDP constraint]
#else
    let context = Just $ reAnnL annCtxt emptyComments $ L lContext [resetEntryDP constraint]
#endif
        annCtxt = AnnContext (Just (NormalSyntax, epl 1)) [epl 0 | needsParens] [epl 0 | needsParens]
        needsParens = hsTypeNeedsParens sigPrec $ unLoc constraint
    ast <- pure $ setEntryDP ast (SameLine 1)
#else
    let context = L lContext [constraint]
    addSimpleAnnT context dp00 $
      (G AnnDarrow, DP (0, 1)) :
      concat
        [ [ (G AnnOpenP, dp00)
          , (G AnnCloseP, dp00)
          ]
        | hsTypeNeedsParens sigPrec $ unLoc constraint
        ]
#endif

    return $ reLocA $ L lTop $ HsQualTy noExtField context ast

liftParseAST
    :: forall ast l.  (ASTElement l ast, ExactPrint (LocatedAn l ast))
    => DynFlags -> String -> TransformT (Either String) (LocatedAn l ast)
liftParseAST df s = case parseAST df "" s of
#if !MIN_VERSION_ghc(9,2,0)
  Right (anns, x) -> modifyAnnsT (anns <>) $> x
#else
  Right x ->  pure (makeDeltaAst x)
#endif
  Left _          -> lift $ Left $ "No parse: " <> s

#if !MIN_VERSION_ghc(9,2,0)
lookupAnn :: (Data a, Monad m)
          => KeywordId -> Located a -> TransformT m (Maybe DeltaPos)
lookupAnn comment la = do
  anns <- getAnnsT
  return $ Map.lookup (mkAnnKey la) anns >>= lookup comment . annsDP

dp00 :: DeltaPos
dp00 = DP (0, 0)

-- | Copy anns attached to a into b with modification, then delete anns of a
transferAnn :: (Data a, Data b) => Located a -> Located b -> (Annotation -> Annotation) -> TransformT (Either String) ()
transferAnn la lb f = do
  anns <- getAnnsT
  let oldKey = mkAnnKey la
      newKey = mkAnnKey lb
  oldValue <- liftMaybe "Unable to find ann" $ Map.lookup oldKey anns
  putAnnsT $ Map.delete oldKey $ Map.insert newKey (f oldValue) anns

#endif

headMaybe :: [a] -> Maybe a
headMaybe []      = Nothing
headMaybe (a : _) = Just a

lastMaybe :: [a] -> Maybe a
lastMaybe []    = Nothing
lastMaybe other = Just $ last other

liftMaybe :: String -> Maybe a -> TransformT (Either String) a
liftMaybe _ (Just x) = return x
liftMaybe s _        = lift $ Left s

------------------------------------------------------------------------------
extendImport :: Maybe String -> String -> LImportDecl GhcPs -> Rewrite
extendImport mparent identifier lDecl@(L l _) =
  Rewrite (locA l) $ \df -> do
    case mparent of
      -- This will also work for `ImportAllConstructors`
#if !MIN_VERSION_ghc(9,2,0)
      Just parent -> extendImportViaParent df parent identifier lDecl
      _           -> extendImportTopLevel identifier lDecl
#else
      -- Parsed source in GHC 9.4 uses absolute position annotation (RealSrcSpan),
      -- while rewriting relies on relative positions. ghc-exactprint has the utility
      -- makeDeltaAst for relativization.
      Just parent -> extendImportViaParent df parent identifier (makeDeltaAst lDecl)
      _           -> extendImportTopLevel identifier (makeDeltaAst lDecl)
#endif

-- | Add an identifier or a data type to import list. Expects a Delta AST
--
-- extendImportTopLevel "foo" AST:
--
-- import A --> Error
-- import A (foo) --> Error
-- import A (bar) --> import A (bar, foo)
extendImportTopLevel ::
  -- | rendered
  String ->
  LImportDecl GhcPs ->
  TransformT (Either String) (LImportDecl GhcPs)
extendImportTopLevel thing (L l it@ImportDecl{..})
  | Just (hide, L l' lies) <- ideclHiding
    , hasSibling <- not $ null lies = do
    src <- uniqueSrcSpanT
    top <- uniqueSrcSpanT
    let rdr = reLocA $ L src $ mkRdrUnqual $ mkVarOcc thing
    let alreadyImported =
          printOutputable (occName (unLoc rdr))
            `elem` map (printOutputable @OccName) (listify (const True) lies)
    when alreadyImported $
      lift (Left $ thing <> " already imported")

    let lie = reLocA $ L src $ IEName rdr
        x = reLocA $ L top $ IEVar noExtField lie

    if x `elem` lies
      then lift (Left $ thing <> " already imported")
      else do
#if !MIN_VERSION_ghc(9,2,0)
        anns <- getAnnsT
        maybe (pure ()) addTrailingCommaT (lastMaybe lies)
        addSimpleAnnT x (DP (0, if hasSibling then 1 else 0)) []
        addSimpleAnnT rdr dp00 [(G AnnVal, dp00)]

        -- When the last item already has a trailing comma, we append a trailing comma to the new item.
        let isAnnComma (G AnnComma, _) = True
            isAnnComma _               = False
            shouldAddTrailingComma = maybe False nodeHasComma (lastMaybe lies)
                && not (nodeHasComma (L l' lies))

            nodeHasComma :: Data a => Located a -> Bool
            nodeHasComma x = isJust $ Map.lookup (mkAnnKey x) anns >>= find isAnnComma . annsDP
        when shouldAddTrailingComma (addTrailingCommaT x)

        -- Parens are attached to `lies`, so if `lies` was empty previously,
        -- we need change the ann key from `[]` to `:` to keep parens and other anns.
        unless hasSibling $
          transferAnn (L l' lies) (L l' [x]) id
        return $ L l it{ideclHiding = Just (hide, L l' $ lies ++ [x])}
#else
        let lies' = addCommaInImportList lies x
        return $ L l it{ideclHiding = Just (hide, L l' lies')}
#endif
extendImportTopLevel _ _ = lift $ Left "Unable to extend the import list"

wildCardSymbol :: String
wildCardSymbol = ".."

-- | Add an identifier with its parent to import list
--
-- extendImportViaParent "Bar" "Cons" AST:
--
-- import A --> Error
-- import A (Bar(..)) --> Error
-- import A (Bar(Cons)) --> Error
-- import A () --> import A (Bar(Cons))
-- import A (Foo, Bar) --> import A (Foo, Bar(Cons))
-- import A (Foo, Bar()) --> import A (Foo, Bar(Cons))
--
-- extendImportViaParent "Bar" ".." AST:
-- import A () --> import A (Bar(..))
-- import A (Foo, Bar) -> import A (Foo, Bar(..))
-- import A (Foo, Bar()) -> import A (Foo, Bar(..))
extendImportViaParent ::
  DynFlags ->
  -- | parent (already parenthesized if needs)
  String ->
  -- | rendered child
  String ->
  LImportDecl GhcPs ->
  TransformT (Either String) (LImportDecl GhcPs)
extendImportViaParent df parent child (L l it@ImportDecl{..})
  | Just (hide, L l' lies) <- ideclHiding = go hide l' [] lies
 where
  go _hide _l' _pre ((L _ll' (IEThingAll _ (L _ ie))) : _xs)
    | parent == unIEWrappedName ie = lift . Left $ child <> " already included in " <> parent <> " imports"
  go hide l' pre (lAbs@(L ll' (IEThingAbs _ absIE@(L _ ie))) : xs)
    -- ThingAbs ie => ThingWith ie child
    | parent == unIEWrappedName ie = do
      srcChild <- uniqueSrcSpanT
      let childRdr = reLocA $ L srcChild $ mkRdrUnqual $ mkVarOcc child
          childLIE = reLocA $ L srcChild $ IEName childRdr
#if !MIN_VERSION_ghc(9,2,0)
          x :: LIE GhcPs = L ll' $ IEThingWith noExtField absIE NoIEWildcard [childLIE] []
      -- take anns from ThingAbs, and attach parens to it
      transferAnn lAbs x $ \old -> old{annsDP = annsDP old ++ [(G AnnOpenP, DP (0, 1)), (G AnnCloseP, dp00)]}
      addSimpleAnnT childRdr dp00 [(G AnnVal, dp00)]
#else
          x :: LIE GhcPs = L ll' $ IEThingWith (addAnns mempty [AddEpAnn AnnOpenP (EpaDelta (SameLine 1) []), AddEpAnn AnnCloseP def] emptyComments) absIE NoIEWildcard [childLIE]
#endif
      return $ L l it{ideclHiding = Just (hide, L l' $ reverse pre ++ [x] ++ xs)}
#if !MIN_VERSION_ghc(9,2,0)
  go hide l' pre ((L l'' (IEThingWith _ twIE@(L _ ie) _ lies' _)) : xs)
#else
  go hide l' pre ((L l'' (IEThingWith l''' twIE@(L _ ie) _ lies')) : xs)
#endif
    -- ThingWith ie lies' => ThingWith ie (lies' ++ [child])
    | parent == unIEWrappedName ie
    , child == wildCardSymbol = do
#if MIN_VERSION_ghc(9,2,0)
        let it' = it{ideclHiding = Just (hide, lies)}
            thing = IEThingWith newl twIE (IEWildcard 2) []
            newl = (\ann -> ann ++ [(AddEpAnn AnnDotdot d0)]) <$> l'''
            lies = L l' $ reverse pre ++ [L l'' thing] ++ xs
        return $ L l it'
#else
        let thing = L l'' (IEThingWith noExtField twIE (IEWildcard 2)  [] [])
        modifyAnnsT (Map.map (\ann -> ann{annsDP = (G AnnDotdot, dp00) : annsDP ann}))
        return $ L l it{ideclHiding = Just (hide, L l' $ reverse pre ++ [thing] ++ xs)}
#endif
    | parent == unIEWrappedName ie
    , hasSibling <- not $ null lies' =
      do
        srcChild <- uniqueSrcSpanT
        let childRdr = reLocA $ L srcChild $ mkRdrUnqual $ mkVarOcc child
#if MIN_VERSION_ghc(9,2,0)
        childRdr <- pure $ setEntryDP childRdr $ SameLine $ if hasSibling then 1 else 0
#endif
        let alreadyImported =
              printOutputable (occName (unLoc childRdr))
                `elem` map (printOutputable @OccName) (listify (const True) lies')
        when alreadyImported $
          lift (Left $ child <> " already included in " <> parent <> " imports")

        let childLIE = reLocA $ L srcChild $ IEName childRdr
#if !MIN_VERSION_ghc(9,2,0)
        when hasSibling $
          addTrailingCommaT (last lies')
        addSimpleAnnT childRdr (DP (0, if hasSibling then 1 else 0)) [(G AnnVal, dp00)]
        return $ L l it{ideclHiding = Just (hide, L l' $ reverse pre ++ [L l'' (IEThingWith noExtField twIE NoIEWildcard (lies' ++ [childLIE]) [])] ++ xs)}
#else
        let it' = it{ideclHiding = Just (hide, lies)}
            lies = L l' $ reverse pre ++
                [L l'' (IEThingWith l''' twIE NoIEWildcard (over _last fixLast lies' ++ [childLIE]))] ++ xs
            fixLast = if hasSibling then first addComma else id
        return $ L l it'
#endif
  go hide l' pre (x : xs) = go hide l' (x : pre) xs
  go hide l' pre []
    | hasSibling <- not $ null pre = do
      -- [] => ThingWith parent [child]
      l'' <- uniqueSrcSpanT
      srcParent <- uniqueSrcSpanT
      srcChild <- uniqueSrcSpanT
      parentRdr <- liftParseAST df parent
      let childRdr = reLocA $ L srcChild $ mkRdrUnqual $ mkVarOcc child
          isParentOperator = hasParen parent
#if !MIN_VERSION_ghc(9,2,0)
      when hasSibling $
        addTrailingCommaT (head pre)
      let parentLIE = L srcParent (if isParentOperator then IEType parentRdr else IEName parentRdr)
          childLIE = reLocA $ L srcChild $ IEName childRdr
#else
      let parentLIE = reLocA $ L srcParent $ (if isParentOperator then IEType (epl 0) parentRdr' else IEName parentRdr')
          parentRdr' = modifyAnns parentRdr $ \case
              it@NameAnn{nann_adornment = NameParens} -> it{nann_open = epl 1, nann_close = epl 0}
              other -> other
          childLIE = reLocA $ L srcChild $ IEName childRdr
#endif
#if !MIN_VERSION_ghc(9,2,0)
          x :: LIE GhcPs = reLocA $ L l'' $ IEThingWith noExtField parentLIE NoIEWildcard [childLIE] []
      -- Add AnnType for the parent if it's parenthesized (type operator)
      when isParentOperator $
        addSimpleAnnT parentLIE (DP (0, 0)) [(G AnnType, DP (0, 0))]
      addSimpleAnnT parentRdr (DP (0, if hasSibling then 1 else 0)) $ unqalDP 1 isParentOperator
      addSimpleAnnT childRdr (DP (0, 0)) [(G AnnVal, dp00)]
      addSimpleAnnT x (DP (0, 0)) [(G AnnOpenP, DP (0, 1)), (G AnnCloseP, DP (0, 0))]
      -- Parens are attached to `pre`, so if `pre` was empty previously,
      -- we need change the ann key from `[]` to `:` to keep parens and other anns.
      unless hasSibling $
        transferAnn (L l' $ reverse pre) (L l' [x]) id

      let lies' = reverse pre ++ [x]
#else
          listAnn = epAnn srcParent [AddEpAnn AnnOpenP (epl 1), AddEpAnn AnnCloseP (epl 0)]
          x :: LIE GhcPs = reLocA $ L l'' $ IEThingWith listAnn parentLIE NoIEWildcard [childLIE]

          lies' = addCommaInImportList (reverse pre) x
#endif
      return $ L l it{ideclHiding = Just (hide, L l' lies')}
extendImportViaParent _ _ _ _ = lift $ Left "Unable to extend the import list via parent"

#if MIN_VERSION_ghc(9,2,0)
-- Add an item in an import list, taking care of adding comma if needed.
addCommaInImportList ::
  -- | Initial list
  [LocatedAn AnnListItem a]
  -- | Additional item
  -> LocatedAn AnnListItem a
  -> [LocatedAn AnnListItem a]
addCommaInImportList lies x =
    fixLast lies ++ [newItem]
  where
    isTrailingAnnComma :: TrailingAnn -> Bool
    isTrailingAnnComma (AddCommaAnn _) = True
    isTrailingAnnComma _ = False

    -- check if there is an existing trailing comma
    existingTrailingComma = fromMaybe False $ do
        L lastItemSrcAnn _ <- lastMaybe lies
        lastItemAnn <- case ann lastItemSrcAnn of
            EpAnn _ lastItemAnn _ -> pure lastItemAnn
            _ -> Nothing
        pure $ any isTrailingAnnComma (lann_trailing lastItemAnn)

    hasSibling = not . null $ lies

    -- Setup the new item. It should have a preceding whitespace if it has siblings, and a trailing comma if the
    -- preceding item already has one.
    newItem = first (if existingTrailingComma then addComma else id) $
        setEntryDP x (SameLine $ if hasSibling then 1 else 0)

    -- Add the comma (if needed)
    fixLast :: [LocatedAn AnnListItem a] -> [LocatedAn AnnListItem a]
    fixLast = over _last (first (if existingTrailingComma then id else addComma))
#endif

unIEWrappedName :: IEWrappedName (IdP GhcPs) -> String
unIEWrappedName (occName -> occ) = T.unpack $ printOutputable $ parenSymOcc occ (ppr occ)

hasParen :: String -> Bool
hasParen ('(' : _) = True
hasParen _         = False

#if !MIN_VERSION_ghc(9,2,0)
unqalDP :: Int -> Bool -> [(KeywordId, DeltaPos)]
unqalDP c paren =
  ( if paren
      then \x -> (G AnnOpenP, DP (0, c)) : x : [(G AnnCloseP, dp00)]
      else pure
  )
    (G AnnVal, dp00)
#endif

------------------------------------------------------------------------------

-- | Hide a symbol from import declaration
hideSymbol ::
  String -> LImportDecl GhcPs -> Rewrite
hideSymbol symbol lidecl@(L loc ImportDecl{..}) =
  case ideclHiding of
    Nothing -> Rewrite (locA loc) $ extendHiding symbol lidecl Nothing
    Just (True, hides) -> Rewrite (locA loc) $ extendHiding symbol lidecl (Just hides)
    Just (False, imports) -> Rewrite (locA loc) $ deleteFromImport symbol lidecl imports
hideSymbol _ (L _ (XImportDecl _)) =
  error "cannot happen"

extendHiding ::
  String ->
  LImportDecl GhcPs ->
#if !MIN_VERSION_ghc(9,2,0)
  Maybe (Located [LIE GhcPs]) ->
#else
  Maybe (XRec GhcPs [LIE GhcPs]) ->
#endif
  DynFlags ->
  TransformT (Either String) (LImportDecl GhcPs)
extendHiding symbol (L l idecls) mlies df = do
  L l' lies <- case mlies of
#if !MIN_VERSION_ghc(9,2,0)
    Nothing -> flip L [] <$> uniqueSrcSpanT
#else
    Nothing -> do
        src <- uniqueSrcSpanT
        let ann = noAnnSrcSpanDP0 src
            ann' = flip (fmap.fmap) ann $ \x -> x
                {al_rest = [AddEpAnn AnnHiding (epl 1)]
                ,al_open = Just $ AddEpAnn AnnOpenP (epl 1)
                ,al_close = Just $ AddEpAnn AnnCloseP (epl 0)
                }
        return $ L ann' []
#endif
    Just pr -> pure pr
  let hasSibling = not $ null lies
  src <- uniqueSrcSpanT
  top <- uniqueSrcSpanT
  rdr <- liftParseAST df symbol
#if MIN_VERSION_ghc(9,2,0)
  rdr <- pure $ modifyAnns rdr $ addParens (isOperator $ unLoc rdr)
#endif
  let lie = reLocA $ L src $ IEName rdr
      x = reLocA $ L top $ IEVar noExtField lie
#if MIN_VERSION_ghc(9,2,0)
  x <- pure $ if hasSibling then first addComma x else x
  lies <- pure $ over _head (`setEntryDP` SameLine 1) lies
#endif
#if !MIN_VERSION_ghc(9,2,0)
      singleHide = L l' [x]
  when (isNothing mlies) $ do
    addSimpleAnnT
      singleHide
      dp00
      [ (G AnnHiding, DP (0, 1))
      , (G AnnOpenP, DP (0, 1))
      , (G AnnCloseP, DP (0, 0))
      ]
  addSimpleAnnT x (DP (0, 0)) []
  addSimpleAnnT rdr dp00 $ unqalDP 0 $ isOperator $ unLoc rdr
  if hasSibling
    then do
      addTrailingCommaT x
      addSimpleAnnT (head lies) (DP (0, 1)) []
      unless (null $ tail lies) $
        addTrailingCommaT (head lies) -- Why we need this?
    else forM_ mlies $ \lies0 -> do
      transferAnn lies0 singleHide id
#endif
  return $ L l idecls{ideclHiding = Just (True, L l' $ x : lies)}
 where
  isOperator = not . all isAlphaNum . occNameString . rdrNameOcc

deleteFromImport ::
  String ->
  LImportDecl GhcPs ->
#if !MIN_VERSION_ghc(9,2,0)
  Located [LIE GhcPs] ->
#else
  XRec GhcPs [LIE GhcPs] ->
#endif
  DynFlags ->
  TransformT (Either String) (LImportDecl GhcPs)
deleteFromImport (T.pack -> symbol) (L l idecl) llies@(L lieLoc lies) _ = do
  let edited = L lieLoc deletedLies
      lidecl' =
        L l $
          idecl
            { ideclHiding = Just (False, edited)
            }
#if !MIN_VERSION_ghc(9,2,0)
  -- avoid import A (foo,)
  whenJust (lastMaybe deletedLies) removeTrailingCommaT
  when (not (null lies) && null deletedLies) $ do
    transferAnn llies edited id
    addSimpleAnnT
      edited
      dp00
      [ (G AnnOpenP, DP (0, 1))
      , (G AnnCloseP, DP (0, 0))
      ]
#endif
  pure lidecl'
 where
  deletedLies =
#if MIN_VERSION_ghc(9,2,0)
    over _last removeTrailingComma $
#endif
    mapMaybe killLie lies
  killLie :: LIE GhcPs -> Maybe (LIE GhcPs)
  killLie v@(L _ (IEVar _ (L _ (unqualIEWrapName -> nam))))
    | nam == symbol = Nothing
    | otherwise = Just v
  killLie v@(L _ (IEThingAbs _ (L _ (unqualIEWrapName -> nam))))
    | nam == symbol = Nothing
    | otherwise = Just v
#if !MIN_VERSION_ghc(9,2,0)
  killLie (L lieL (IEThingWith xt ty@(L _ (unqualIEWrapName -> nam)) wild cons flds))
#else
  killLie (L lieL (IEThingWith xt ty@(L _ (unqualIEWrapName -> nam)) wild cons))
#endif
    | nam == symbol = Nothing
    | otherwise =
      Just $
        L lieL $
          IEThingWith
            xt
            ty
            wild
            (filter ((/= symbol) . unqualIEWrapName . unLoc) cons)
#if !MIN_VERSION_ghc(9,2,0)
            (filter ((/= symbol) . T.pack . Util.unpackFS . flLabel . unLoc) flds)
#endif
  killLie v = Just v
