{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}

module Development.IDE.Plugin.CodeAction.ExactPrint (
  Rewrite (..),
  rewriteToEdit,
  rewriteToWEdit,
  transferAnn,

  -- * Utilities
  appendConstraint,
  extendImport,
  hideSymbol,
  liftParseAST,
) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Extra                   (whenJust)
import           Control.Monad.Trans
import           Data.Char                             (isAlphaNum)
import           Data.Data                             (Data)
import           Data.Functor
import           Data.Generics                         (listify)
import qualified Data.Map.Strict                       as Map
import           Data.Maybe                            (fromJust, isNothing,
                                                        mapMaybe)
import qualified Data.Text                             as T
import           Development.IDE.GHC.Compat            hiding (parseExpr)
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.ExactPrint        (ASTElement (parseAST),
                                                        Annotate)
import           Development.IDE.Spans.Common
import           FieldLabel                            (flLabel)
import           GHC.Exts                              (IsList (fromList))
import           GhcPlugins                            (sigPrec)
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Types (DeltaPos (DP),
                                                        KeywordId (G), mkAnnKey)
import           Language.LSP.Types
import           OccName
import           Outputable                            (ppr, showSDocUnsafe)
import           Retrie.GHC                            (rdrNameOcc, unpackFS)

------------------------------------------------------------------------------

-- | Construct a 'Rewrite', replacing the node at the given 'SrcSpan' with the
--   given 'ast'.
data Rewrite where
  Rewrite ::
    Annotate ast =>
    -- | The 'SrcSpan' that we want to rewrite
    SrcSpan ->
    -- | The ast that we want to graft
    (DynFlags -> TransformT (Either String) (Located ast)) ->
    Rewrite

------------------------------------------------------------------------------

-- | Convert a 'Rewrite' into a list of '[TextEdit]'.
rewriteToEdit ::
  DynFlags ->
  Anns ->
  Rewrite ->
  Either String [TextEdit]
rewriteToEdit dflags anns (Rewrite dst f) = do
  (ast, (anns, _), _) <- runTransformT anns $ do
    ast <- f dflags
    ast <$ setEntryDPT ast (DP (0, 0))
  let editMap =
        [ TextEdit (fromJust $ srcSpanToRange dst) $
            T.pack $ exactPrint ast anns
        ]
  pure editMap

-- | Convert a 'Rewrite' into a 'WorkspaceEdit'
rewriteToWEdit :: DynFlags -> Uri -> Anns -> Rewrite -> Either String WorkspaceEdit
rewriteToWEdit dflags uri anns r = do
  edits <- rewriteToEdit dflags anns r
  return $
    WorkspaceEdit
      { _changes = Just (fromList [(uri, List edits)])
      , _documentChanges = Nothing
      , _changeAnnotations = Nothing
      }

------------------------------------------------------------------------------

-- | Fix the parentheses around a type context
fixParens ::
  (Monad m, Data (HsType pass)) =>
  Maybe DeltaPos ->
  Maybe DeltaPos ->
  LHsContext pass ->
  TransformT m [LHsType pass]
fixParens openDP closeDP ctxt@(L _ elems) = do
  -- Paren annotation for type contexts are usually quite screwed up
  -- we remove duplicates and fix negative DPs
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
 where
  parens = Map.fromList [(G AnnOpenP, dp00), (G AnnCloseP, dp00)]

  dropHsParTy :: LHsType pass -> LHsType pass
  dropHsParTy (L _ (HsParTy _ ty)) = ty
  dropHsParTy other                = other

-- | Append a constraint at the end of a type context.
--   If no context is present, a new one will be created.
appendConstraint ::
  -- | The new constraint to append
  String ->
  -- | The type signature where the constraint is to be inserted, also assuming annotated
  LHsType GhcPs ->
  Rewrite
appendConstraint constraintT = go
 where
  go (L l it@HsQualTy{hst_ctxt = L l' ctxt}) = Rewrite l $ \df -> do
    constraint <- liftParseAST df constraintT
    setEntryDPT constraint (DP (0, 1))

    -- Paren annotations are usually attached to the first and last constraints,
    -- rather than to the constraint list itself, so to preserve them we need to reposition them
    closeParenDP <- lookupAnn (G AnnCloseP) `mapM` lastMaybe ctxt
    openParenDP <- lookupAnn (G AnnOpenP) `mapM` headMaybe ctxt
    ctxt' <- fixParens (join openParenDP) (join closeParenDP) (L l' ctxt)

    addTrailingCommaT (last ctxt')

    return $ L l $ it{hst_ctxt = L l' $ ctxt' ++ [constraint]}
  go (L _ HsForAllTy{hst_body}) = go hst_body
  go (L _ (HsParTy _ ty)) = go ty
  go (L l other) = Rewrite l $ \df -> do
    -- there isn't a context, so we must create one
    constraint <- liftParseAST df constraintT
    lContext <- uniqueSrcSpanT
    lTop <- uniqueSrcSpanT
    let context = L lContext [constraint]
    addSimpleAnnT context (DP (0, 0)) $
      (G AnnDarrow, DP (0, 1)) :
      concat
        [ [ (G AnnOpenP, dp00)
          , (G AnnCloseP, dp00)
          ]
        | hsTypeNeedsParens sigPrec $ unLoc constraint
        ]
    return $ L lTop $ HsQualTy noExtField context (L l other)

liftParseAST :: ASTElement ast => DynFlags -> String -> TransformT (Either String) (Located ast)
liftParseAST df s = case parseAST df "" s of
  Right (anns, x) -> modifyAnnsT (anns <>) $> x
  Left _          -> lift $ Left $ "No parse: " <> s

lookupAnn :: (Data a, Monad m) => KeywordId -> Located a -> TransformT m (Maybe DeltaPos)
lookupAnn comment la = do
  anns <- getAnnsT
  return $ Map.lookup (mkAnnKey la) anns >>= lookup comment . annsDP

dp00 :: DeltaPos
dp00 = DP (0, 0)

headMaybe :: [a] -> Maybe a
headMaybe []      = Nothing
headMaybe (a : _) = Just a

lastMaybe :: [a] -> Maybe a
lastMaybe []    = Nothing
lastMaybe other = Just $ last other

liftMaybe :: String -> Maybe a -> TransformT (Either String) a
liftMaybe _ (Just x) = return x
liftMaybe s _        = lift $ Left s

-- | Copy anns attached to a into b with modification, then delete anns of a
transferAnn :: (Data a, Data b) => Located a -> Located b -> (Annotation -> Annotation) -> TransformT (Either String) ()
transferAnn la lb f = do
  anns <- getAnnsT
  let oldKey = mkAnnKey la
      newKey = mkAnnKey lb
  oldValue <- liftMaybe "Unable to find ann" $ Map.lookup oldKey anns
  putAnnsT $ Map.delete oldKey $ Map.insert newKey (f oldValue) anns

------------------------------------------------------------------------------
extendImport :: Maybe String -> String -> LImportDecl GhcPs -> Rewrite
extendImport mparent identifier lDecl@(L l _) =
  Rewrite l $ \df -> do
    case mparent of
      Just parent -> extendImportViaParent df parent identifier lDecl
      _           -> extendImportTopLevel df identifier lDecl

-- | Add an identifier to import list
--
-- extendImportTopLevel "foo" AST:
--
-- import A --> Error
-- import A (foo) --> Error
-- import A (bar) --> import A (bar, foo)
extendImportTopLevel :: DynFlags -> String -> LImportDecl GhcPs -> TransformT (Either String) (LImportDecl GhcPs)
extendImportTopLevel df idnetifier (L l it@ImportDecl{..})
  | Just (hide, L l' lies) <- ideclHiding
    , hasSibling <- not $ null lies = do
    src <- uniqueSrcSpanT
    top <- uniqueSrcSpanT
    rdr <- liftParseAST df idnetifier

    let alreadyImported =
          showNameWithoutUniques (occName (unLoc rdr))
            `elem` map (showNameWithoutUniques @OccName) (listify (const True) lies)
    when alreadyImported $
      lift (Left $ idnetifier <> " already imported")

    let lie = L src $ IEName rdr
        x = L top $ IEVar noExtField lie
    if x `elem` lies
      then lift (Left $ idnetifier <> " already imported")
      else do
        when hasSibling $
          addTrailingCommaT (last lies)
        addSimpleAnnT x (DP (0, if hasSibling then 1 else 0)) []
        addSimpleAnnT rdr dp00 $ unqalDP $ hasParen idnetifier
        -- Parens are attachted to `lies`, so if `lies` was empty previously,
        -- we need change the ann key from `[]` to `:` to keep parens and other anns.
        unless hasSibling $
          transferAnn (L l' lies) (L l' [x]) id
        return $ L l it{ideclHiding = Just (hide, L l' $ lies ++ [x])}
extendImportTopLevel _ _ _ = lift $ Left "Unable to extend the import list"

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
extendImportViaParent :: DynFlags -> String -> String -> LImportDecl GhcPs -> TransformT (Either String) (LImportDecl GhcPs)
extendImportViaParent df parent child (L l it@ImportDecl{..})
  | Just (hide, L l' lies) <- ideclHiding = go hide l' [] lies
 where
  go :: Bool -> SrcSpan -> [LIE GhcPs] -> [LIE GhcPs] -> TransformT (Either String) (LImportDecl GhcPs)
  go _hide _l' _pre ((L _ll' (IEThingAll _ (L _ ie))) : _xs)
    | parent == unIEWrappedName ie = lift . Left $ child <> " already included in " <> parent <> " imports"
  go hide l' pre (lAbs@(L ll' (IEThingAbs _ absIE@(L _ ie))) : xs)
    -- ThingAbs ie => ThingWith ie child
    | parent == unIEWrappedName ie = do
      srcChild <- uniqueSrcSpanT
      childRdr <- liftParseAST df child
      let childLIE = L srcChild $ IEName childRdr
          x :: LIE GhcPs = L ll' $ IEThingWith noExtField absIE NoIEWildcard [childLIE] []
      -- take anns from ThingAbs, and attatch parens to it
      transferAnn lAbs x $ \old -> old{annsDP = annsDP old ++ [(G AnnOpenP, DP (0, 1)), (G AnnCloseP, dp00)]}
      addSimpleAnnT childRdr dp00 [(G AnnVal, dp00)]
      return $ L l it{ideclHiding = Just (hide, L l' $ reverse pre ++ [x] ++ xs)}
  go hide l' pre ((L l'' (IEThingWith _ twIE@(L _ ie) _ lies' _)) : xs)
    -- ThingWith ie lies' => ThingWith ie (lies' ++ [child])
    | parent == unIEWrappedName ie
      , hasSibling <- not $ null lies' =
      do
        srcChild <- uniqueSrcSpanT
        childRdr <- liftParseAST df child

        let alreadyImported =
              showNameWithoutUniques (occName (unLoc childRdr))
                `elem` map (showNameWithoutUniques @OccName) (listify (const True) lies')
        when alreadyImported $
          lift (Left $ child <> " already included in " <> parent <> " imports")

        when hasSibling $
          addTrailingCommaT (last lies')
        let childLIE = L srcChild $ IEName childRdr
        addSimpleAnnT childRdr (DP (0, if hasSibling then 1 else 0)) $ unqalDP $ hasParen child
        return $ L l it{ideclHiding = Just (hide, L l' $ reverse pre ++ [L l'' (IEThingWith noExtField twIE NoIEWildcard (lies' ++ [childLIE]) [])] ++ xs)}
  go hide l' pre (x : xs) = go hide l' (x : pre) xs
  go hide l' pre []
    | hasSibling <- not $ null pre = do
      -- [] => ThingWith parent [child]
      l'' <- uniqueSrcSpanT
      srcParent <- uniqueSrcSpanT
      srcChild <- uniqueSrcSpanT
      parentRdr <- liftParseAST df parent
      childRdr <- liftParseAST df child
      when hasSibling $
        addTrailingCommaT (head pre)
      let parentLIE = L srcParent $ IEName parentRdr
          childLIE = L srcChild $ IEName childRdr
          x :: LIE GhcPs = L l'' $ IEThingWith noExtField parentLIE NoIEWildcard [childLIE] []
      addSimpleAnnT parentRdr (DP (0, if hasSibling then 1 else 0)) $ unqalDP $ hasParen parent
      addSimpleAnnT childRdr (DP (0, 0)) $ unqalDP $ hasParen child
      addSimpleAnnT x (DP (0, 0)) [(G AnnOpenP, DP (0, 1)), (G AnnCloseP, DP (0, 0))]
      -- Parens are attachted to `pre`, so if `pre` was empty previously,
      -- we need change the ann key from `[]` to `:` to keep parens and other anns.
      unless hasSibling $
        transferAnn (L l' $ reverse pre) (L l' [x]) id
      return $ L l it{ideclHiding = Just (hide, L l' $ reverse pre ++ [x])}
extendImportViaParent _ _ _ _ = lift $ Left "Unable to extend the import list via parent"

unIEWrappedName :: IEWrappedName (IdP GhcPs) -> String
unIEWrappedName (occName -> occ) = showSDocUnsafe $ parenSymOcc occ (ppr occ)

hasParen :: String -> Bool
hasParen ('(' : _) = True
hasParen _         = False

unqalDP :: Bool -> [(KeywordId, DeltaPos)]
unqalDP paren =
  ( if paren
      then \x -> (G AnnOpenP, dp00) : x : [(G AnnCloseP, dp00)]
      else pure
  )
    (G AnnVal, dp00)

------------------------------------------------------------------------------

-- | Hide a symbol from import declaration
hideSymbol ::
  String -> LImportDecl GhcPs -> Rewrite
hideSymbol symbol lidecl@(L loc ImportDecl{..}) =
  case ideclHiding of
    Nothing -> Rewrite loc $ extendHiding symbol lidecl Nothing
    Just (True, hides) -> Rewrite loc $ extendHiding symbol lidecl (Just hides)
    Just (False, imports) -> Rewrite loc $ deleteFromImport symbol lidecl imports
hideSymbol _ (L _ (XImportDecl _)) =
  error "cannot happen"

extendHiding ::
  String ->
  LImportDecl GhcPs ->
  Maybe (Located [LIE GhcPs]) ->
  DynFlags ->
  TransformT (Either String) (LImportDecl GhcPs)
extendHiding symbol (L l idecls) mlies df = do
  L l' lies <- case mlies of
    Nothing -> flip L [] <$> uniqueSrcSpanT
    Just pr -> pure pr
  let hasSibling = not $ null lies
  src <- uniqueSrcSpanT
  top <- uniqueSrcSpanT
  rdr <- liftParseAST df symbol
  let lie = L src $ IEName rdr
      x = L top $ IEVar noExtField lie
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
  addSimpleAnnT rdr dp00 $ unqalDP $ isOperator $ unLoc rdr
  if hasSibling
    then when hasSibling $ do
      addTrailingCommaT x
      addSimpleAnnT (head lies) (DP (0, 1)) []
      unless (null $ tail lies) $
        addTrailingCommaT (head lies) -- Why we need this?
    else forM_ mlies $ \lies0 -> do
      transferAnn lies0 singleHide id
  return $ L l idecls{ideclHiding = Just (True, L l' $ x : lies)}
 where
  isOperator = not . all isAlphaNum . occNameString . rdrNameOcc

deleteFromImport ::
  String ->
  LImportDecl GhcPs ->
  Located [LIE GhcPs] ->
  DynFlags ->
  TransformT (Either String) (LImportDecl GhcPs)
deleteFromImport (T.pack -> symbol) (L l idecl) llies@(L lieLoc lies) _ = do
  let edited = L lieLoc deletedLies
      lidecl' =
        L l $
          idecl
            { ideclHiding = Just (False, edited)
            }
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
  pure lidecl'
 where
  deletedLies =
    mapMaybe killLie lies
  killLie :: LIE GhcPs -> Maybe (LIE GhcPs)
  killLie v@(L _ (IEVar _ (L _ (unqualIEWrapName -> nam))))
    | nam == symbol = Nothing
    | otherwise = Just v
  killLie v@(L _ (IEThingAbs _ (L _ (unqualIEWrapName -> nam))))
    | nam == symbol = Nothing
    | otherwise = Just v
  killLie (L lieL (IEThingWith xt ty@(L _ (unqualIEWrapName -> nam)) wild cons flds))
    | nam == symbol = Nothing
    | otherwise =
      Just $
        L lieL $
          IEThingWith
            xt
            ty
            wild
            (filter ((/= symbol) . unqualIEWrapName . unLoc) cons)
            (filter ((/= symbol) . T.pack . unpackFS . flLabel . unLoc) flds)
  killLie v = Just v
