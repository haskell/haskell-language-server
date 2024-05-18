{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
module Development.IDE.Plugin.CodeAction.ExactPrint (
  Rewrite (..),
  rewriteToEdit,
  rewriteToWEdit,

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
import           Data.Char                              (isAlphaNum)
import           Data.Data                              (Data)
import           Data.Generics                          (listify)
import qualified Data.Text                              as T
import           Development.IDE.GHC.Compat             hiding (Annotation)
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.ExactPrint
import           Development.IDE.GHC.Util
import           Development.IDE.Spans.Common
import           GHC.Exts                               (IsList (fromList))
import           GHC.Stack                              (HasCallStack)
import           Language.Haskell.GHC.ExactPrint
import           Language.LSP.Protocol.Types

import           Control.Lens                           (_head, _last, over)
import           Data.Bifunctor                         (first)
import           Data.Maybe                             (fromMaybe, mapMaybe)
import           Development.IDE.Plugin.CodeAction.Util
import           GHC                                    (AddEpAnn (..),
                                                         AnnContext (..),
                                                         AnnList (..),
                                                         AnnParen (..),
                                                         DeltaPos (SameLine),
                                                         EpAnn (..),
                                                         IsUnicodeSyntax (NormalSyntax),
                                                         NameAdornment (NameParens),
                                                         TrailingAnn (AddCommaAnn),
                                                         emptyComments, reAnnL)

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if !MIN_VERSION_ghc(9,9,0)
import           Data.Default                           (Default (..))
import           GHC                                    (EpaLocation (EpaDelta),
                                                         addAnns, ann)
#endif

#if MIN_VERSION_ghc(9,9,0)
import           GHC                                    (EpaLocation' (..),
                                                         NoAnn (..))
#endif

------------------------------------------------------------------------------

-- | Construct a 'Rewrite', replacing the node at the given 'SrcSpan' with the
--   given 'ast'.
data Rewrite where
  Rewrite ::
    (ExactPrint (GenLocated (Anno ast) ast), ResetEntryDP (Anno ast), Outputable (GenLocated (Anno ast) ast), Data (GenLocated (Anno ast) ast)) =>
    -- | The 'SrcSpan' that we want to rewrite
    SrcSpan ->
    -- | The ast that we want to graft
    (DynFlags -> TransformT (Either String) (GenLocated (Anno ast) ast)) ->
    Rewrite

------------------------------------------------------------------------------
class ResetEntryDP ann where
    resetEntryDP :: GenLocated ann ast -> GenLocated ann ast
#if MIN_VERSION_ghc(9,9,0)
instance {-# OVERLAPPING #-} NoAnn an => ResetEntryDP (EpAnn an) where
    -- resetEntryDP = flip setEntryDP (SameLine 0)
    resetEntryDP (L srcAnn x) = setEntryDP (L srcAnn{anns=noAnn} x) (SameLine 0)
#else
instance {-# OVERLAPPING #-} Default an => ResetEntryDP (SrcAnn an) where
    -- resetEntryDP = flip setEntryDP (SameLine 0)
    resetEntryDP (L srcAnn x) = setEntryDP (L srcAnn{ann=EpAnnNotUsed} x) (SameLine 0)
#endif
instance {-# OVERLAPPABLE #-} ResetEntryDP fallback where
    resetEntryDP = id

-- | Convert a 'Rewrite' into a list of '[TextEdit]'.
rewriteToEdit :: HasCallStack =>
  DynFlags ->
  Rewrite ->
  Either String [TextEdit]
rewriteToEdit dflags
              (Rewrite dst f) = do
  (ast, _ , _) <- runTransformT $ do
    ast <- f dflags
    pure $ traceAst "REWRITE_result" $ resetEntryDP ast
  let edits = case srcSpanToRange dst of
        Just range -> [ TextEdit range $ T.pack $ exactPrint ast ]
        Nothing -> []
  pure edits

-- | Convert a 'Rewrite' into a 'WorkspaceEdit'
rewriteToWEdit :: DynFlags
               -> Uri
               -> Rewrite
               -> Either String WorkspaceEdit
rewriteToWEdit dflags uri
               r = do
  edits <- rewriteToEdit dflags
                         r
  return $
    WorkspaceEdit
      { _changes = Just (fromList [(uri, edits)])
      , _documentChanges = Nothing
      , _changeAnnotations = Nothing
      }

------------------------------------------------------------------------------


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
#if !MIN_VERSION_ghc(9,4,0)
    go (L l it@HsQualTy{hst_ctxt = Just (L l' ctxt), hst_body}) = Rewrite (locA l) $ \_ -> do
#else
    go (L l it@HsQualTy{hst_ctxt = L l' ctxt, hst_body}) = Rewrite (locA l) $ \_ -> do
#endif
      let ctxt' = filter (not . toRemove) ctxt
          removeStuff = (toRemove <$> headMaybe ctxt) == Just True
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
#else
  go (L l it@HsQualTy{hst_ctxt = Just (L l' ctxt)}) = Rewrite (locA l) $ \df -> do
#endif
    constraint <- liftParseAST df constraintT
    constraint <- pure $ setEntryDP constraint (SameLine 1)
#if MIN_VERSION_ghc(9,9,0)
    let l'' = fmap (addParensToCtxt close_dp) l'
#else
    let l'' = (fmap.fmap) (addParensToCtxt close_dp) l'
#endif
    -- For singleton constraints, the close Paren DP is attached to an HsPar wrapping the constraint
    -- we have to reposition it manually into the AnnContext
        close_dp = case ctxt of
#if MIN_VERSION_ghc(9,9,0)
            [L _ (HsParTy AnnParen{ap_close} _)] -> Just ap_close
#else
            [L _ (HsParTy EpAnn{anns=AnnParen{ap_close}} _)] -> Just ap_close
#endif
            _ -> Nothing
        ctxt' = over _last (first addComma) $ map dropHsParTy ctxt
#if MIN_VERSION_ghc(9,4,0)
    return $ L l $ it{hst_ctxt = L l'' $ ctxt' ++ [constraint]}
#else
    return $ L l $ it{hst_ctxt = Just $ L l'' $ ctxt' ++ [constraint]}
#endif
  go (L _ HsForAllTy{hst_body}) = go hst_body
  go (L _ (HsParTy _ ty)) = go ty
  go ast@(L l _) = Rewrite (locA l) $ \df -> do
    -- there isn't a context, so we must create one
    constraint <- liftParseAST df constraintT
    lContext <- uniqueSrcSpanT
    lTop <- uniqueSrcSpanT
#if MIN_VERSION_ghc(9,4,0)
    let context = reAnnL annCtxt emptyComments $ L lContext [resetEntryDP constraint]
#else
    let context = Just $ reAnnL annCtxt emptyComments $ L lContext [resetEntryDP constraint]
#endif
        annCtxt = AnnContext (Just (NormalSyntax, epl 1)) [epl 0 | needsParens] [epl 0 | needsParens]
        needsParens = hsTypeNeedsParens sigPrec $ unLoc constraint
    ast <- pure $ setEntryDP (makeDeltaAst ast) (SameLine 1)

    return $ reLocA $ L lTop $ HsQualTy noExtField context ast

liftParseAST
    :: forall ast l.  (ASTElement l ast, ExactPrint (LocatedAn l ast))
    => DynFlags -> String -> TransformT (Either String) (LocatedAn l ast)
liftParseAST df s = case parseAST df "" s of
  Right x ->  pure (makeDeltaAst x)
  Left _          -> TransformT $ lift $ Left $ "No parse: " <> s


headMaybe :: [a] -> Maybe a
headMaybe []      = Nothing
headMaybe (a : _) = Just a

lastMaybe :: [a] -> Maybe a
lastMaybe []    = Nothing
lastMaybe other = Just $ last other

------------------------------------------------------------------------------
extendImport :: Maybe String -> String -> LImportDecl GhcPs -> Rewrite
extendImport mparent identifier lDecl@(L l _) =
  Rewrite (locA l) $ \df -> do
    case mparent of
      -- This will also work for `ImportAllConstructors`
      -- Parsed source in GHC 9.4 uses absolute position annotation (RealSrcSpan),
      -- while rewriting relies on relative positions. ghc-exactprint has the utility
      -- makeDeltaAst for relativization.
      Just parent -> extendImportViaParent df parent identifier (makeDeltaAst lDecl)
      _           -> extendImportTopLevel identifier (makeDeltaAst lDecl)

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
#if MIN_VERSION_ghc(9,5,0)
  | Just (hide, L l' lies) <- ideclImportList
#else
  | Just (hide, L l' lies) <- ideclHiding
#endif
  = do
    src <- uniqueSrcSpanT
    top <- uniqueSrcSpanT
    let rdr = reLocA $ L src $ mkRdrUnqual $ mkVarOcc thing
    let alreadyImported =
          printOutputable (occName (unLoc rdr))
            `elem` map (printOutputable @OccName) (listify (const True) lies)
    when alreadyImported $
      TransformT $ lift (Left $ thing <> " already imported")

    let lie = reLocA $ L src $ IEName
#if MIN_VERSION_ghc(9,5,0)
                                noExtField
#endif
                                rdr
        x = reLocA $ L top $ IEVar
#if MIN_VERSION_ghc(9,8,0)
                               Nothing -- no deprecated
#else
                               noExtField
#endif
                               lie
#if MIN_VERSION_ghc(9,9,0)
                               Nothing
#endif

    if x `elem` lies
      then TransformT $ lift (Left $ thing <> " already imported")
      else do
        let lies' = addCommaInImportList lies x
#if MIN_VERSION_ghc(9,5,0)
        return $ L l it{ideclImportList = Just (hide, L l' lies')}
#else
        return $ L l it{ideclHiding = Just (hide, L l' lies')}
#endif
extendImportTopLevel _ _ = TransformT $ lift $ Left "Unable to extend the import list"

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
#if MIN_VERSION_ghc(9,5,0)
  | Just (hide, L l' lies) <- ideclImportList = go hide l' [] lies
#else
  | Just (hide, L l' lies) <- ideclHiding = go hide l' [] lies
#endif
 where
#if MIN_VERSION_ghc(9,9,0)
  go _hide _l' _pre ((L _ll' (IEThingAll _ (L _ ie) _)) : _xs)
#else
  go _hide _l' _pre ((L _ll' (IEThingAll _ (L _ ie))) : _xs)
#endif
    | parent == unIEWrappedName ie = TransformT $ lift . Left $ child <> " already included in " <> parent <> " imports"
#if MIN_VERSION_ghc(9,9,0)
  go hide l' pre ((L ll' (IEThingAbs _ absIE@(L _ ie) docs)) : xs)
#else
  go hide l' pre ((L ll' (IEThingAbs _ absIE@(L _ ie))) : xs)
#endif
    -- ThingAbs ie => ThingWith ie child
    | parent == unIEWrappedName ie = do
      srcChild <- uniqueSrcSpanT
      let childRdr = reLocA $ L srcChild $ mkRdrUnqual $ mkVarOcc child
          childLIE = reLocA $ L srcChild $ IEName
#if MIN_VERSION_ghc(9,5,0)
                                             noExtField
#endif
                                             childRdr
          x :: LIE GhcPs = L ll' $ IEThingWith
#if MIN_VERSION_ghc(9,9,0)
                                     (Nothing, [AddEpAnn AnnOpenP (EpaDelta (SameLine 1) []), AddEpAnn AnnCloseP noAnn])
#elif MIN_VERSION_ghc(9,7,0)
                                     (Nothing, addAnns mempty [AddEpAnn AnnOpenP (EpaDelta (SameLine 1) []), AddEpAnn AnnCloseP def] emptyComments)
#else
                                     (addAnns mempty [AddEpAnn AnnOpenP (EpaDelta (SameLine 1) []), AddEpAnn AnnCloseP def] emptyComments)
#endif
                                     absIE NoIEWildcard [childLIE]
#if MIN_VERSION_ghc(9,9,0)
                                     docs
#endif


#if MIN_VERSION_ghc(9,5,0)
      return $ L l it{ideclImportList = Just (hide, L l' $ reverse pre ++ [x] ++ xs)}
#else
      return $ L l it{ideclHiding = Just (hide, L l' $ reverse pre ++ [x] ++ xs)}
#endif

#if MIN_VERSION_ghc(9,9,0)
  go hide l' pre ((L l'' (IEThingWith l''' twIE@(L _ ie) _ lies' docs)) : xs)
#else
  go hide l' pre ((L l'' (IEThingWith l''' twIE@(L _ ie) _ lies')) : xs)
#endif
    -- ThingWith ie lies' => ThingWith ie (lies' ++ [child])
    | parent == unIEWrappedName ie
    , child == wildCardSymbol = do
#if MIN_VERSION_ghc(9,5,0)
        let it' = it{ideclImportList = Just (hide, lies)}
#else
        let it' = it{ideclHiding = Just (hide, lies)}
#endif
            thing = IEThingWith newl twIE (IEWildcard 2) []
#if MIN_VERSION_ghc(9,9,0)
                                docs
#endif
#if MIN_VERSION_ghc(9,7,0) && !MIN_VERSION_ghc(9,9,0)
            newl = fmap (\ann -> ann ++ [AddEpAnn AnnDotdot d0]) <$> l'''
#else
            newl = (\ann -> ann ++ [AddEpAnn AnnDotdot d0]) <$> l'''
#endif
            lies = L l' $ reverse pre ++ [L l'' thing] ++ xs
        return $ L l it'
    | parent == unIEWrappedName ie = do
        let hasSibling = not $ null lies'
        srcChild <- uniqueSrcSpanT
        let childRdr = reLocA $ L srcChild $ mkRdrUnqual $ mkVarOcc child
        childRdr <- pure $ setEntryDP childRdr $ SameLine $ if hasSibling then 1 else 0
        let alreadyImported =
              printOutputable (occName (unLoc childRdr))
                `elem` map (printOutputable @OccName) (listify (const True) lies')
        when alreadyImported $
          TransformT $ lift (Left $ child <> " already included in " <> parent <> " imports")

        let childLIE = reLocA $ L srcChild $ IEName
#if MIN_VERSION_ghc(9,5,0)
                                               noExtField
#endif
                                               childRdr
#if MIN_VERSION_ghc(9,5,0)
        let it' = it{ideclImportList = Just (hide, lies)}
#else
        let it' = it{ideclHiding = Just (hide, lies)}
#endif
            lies = L l' $ reverse pre ++
                [L l'' (IEThingWith l''' twIE NoIEWildcard (over _last fixLast lies' ++ [childLIE])
#if MIN_VERSION_ghc(9,9,0)
                                    docs
#endif
                       )] ++ xs
            fixLast = if hasSibling then first addComma else id
        return $ L l it'
  go hide l' pre (x : xs) = go hide l' (x : pre) xs
  go hide l' pre [] = do
      -- [] => ThingWith parent [child]
      l'' <- uniqueSrcSpanT
      srcParent <- uniqueSrcSpanT
      srcChild <- uniqueSrcSpanT
      parentRdr <- liftParseAST df parent
      let childRdr = reLocA $ L srcChild $ mkRdrUnqual $ mkVarOcc child
          isParentOperator = hasParen parent
      let parentLIE = reLocA $ L srcParent $ if isParentOperator then IEType (epl 0) parentRdr'
                                               else IEName
#if MIN_VERSION_ghc(9,5,0)
                                                      noExtField
#endif
                                                      parentRdr'
          parentRdr' = modifyAnns parentRdr $ \case
              it@NameAnn{nann_adornment = NameParens} -> it{nann_open = epl 1, nann_close = epl 0}
              other -> other
          childLIE = reLocA $ L srcChild $ IEName
#if MIN_VERSION_ghc(9,5,0)
                                             noExtField
#endif
                                             childRdr
#if MIN_VERSION_ghc(9,9,0)
          listAnn = (Nothing, [AddEpAnn AnnOpenP (epl 1), AddEpAnn AnnCloseP (epl 0)])
#elif MIN_VERSION_ghc(9,7,0)
          listAnn = (Nothing, epAnn srcParent [AddEpAnn AnnOpenP (epl 1), AddEpAnn AnnCloseP (epl 0)])
#else
          listAnn = epAnn srcParent [AddEpAnn AnnOpenP (epl 1), AddEpAnn AnnCloseP (epl 0)]
#endif
          x :: LIE GhcPs = reLocA $ L l'' $ IEThingWith listAnn parentLIE NoIEWildcard [childLIE]
#if MIN_VERSION_ghc(9,9,0)
                                                        Nothing -- TODO preserve docs?
#endif

          lies' = addCommaInImportList (reverse pre) x
#if MIN_VERSION_ghc(9,5,0)
      return $ L l it{ideclImportList = Just (hide, L l' lies')}
#else
      return $ L l it{ideclHiding = Just (hide, L l' lies')}
#endif
extendImportViaParent _ _ _ _ = TransformT $ lift $ Left "Unable to extend the import list via parent"

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
#if MIN_VERSION_ghc(9,9,0)
        lastItemAnn <- case lastItemSrcAnn of
            EpAnn _ lastItemAnn _ -> pure lastItemAnn
#else
        lastItemAnn <- case ann lastItemSrcAnn of
            EpAnn _ lastItemAnn _ -> pure lastItemAnn
            _ -> Nothing
#endif
        pure $ any isTrailingAnnComma (lann_trailing lastItemAnn)

    hasSibling = not $ null lies

    -- Setup the new item. It should have a preceding whitespace if it has siblings, and a trailing comma if the
    -- preceding item already has one.
    newItem = first (if existingTrailingComma then addComma else id) $
        setEntryDP x (SameLine $ if hasSibling then 1 else 0)

    -- Add the comma (if needed)
    fixLast :: [LocatedAn AnnListItem a] -> [LocatedAn AnnListItem a]
    fixLast = over _last (first (if existingTrailingComma then id else addComma))

#if MIN_VERSION_ghc(9,5,0)
unIEWrappedName :: IEWrappedName GhcPs -> String
#else
unIEWrappedName :: IEWrappedName (IdP GhcPs) -> String
#endif
unIEWrappedName (occName -> occ) = T.unpack $ printOutputable $ parenSymOcc occ (ppr occ)

hasParen :: String -> Bool
hasParen ('(' : _) = True
hasParen _         = False


------------------------------------------------------------------------------

-- | Hide a symbol from import declaration
hideSymbol ::
  String -> LImportDecl GhcPs -> Rewrite
hideSymbol symbol lidecl@(L loc ImportDecl{..}) =
#if MIN_VERSION_ghc(9,5,0)
  case ideclImportList of
    Nothing -> Rewrite (locA loc) $ extendHiding symbol lidecl Nothing
    Just (EverythingBut, hides) -> Rewrite (locA loc) $ extendHiding symbol lidecl (Just hides)
    Just (Exactly, imports) -> Rewrite (locA loc) $ deleteFromImport symbol lidecl imports
#else
  case ideclHiding of
    Nothing -> Rewrite (locA loc) $ extendHiding symbol lidecl Nothing
    Just (True, hides) -> Rewrite (locA loc) $ extendHiding symbol lidecl (Just hides)
    Just (False, imports) -> Rewrite (locA loc) $ deleteFromImport symbol lidecl imports
#endif

extendHiding ::
  String ->
  LImportDecl GhcPs ->
  Maybe (XRec GhcPs [LIE GhcPs]) ->
  DynFlags ->
  TransformT (Either String) (LImportDecl GhcPs)
extendHiding symbol (L l idecls) mlies df = do
  L l' lies <- case mlies of
    Nothing -> do
#if MIN_VERSION_ghc(9,9,0)
        let ann = noAnnSrcSpanDP0
#else
        src <- uniqueSrcSpanT
        let ann = noAnnSrcSpanDP0 src
#endif
#if MIN_VERSION_ghc(9,9,0)
            ann' = flip fmap ann $ \x -> x
#else
            ann' = flip (fmap.fmap) ann $ \x -> x
#endif
                {al_rest = [AddEpAnn AnnHiding (epl 1)]
                ,al_open = Just $ AddEpAnn AnnOpenP (epl 1)
                ,al_close = Just $ AddEpAnn AnnCloseP (epl 0)
                }
        return $ L ann' []
    Just pr -> pure pr
  let hasSibling = not $ null lies
  src <- uniqueSrcSpanT
  top <- uniqueSrcSpanT
  rdr <- liftParseAST df symbol
  rdr <- pure $ modifyAnns rdr $ addParens (isOperator $ unLoc rdr)
  let lie = reLocA $ L src $ IEName
#if MIN_VERSION_ghc(9,5,0)
                               noExtField
#endif
                               rdr
      x = reLocA $ L top $ IEVar
#if MIN_VERSION_ghc(9,7,0)
                             Nothing
#else
                             noExtField
#endif
                             lie
#if MIN_VERSION_ghc(9,9,0)
                             Nothing
#endif
  x <- pure $ if hasSibling then first addComma x else x
  lies <- pure $ over _head (`setEntryDP` SameLine 1) lies
#if MIN_VERSION_ghc(9,5,0)
  return $ L l idecls{ideclImportList = Just (EverythingBut, L l' $ x : lies)}
#else
  return $ L l idecls{ideclHiding = Just (True, L l' $ x : lies)}
#endif
 where
  isOperator = not . all isAlphaNum . occNameString . rdrNameOcc

deleteFromImport ::
  String ->
  LImportDecl GhcPs ->
  XRec GhcPs [LIE GhcPs] ->
  DynFlags ->
  TransformT (Either String) (LImportDecl GhcPs)
deleteFromImport (T.pack -> symbol) (L l idecl) (L lieLoc lies) _ = do
  let edited = L lieLoc deletedLies
      lidecl' =
        L l $
          idecl
#if MIN_VERSION_ghc(9,5,0)
            { ideclImportList = Just (Exactly, edited)
#else
            { ideclHiding = Just (False, edited)
#endif
            }
  pure lidecl'
 where
  deletedLies =
    over _last removeTrailingComma $
    mapMaybe killLie lies
  killLie :: LIE GhcPs -> Maybe (LIE GhcPs)
#if MIN_VERSION_ghc(9,9,0)
  killLie v@(L _ (IEVar _ (L _ (unqualIEWrapName -> nam)) _))
#else
  killLie v@(L _ (IEVar _ (L _ (unqualIEWrapName -> nam))))
#endif
    | nam == symbol = Nothing
    | otherwise = Just v
#if MIN_VERSION_ghc(9,9,0)
  killLie v@(L _ (IEThingAbs _ (L _ (unqualIEWrapName -> nam)) _))
#else
  killLie v@(L _ (IEThingAbs _ (L _ (unqualIEWrapName -> nam))))
#endif
    | nam == symbol = Nothing
    | otherwise = Just v
#if MIN_VERSION_ghc(9,9,0)
  killLie (L lieL (IEThingWith xt ty@(L _ (unqualIEWrapName -> nam)) wild cons docs))
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
#if MIN_VERSION_ghc(9,9,0)
            docs
#endif
  killLie v = Just v
