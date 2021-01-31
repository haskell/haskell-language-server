{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Development.IDE.Plugin.CodeAction.ExactPrint
  ( Rewrite (..),
    rewriteToEdit,

    -- * Utilities
    appendConstraint,
    extendImport,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Data (Data)
import Data.Functor
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Development.IDE.GHC.Compat hiding (parseExpr)
import Development.IDE.GHC.ExactPrint
import Development.IDE.Types.Location
import GhcPlugins (realSrcSpanEnd, realSrcSpanStart, sigPrec)
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types (DeltaPos (DP), KeywordId (G), mkAnnKey)
import Language.Haskell.LSP.Types
import OccName
import Outputable (ppr, showSDocUnsafe)

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

-- | Convert a 'Rewrite' into a 'WorkspaceEdit'.
rewriteToEdit ::
  DynFlags ->
  Anns ->
  Rewrite ->
  Either String [TextEdit]
rewriteToEdit dflags anns (Rewrite dst f) = do
  (ast, (anns, _), _) <- runTransformT anns $ f dflags
  let editMap = [ TextEdit (fromJust $ srcSpanToRange dst) $
                    T.pack $ tail $ exactPrint ast anns
                ]
  pure editMap

srcSpanToRange :: SrcSpan -> Maybe Range
srcSpanToRange (UnhelpfulSpan _) = Nothing
srcSpanToRange (RealSrcSpan real) = Just $ realSrcSpanToRange real

realSrcSpanToRange :: RealSrcSpan -> Range
realSrcSpanToRange real =
  Range
    (realSrcLocToPosition $ realSrcSpanStart real)
    (realSrcLocToPosition $ realSrcSpanEnd real)

realSrcLocToPosition :: RealSrcLoc -> Position
realSrcLocToPosition real =
  Position (srcLocLine real - 1) (srcLocCol real - 1)

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
    dropHsParTy other = other

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
    go (L l it@HsQualTy {hst_ctxt = L l' ctxt}) = Rewrite l $ \df -> do
      constraint <- liftParseAST df constraintT
      setEntryDPT constraint (DP (0, 1))

      -- Paren annotations are usually attached to the first and last constraints,
      -- rather than to the constraint list itself, so to preserve them we need to reposition them
      closeParenDP <- lookupAnn (G AnnCloseP) `mapM` lastMaybe ctxt
      openParenDP <- lookupAnn (G AnnOpenP) `mapM` headMaybe ctxt
      ctxt' <- fixParens (join openParenDP) (join closeParenDP) (L l' ctxt)

      addTrailingCommaT (last ctxt')

      return $ L l $ it {hst_ctxt = L l' $ ctxt' ++ [constraint]}
    go (L _ HsForAllTy {hst_body}) = go hst_body
    go (L _ (HsParTy _ ty)) = go ty
    go (L l other) = Rewrite l $ \df -> do
      -- there isn't a context, so we must create one
      constraint <- liftParseAST df constraintT
      lContext <- uniqueSrcSpanT
      lTop <- uniqueSrcSpanT
      let context = L lContext [constraint]
      addSimpleAnnT context (DP (0, 1)) $
        [ (G AnnDarrow, DP (0, 1))
        ]
          ++ concat
            [ [ (G AnnOpenP, dp00),
                (G AnnCloseP, dp00)
              ]
              | hsTypeNeedsParens sigPrec $ unLoc constraint
            ]
      return $ L lTop $ HsQualTy noExtField context (L l other)

liftParseAST :: ASTElement ast => DynFlags -> String -> TransformT (Either String) (Located ast)
liftParseAST df s = case parseAST df "" s of
  Right (anns, x) -> modifyAnnsT (anns <>) $> x
  Left _ -> lift $ Left $ "No parse: " <> s

lookupAnn :: (Data a, Monad m) => KeywordId -> Located a -> TransformT m (Maybe DeltaPos)
lookupAnn comment la = do
  anns <- getAnnsT
  return $ Map.lookup (mkAnnKey la) anns >>= lookup comment . annsDP

dp00 :: DeltaPos
dp00 = DP (0, 0)

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (a : _) = Just a

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe other = Just $ last other

liftMaybe :: String -> Maybe a -> TransformT (Either String) a
liftMaybe _ (Just x) = return x
liftMaybe s _ = lift $ Left s

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
      _ -> extendImportTopLevel df identifier lDecl

-- | Add an identifier to import list
--
-- extendImportTopLevel "foo" AST:
--
-- import A --> Error
-- import A (bar) --> import A (bar, foo)
extendImportTopLevel :: DynFlags -> String -> LImportDecl GhcPs -> TransformT (Either String) (LImportDecl GhcPs)
extendImportTopLevel df idnetifier (L l it@ImportDecl {..})
  | Just (hide, L l' lies) <- ideclHiding,
    hasSibling <- not $ null lies = do
    src <- uniqueSrcSpanT
    top <- uniqueSrcSpanT
    rdr <- liftParseAST df idnetifier
    let lie = L src $ IEName rdr
        x = L top $ IEVar noExtField lie
    when hasSibling $
      addTrailingCommaT (last lies)
    addSimpleAnnT x (DP (0, if hasSibling then 1 else 0)) []
    addSimpleAnnT rdr dp00 $ unqalDP $ hasParen idnetifier
    -- Parens are attachted to `lies`, so if `lies` was empty previously,
    -- we need change the ann key from `[]` to `:` to keep parens and other anns.
    unless hasSibling $
      transferAnn (L l' lies) (L l' [x]) id
    return $ L l it {ideclHiding = Just (hide, L l' $ lies ++ [x])}
extendImportTopLevel _ _ _ = lift $ Left "Unable to extend the import list"

-- | Add an identifier with its parent to import list
--
-- extendImportViaParent "Bar" "Cons" AST:
--
-- import A --> Error
-- import A () --> import A (Bar(Cons))
-- import A (Foo, Bar) --> import A (Foo, Bar(Cons))
-- import A (Foo, Bar()) --> import A (Foo, Bar(Cons))
extendImportViaParent :: DynFlags -> String -> String -> LImportDecl GhcPs -> TransformT (Either String) (LImportDecl GhcPs)
extendImportViaParent df parent child (L l it@ImportDecl {..})
  | Just (hide, L l' lies) <- ideclHiding = go hide l' [] lies
  where
    go :: Bool -> SrcSpan -> [LIE GhcPs] -> [LIE GhcPs] -> TransformT (Either String) (LImportDecl GhcPs)
    go hide l' pre (lAbs@(L ll' (IEThingAbs _ absIE@(L _ ie))) : xs)
      -- ThingAbs ie => ThingWith ie child
      | parent == unIEWrappedName ie = do
        srcChild <- uniqueSrcSpanT
        childRdr <- liftParseAST df child
        let childLIE = L srcChild $ IEName childRdr
            x :: LIE GhcPs = L ll' $ IEThingWith noExtField absIE NoIEWildcard [childLIE] []
        -- take anns from ThingAbs, and attatch parens to it
        transferAnn lAbs x $ \old -> old {annsDP = annsDP old ++ [(G AnnOpenP, DP (0, 1)), (G AnnCloseP, dp00)]}
        addSimpleAnnT childRdr dp00 [(G AnnVal, dp00)]
        return $ L l it {ideclHiding = Just (hide, L l' $ reverse pre ++ [x] ++ xs)}
    go hide l' pre ((L l'' (IEThingWith _ twIE@(L _ ie) _ lies' _)) : xs)
      -- ThingWith ie lies' => ThingWith ie (lies' ++ [child])
      | parent == unIEWrappedName ie,
        hasSibling <- not $ null lies' =
        do
          srcChild <- uniqueSrcSpanT
          childRdr <- liftParseAST df child
          when hasSibling $
            addTrailingCommaT (last lies')
          let childLIE = L srcChild $ IEName childRdr
          addSimpleAnnT childRdr (DP (0, if hasSibling then 1 else 0)) $ unqalDP $ hasParen child
          return $ L l it {ideclHiding = Just (hide, L l' $ reverse pre ++ [L l'' (IEThingWith noExtField twIE NoIEWildcard (lies' ++ [childLIE]) [])] ++ xs)}
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
        return $ L l it {ideclHiding = Just (hide, L l' $ reverse pre ++ [x])}
extendImportViaParent _ _ _ _ = lift $ Left "Unable to extend the import list via parent"

unIEWrappedName :: IEWrappedName (IdP GhcPs) -> String
unIEWrappedName (occName -> occ) = showSDocUnsafe $ parenSymOcc occ (ppr occ)

hasParen :: String -> Bool
hasParen ('(' : _) = True
hasParen _ = False

unqalDP :: Bool -> [(KeywordId, DeltaPos)]
unqalDP paren =
  ( if paren
      then \x -> (G AnnOpenP, dp00) : x : [(G AnnCloseP, dp00)]
      else pure
  )
    (G AnnVal, dp00)
