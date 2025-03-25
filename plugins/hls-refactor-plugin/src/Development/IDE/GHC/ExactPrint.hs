{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module hosts various abstractions and utility functions to work with ghc-exactprint.
module Development.IDE.GHC.ExactPrint
    ( Graft(..),
      graftDecls,
      graftDeclsWithM,
      annotate,
      annotateDecl,
      hoistGraft,
      graftWithM,
      graftExprWithM,
      genericGraftWithSmallestM,
      genericGraftWithLargestM,
      graftSmallestDeclsWithM,
      transform,
      transformM,
      ExactPrint(..),
      modifySmallestDeclWithM,
      modifyMgMatchesT,
      modifyMgMatchesT',
      modifySigWithM,
      genAnchor1,
      setPrecedingLines,
      addParens,
      addParensToCtxt,
      modifyAnns,
      removeComma,
      -- * Helper function
      eqSrcSpan,
      epl,
      epAnn,
      removeTrailingComma,
      annotateParsedSource,
      getAnnotatedParsedSourceRule,
      GetAnnotatedParsedSource(..),
      ASTElement (..),
      ExceptStringT (..),
      TransformT,
      Log(..),
    )
where

import           Control.Applicative                     (Alternative)
import           Control.Arrow                           ((***))
import           Control.DeepSeq
import           Control.Monad
import qualified Control.Monad.Fail                      as Fail
import           Control.Monad.IO.Class                  (MonadIO)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Zip
import           Data.Bifunctor
import           Data.Bool                               (bool)
import qualified Data.DList                              as DL
import           Data.Either.Extra                       (mapLeft)
import           Data.Functor.Classes
import           Data.Functor.Contravariant
import           Data.Monoid                             (All (All), getAll)
import qualified Data.Text                               as T
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake              hiding (Log)
import qualified Development.IDE.Core.Shake              as Shake
import           Development.IDE.GHC.Compat              hiding (parseImport,
                                                          parsePattern,
                                                          parseType)
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.Graph                   (RuleResult, Rules)
import           Development.IDE.Graph.Classes
import           Generics.SYB
import           Generics.SYB.GHC
import qualified GHC.Generics                            as GHC
import           Ide.Logger                              (Pretty (pretty),
                                                          Recorder,
                                                          WithPriority,
                                                          cmapWithPrio)
import           Ide.PluginUtils
import           Language.Haskell.GHC.ExactPrint.Parsers
import           Language.LSP.Protocol.Types

import           Control.Lens                            (_last, (&))
import           Control.Lens.Operators                  ((%~))
import           Data.List                               (partition)
import           GHC                                     (DeltaPos (..),
                                                          SrcSpanAnnN)

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if !MIN_VERSION_ghc(9,9,0)
import           Data.Default                            (Default)
import           GHC                                     ( Anchor (..),
                                                          AnchorOperation,
                                                          EpAnn (..),
                                                          NameAdornment (NameParens),
                                                          NameAnn (..),
                                                          SrcSpanAnn' (SrcSpanAnn),
                                                          SrcSpanAnnA,
                                                          TrailingAnn (AddCommaAnn),
                                                          emptyComments,
                                                          realSrcSpan,
                                                          spanAsAnchor)
import           GHC.Parser.Annotation                   (AnnContext (..),
                                                          EpaLocation (EpaDelta),
                                                          deltaPos)
import           GHC.Types.SrcLoc                        (generatedSrcSpan)
#endif

#if MIN_VERSION_ghc(9,9,0)
import           GHC                                     (
#if !MIN_VERSION_ghc(9,11,0)
                                                          Anchor,
#endif
                                                          AnnContext (..),
                                                          EpAnn (..),
                                                          EpaLocation,
                                                          EpaLocation' (..),
                                                          NameAdornment (..),
                                                          NameAnn (..),
                                                          SrcSpanAnnA,
                                                          TrailingAnn (..),
                                                          deltaPos,
                                                          emptyComments,
                                                          spanAsAnchor)
#endif

setPrecedingLines ::
#if !MIN_VERSION_ghc(9,9,0)
     Default t =>
#endif
    LocatedAn t a -> Int -> Int -> LocatedAn t a
setPrecedingLines ast n c = setEntryDP ast (deltaPos n c)
------------------------------------------------------------------------------

data Log = LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog

data GetAnnotatedParsedSource = GetAnnotatedParsedSource
  deriving (Eq, Show, GHC.Generic)

instance Hashable GetAnnotatedParsedSource
instance NFData GetAnnotatedParsedSource
type instance RuleResult GetAnnotatedParsedSource = ParsedSource

#if MIN_VERSION_ghc(9,5,0)
instance Show (HsModule GhcPs) where
  show _ = "<HsModule GhcPs>"
#else
instance Show HsModule where
  show _ = "<HsModule GhcPs>"
#endif

-- | Get the latest version of the annotated parse source with comments.
getAnnotatedParsedSourceRule :: Recorder (WithPriority Log) -> Rules ()
getAnnotatedParsedSourceRule recorder = define (cmapWithPrio LogShake recorder) $ \GetAnnotatedParsedSource nfp -> do
  pm <- use GetParsedModuleWithComments nfp
  return ([], fmap annotateParsedSource pm)

annotateParsedSource :: ParsedModule -> ParsedSource
annotateParsedSource (ParsedModule _ ps _) =
#if MIN_VERSION_ghc(9,9,0)
    ps
#else
    (makeDeltaAst ps)
#endif

------------------------------------------------------------------------------

{- | A transformation for grafting source trees together. Use the semigroup
 instance to combine 'Graft's, and run them via 'transform'.
-}
newtype Graft m a = Graft
    { runGraft :: DynFlags -> a -> TransformT m a
    }

hoistGraft :: (forall x. m x -> n x) -> Graft m a -> Graft n a
hoistGraft h (Graft f) = Graft (fmap (hoistTransform h) . f)

newtype ExceptStringT m a = ExceptStringT {runExceptString :: ExceptT String m a}
    deriving newtype
        ( MonadTrans
        , Monad
        , Functor
        , Applicative
        , Alternative
        , Foldable
        , Contravariant
        , MonadIO
        , Eq1
        , Ord1
        , Show1
        , Read1
        , MonadZip
        , MonadPlus
        , Eq
        , Ord
        , Show
        , Read
        )

instance Monad m => Fail.MonadFail (ExceptStringT m) where
    fail = ExceptStringT . ExceptT . pure . Left

instance Monad m => Semigroup (Graft m a) where
    Graft a <> Graft b = Graft $ \dflags -> a dflags >=> b dflags

instance Monad m => Monoid (Graft m a) where
    mempty = Graft $ const pure

------------------------------------------------------------------------------

-- | Convert a 'Graft' into a 'WorkspaceEdit'.
transform ::
    DynFlags ->
    ClientCapabilities ->
    VersionedTextDocumentIdentifier ->
    Graft (Either String) ParsedSource ->
    ParsedSource ->
    Either String WorkspaceEdit
transform dflags ccs verTxtDocId f a = do
    let src = printA a
    a' <- transformA a $ runGraft f dflags
    let res = printA a'
    pure $ diffText ccs (verTxtDocId, T.pack src) (T.pack res) IncludeDeletions

------------------------------------------------------------------------------

-- | Convert a 'Graft' into a 'WorkspaceEdit'.
transformM ::
    Monad m =>
    DynFlags ->
    ClientCapabilities ->
    VersionedTextDocumentIdentifier ->
    Graft (ExceptStringT m) ParsedSource ->
    ParsedSource ->
    m (Either String WorkspaceEdit)
transformM dflags ccs verTextDocId f a = runExceptT $
    runExceptString $ do
        let src = printA a
        a' <- transformA a $ runGraft f dflags
        let res = printA a'
        pure $ diffText ccs (verTextDocId, T.pack src) (T.pack res) IncludeDeletions


-- | Returns whether or not this node requires its immediate children to have
-- be parenthesized and have a leading space.
--
-- A more natural type for this function would be to return @(Bool, Bool)@, but
-- we use 'All' instead for its monoid instance.
needsParensSpace ::
    HsExpr GhcPs ->
    -- | (Needs parens, needs space)
    (All, All)
needsParensSpace HsLam{}         = (All False, All False)
#if !MIN_VERSION_ghc(9,9,0)
needsParensSpace HsLamCase{}     = (All False, All True)
#endif
needsParensSpace HsApp{}         = mempty
needsParensSpace HsAppType{}     = mempty
needsParensSpace OpApp{}         = mempty
needsParensSpace HsPar{}         = (All False, All False)
needsParensSpace SectionL{}      = (All False, All False)
needsParensSpace SectionR{}      = (All False, All False)
needsParensSpace ExplicitTuple{} = (All False, All False)
needsParensSpace ExplicitSum{}   = (All False, All False)
needsParensSpace HsCase{}        = (All False, All True)
needsParensSpace HsIf{}          = (All False, All False)
needsParensSpace HsMultiIf{}     = (All False, All False)
needsParensSpace HsLet{}         = (All False, All True)
needsParensSpace HsDo{}          = (All False, All False)
needsParensSpace ExplicitList{}  = (All False, All False)
needsParensSpace RecordCon{}     = (All False, All True)
needsParensSpace RecordUpd{}     = mempty
needsParensSpace _               = mempty


------------------------------------------------------------------------------

{- | Construct a 'Graft', replacing the node at the given 'SrcSpan' with the
 given @Located ast@. The node at that position must already be a @Located
 ast@, or this is a no-op.
-}
graft' ::
    forall ast a l.
    (Data a, ASTElement l ast) =>
    -- | Do we need to insert a space before this grafting? In do blocks, the
    -- answer is no, or we will break layout. But in function applications,
    -- the answer is yes, or the function call won't get its argument. Yikes!
    --
    -- More often the answer is yes, so when in doubt, use that.
    Bool ->
    SrcSpan ->
    LocatedAn l ast ->
    Graft (Either String) a
graft' needs_space dst val = Graft $ \dflags a -> do
    val' <- annotate dflags needs_space val
    pure $
        everywhere'
            ( mkT $
                \case
                    (L src _ :: LocatedAn l ast)
                        | locA src `eqSrcSpan` dst -> val'
                    l                         -> l
            )
            a


-- | Like 'graft', but specialized to 'LHsExpr', and intelligently inserts
-- parentheses if they're necessary.
graftExpr ::
    forall a.
    (Data a) =>
    SrcSpan ->
    LHsExpr GhcPs ->
    Graft (Either String) a
graftExpr dst val = Graft $ \dflags a -> do
    let (needs_space, mk_parens) = getNeedsSpaceAndParenthesize dst a

    runGraft
      (graft' needs_space dst $ mk_parens val)
      dflags
      a

getNeedsSpaceAndParenthesize ::
    (ASTElement l ast, Data a) =>
    SrcSpan ->
    a ->
    (Bool, LocatedAn l ast -> LocatedAn l ast)
getNeedsSpaceAndParenthesize dst a =
  -- Traverse the tree, looking for our replacement node. But keep track of
  -- the context (parent HsExpr constructor) we're in while we do it. This
  -- lets us determine whether or not we need parentheses.
  let (needs_parens, needs_space) =
          everythingWithContext (Nothing, Nothing) (<>)
            ( mkQ (mempty, ) $ \x s -> case x of
                (L src _ :: LHsExpr GhcPs) | locA src `eqSrcSpan` dst ->
                  (s, s)
                L _ x' -> (mempty, Just *** Just $ needsParensSpace x')
            ) a
   in ( maybe True getAll needs_space
      , bool id maybeParensAST $ maybe False getAll needs_parens
      )


------------------------------------------------------------------------------

graftExprWithM ::
    forall m a.
    (Fail.MonadFail m, Data a) =>
    SrcSpan ->
    (LHsExpr GhcPs -> TransformT m (Maybe (LHsExpr GhcPs))) ->
    Graft m a
graftExprWithM dst trans = Graft $ \dflags a -> do
    let (needs_space, mk_parens) = getNeedsSpaceAndParenthesize dst a

    everywhereM'
        ( mkM $
            \case
                val@(L src _ :: LHsExpr GhcPs)
                    | locA src `eqSrcSpan` dst -> do
                        mval <- trans val
                        case mval of
                            Just val' -> do
                                val'' <-
                                    hoistTransform (either Fail.fail pure)
                                        (annotate @AnnListItem @(HsExpr GhcPs) dflags needs_space (mk_parens val'))
                                pure val''
                            Nothing -> pure val
                l -> pure l
        )
        a

graftWithM ::
    forall ast m a l.
    (Fail.MonadFail m, Data a, ASTElement l ast) =>
    SrcSpan ->
    (LocatedAn l ast -> TransformT m (Maybe (LocatedAn l ast))) ->
    Graft m a
graftWithM dst trans = Graft $ \dflags a -> do
    everywhereM'
        ( mkM $
            \case
                val@(L src _ :: LocatedAn l ast)
                    | locA src `eqSrcSpan` dst -> do
                        mval <- trans val
                        case mval of
                            Just val' -> do
                                val'' <-
                                    hoistTransform (either Fail.fail pure) $
                                        annotate dflags False $ maybeParensAST val'
                                pure val''
                            Nothing -> pure val
                l -> pure l
        )
        a

-- | Run the given transformation only on the smallest node in the tree that
-- contains the 'SrcSpan'.
genericGraftWithSmallestM ::
    forall m a ast.
    (Monad m, Data a, Typeable ast) =>
    -- | The type of nodes we'd like to consider when finding the smallest.
    Proxy (Located ast) ->
    SrcSpan ->
    (DynFlags -> ast -> GenericM (TransformT m)) ->
    Graft m a
genericGraftWithSmallestM proxy dst trans = Graft $ \dflags ->
    smallestM (genericIsSubspan proxy dst) (trans dflags)

-- | Run the given transformation only on the largest node in the tree that
-- contains the 'SrcSpan'.
genericGraftWithLargestM ::
    forall m a ast.
    (Monad m, Data a, Typeable ast) =>
    -- | The type of nodes we'd like to consider when finding the largest.
    Proxy (Located ast) ->
    SrcSpan ->
    (DynFlags -> ast -> GenericM (TransformT m)) ->
    Graft m a
genericGraftWithLargestM proxy dst trans = Graft $ \dflags ->
    largestM (genericIsSubspan proxy dst) (trans dflags)


graftDecls ::
    forall a.
    (HasDecls a) =>
    SrcSpan ->
    [LHsDecl GhcPs] ->
    Graft (Either String) a
graftDecls dst decs0 = Graft $ \dflags a -> do
    decs <- forM decs0 $ \decl -> do
        annotateDecl dflags decl
    let go [] = DL.empty
        go (L src e : rest)
            | locA src `eqSrcSpan` dst = DL.fromList decs <> DL.fromList rest
            | otherwise = DL.singleton (L src e) <> go rest
    modifyDeclsT (pure . DL.toList . go) a


-- | Replace the smallest declaration whose SrcSpan satisfies the given condition with a new
-- list of declarations.
--
-- For example, if you would like to move a where-clause-defined variable to the same
-- level as its parent HsDecl, you could use this function.
--
-- When matching declaration is found in the sub-declarations of `a`, `Just r` is also returned with the new `a`.
-- If no declaration matched, then `Nothing` is returned.
modifySmallestDeclWithM ::
  forall a m r.
  (HasDecls a, Monad m) =>
  (SrcSpan -> m Bool) ->
  (LHsDecl GhcPs -> TransformT m ([LHsDecl GhcPs], r)) ->
  a ->
  TransformT m (a, Maybe r)
modifySmallestDeclWithM validSpan f a = do
  let modifyMatchingDecl [] = pure (DL.empty, Nothing)
      modifyMatchingDecl (ldecl@(L src _) : rest) =
        TransformT (lift $ validSpan $ locA src) >>= \case
            True -> do
              (decs', r) <- f ldecl
              pure (DL.fromList decs' <> DL.fromList rest, Just r)
            False -> first (DL.singleton ldecl <>) <$> modifyMatchingDecl rest
  modifyDeclsT' (fmap (first DL.toList) . modifyMatchingDecl) a

#if MIN_VERSION_ghc(9,9,0)
generatedAnchor :: DeltaPos -> Anchor
generatedAnchor dp = EpaDelta dp []
#else
generatedAnchor :: AnchorOperation -> Anchor
generatedAnchor anchorOp = GHC.Anchor (GHC.realSrcSpan generatedSrcSpan) anchorOp
#endif

setAnchor :: Anchor -> SrcSpanAnnN -> SrcSpanAnnN
#if MIN_VERSION_ghc(9,9,0)
setAnchor anc (EpAnn _ nameAnn comments) =
  EpAnn anc nameAnn comments
#else
setAnchor anc (SrcSpanAnn (EpAnn _ nameAnn comments) span) =
  SrcSpanAnn (EpAnn anc nameAnn comments) span
setAnchor _ spanAnnN = spanAnnN
#endif

removeTrailingAnns :: SrcSpanAnnN -> SrcSpanAnnN
#if MIN_VERSION_ghc(9,9,0)
removeTrailingAnns (EpAnn anc nameAnn comments) =
  let nameAnnSansTrailings = nameAnn {nann_trailing = []}
  in EpAnn anc nameAnnSansTrailings comments
#else
removeTrailingAnns (SrcSpanAnn (EpAnn anc nameAnn comments) span) =
  let nameAnnSansTrailings = nameAnn {nann_trailing = []}
  in SrcSpanAnn (EpAnn anc nameAnnSansTrailings comments) span
removeTrailingAnns spanAnnN = spanAnnN
#endif

-- | Modify the type signature for the given IdP. This function handles splitting a multi-sig
-- SigD into multiple SigD if the type signature is changed.
--
-- For example, update the type signature for `foo` from `Int` to `Bool`:
--
-- - foo :: Int
-- + foo :: Bool
--
-- - foo, bar :: Int
-- + bar :: Int
-- + foo :: Bool
--
-- - foo, bar, baz :: Int
-- + bar, baz :: Int
-- + foo :: Bool
modifySigWithM ::
  forall a m.
  (HasDecls a, Monad m, ExactPrint a) =>
  IdP GhcPs ->
  (LHsSigType GhcPs -> LHsSigType GhcPs) ->
  a ->
  TransformT m a
modifySigWithM queryId f a = do
  let modifyMatchingSigD :: [LHsDecl GhcPs] -> TransformT m (DL.DList (LHsDecl GhcPs))
      modifyMatchingSigD [] = pure DL.empty
      modifyMatchingSigD (ldecl@(L annSigD (SigD xsig (TypeSig xTypeSig ids (HsWC xHsWc lHsSig)))) : rest)
        | queryId `elem` (unLoc <$> ids) = do
            let newSig = f lHsSig
            -- If this signature update caused no change, then we don't need to split up multi-signatures
            if newSig `geq` lHsSig
              then pure $ DL.singleton ldecl <> DL.fromList rest
              else case partition ((== queryId) . unLoc) ids of
                ([L annMatchedId matchedId], otherIds) ->
                  let matchedId' = L (setAnchor genAnchor0 $ removeTrailingAnns annMatchedId) matchedId
                      matchedIdSig =
                        let sig' = SigD xsig (TypeSig xTypeSig [matchedId'] (HsWC xHsWc newSig))
                            epAnn = bool (noAnnSrcSpanDP
#if !MIN_VERSION_ghc(9,9,0)
                                            generatedSrcSpan
#endif
                                            (DifferentLine 1 0))
                                         annSigD (null otherIds)
                        in L epAnn sig'
                      otherSig = case otherIds of
                        [] -> []
#if MIN_VERSION_ghc(9,9,0)
                        (L epAnn id1:ids) ->
#else
                        (L (SrcSpanAnn epAnn span) id1:ids) ->
#endif
                          [
                          let epAnn' = case epAnn of
                                EpAnn _ nameAnn commentsId1 -> EpAnn genAnchor0 nameAnn commentsId1
#if MIN_VERSION_ghc(9,9,0)
                              ids' = L epAnn' id1:ids
#else
                                EpAnnNotUsed -> EpAnn genAnchor0 mempty emptyComments
                              ids' = L (SrcSpanAnn epAnn' span) id1:ids
#endif
                              ids'' = ids' & _last %~ first removeTrailingAnns
                            in L annSigD (SigD xsig (TypeSig xTypeSig ids'' (HsWC xHsWc lHsSig)))
                          ]
                  in pure $ DL.fromList otherSig <> DL.singleton matchedIdSig <> DL.fromList rest
                _ -> error "multiple ids matched"
      modifyMatchingSigD (ldecl : rest) = (DL.singleton ldecl <>) <$> modifyMatchingSigD rest
  modifyDeclsT (fmap DL.toList . modifyMatchingSigD) $ makeDeltaAst a

genAnchor0 :: Anchor
genAnchor0 = generatedAnchor m0

genAnchor1 :: Anchor
genAnchor1 = generatedAnchor m1

#if MIN_VERSION_ghc(9,9,0)
m0, m1 :: DeltaPos
m0 = SameLine 0
m1 = SameLine 1
#endif


-- | Apply a transformation to the decls contained in @t@
modifyDeclsT' :: (HasDecls t, HasTransform m)
             => ([LHsDecl GhcPs] -> m ([LHsDecl GhcPs], r))
             -> t -> m (t, r)
modifyDeclsT' action t = do
#if MIN_VERSION_ghc_exactprint(1,10,0)
  decls <- pure $ hsDecls t
#else
  decls <- liftT $ hsDecls t
#endif
  (decls', r) <- action decls
#if MIN_VERSION_ghc_exactprint(1,10,0)
  t' <- pure $ replaceDecls t decls'
#else
  t' <- liftT $ replaceDecls t decls'
#endif
  pure (t', r)

-- | Modify each LMatch in a MatchGroup
modifyMgMatchesT ::
  Monad m =>
  MatchGroup GhcPs (LHsExpr GhcPs) ->
  (LMatch GhcPs (LHsExpr GhcPs) -> TransformT m (LMatch GhcPs (LHsExpr GhcPs))) ->
  TransformT m (MatchGroup GhcPs (LHsExpr GhcPs))
modifyMgMatchesT mg f = fst <$> modifyMgMatchesT' mg (fmap (, ()) . f) () ((.) pure . const)

-- | Modify the each LMatch in a MatchGroup
modifyMgMatchesT' ::
  Monad m =>
  MatchGroup GhcPs (LHsExpr GhcPs) ->
  (LMatch GhcPs (LHsExpr GhcPs) -> TransformT m (LMatch GhcPs (LHsExpr GhcPs), r)) ->
  r ->
  (r -> r -> m r) ->
  TransformT m (MatchGroup GhcPs (LHsExpr GhcPs), r)
#if MIN_VERSION_ghc(9,5,0)
modifyMgMatchesT' (MG xMg (L locMatches matches)) f def combineResults = do
  (unzip -> (matches', rs)) <- mapM f matches
  r' <- TransformT $ lift $ foldM combineResults def rs
  pure (MG xMg (L locMatches matches'), r')
#else
modifyMgMatchesT' (MG xMg (L locMatches matches) originMg) f def combineResults = do
  (unzip -> (matches', rs)) <- mapM f matches
  r' <- lift $ foldM combineResults def rs
  pure (MG xMg (L locMatches matches') originMg, r')
#endif

graftSmallestDeclsWithM ::
    forall a.
    (HasDecls a) =>
    SrcSpan ->
    (LHsDecl GhcPs -> TransformT (Either String) (Maybe [LHsDecl GhcPs])) ->
    Graft (Either String) a
graftSmallestDeclsWithM dst toDecls = Graft $ \dflags a -> do
    let go [] = pure DL.empty
        go (e@(L src _) : rest)
            | dst `isSubspanOf` locA src = toDecls e >>= \case
                Just decs0 -> do
                    decs <- forM decs0 $ \decl ->
                        annotateDecl dflags decl
                    pure $ DL.fromList decs <> DL.fromList rest
                Nothing -> (DL.singleton e <>) <$> go rest
            | otherwise = (DL.singleton e <>) <$> go rest
    modifyDeclsT (fmap DL.toList . go) a

graftDeclsWithM ::
    forall a m.
    (HasDecls a, Fail.MonadFail m) =>
    SrcSpan ->
    (LHsDecl GhcPs -> TransformT m (Maybe [LHsDecl GhcPs])) ->
    Graft m a
graftDeclsWithM dst toDecls = Graft $ \dflags a -> do
    let go [] = pure DL.empty
        go (e@(L src _) : rest)
            | locA src `eqSrcSpan` dst = toDecls e >>= \case
                Just decs0 -> do
                    decs <- forM decs0 $ \decl ->
                        hoistTransform (either Fail.fail pure) $
                          annotateDecl dflags decl
                    pure $ DL.fromList decs <> DL.fromList rest
                Nothing -> (DL.singleton e <>) <$> go rest
            | otherwise = (DL.singleton e <>) <$> go rest
    modifyDeclsT (fmap DL.toList . go) a


-- In 9.2+, we need `Default l` to do `setPrecedingLines` on annotated elements.
-- In older versions, we pass around annotations explicitly, so the instance isn't needed.
class
    ( Data ast
    , Typeable l
    , Outputable l
    , Outputable ast
#if !MIN_VERSION_ghc(9,9,0)
    , Default l
#endif
    ) => ASTElement l ast | ast -> l where
    parseAST :: Parser (LocatedAn l ast)
    maybeParensAST :: LocatedAn l ast -> LocatedAn l ast
    {- | Construct a 'Graft', replacing the node at the given 'SrcSpan' with
        the given @Located ast@. The node at that position must already be
        a @Located ast@, or this is a no-op.
    -}
    graft ::
        forall a.
        (Data a) =>
        SrcSpan ->
        LocatedAn l ast ->
        Graft (Either String) a
    graft dst = graft' True dst . maybeParensAST

instance p ~ GhcPs => ASTElement AnnListItem (HsExpr p) where
    parseAST = parseExpr
    maybeParensAST = parenthesize
    graft = graftExpr

instance p ~ GhcPs => ASTElement AnnListItem (Pat p) where
    parseAST = parsePattern
    maybeParensAST = parenthesizePat appPrec

instance p ~ GhcPs => ASTElement AnnListItem (HsType p) where
    parseAST = parseType
    maybeParensAST = parenthesizeHsType appPrec

instance p ~ GhcPs => ASTElement AnnListItem (HsDecl p) where
    parseAST = parseDecl
    maybeParensAST = id

instance p ~ GhcPs => ASTElement AnnListItem (ImportDecl p) where
    parseAST = parseImport
    maybeParensAST = id

instance ASTElement NameAnn RdrName where
    parseAST df fp = parseWith df fp parseIdentifier
    maybeParensAST = id

------------------------------------------------------------------------------


------------------------------------------------------------------------------


-- | Given an 'LHSExpr', compute its exactprint annotations.
--   Note that this function will throw away any existing annotations (and format)
annotate :: ASTElement l ast
    => DynFlags -> Bool -> LocatedAn l ast -> TransformT (Either String) (LocatedAn l ast)
annotate dflags needs_space ast = do
    uniq <- show <$> uniqueSrcSpanT
    let rendered = render dflags ast
#if MIN_VERSION_ghc(9,4,0)
    expr' <- TransformT $ lift $ mapLeft (showSDoc dflags . ppr) $ parseAST dflags uniq rendered
    pure $ setPrecedingLines expr' 0 (bool 0 1 needs_space)
#else
    expr' <- lift $ mapLeft show $ parseAST dflags uniq rendered
    pure $ setPrecedingLines expr' 0 (bool 0 1 needs_space)
#endif

-- | Given an 'LHsDecl', compute its exactprint annotations.
annotateDecl :: DynFlags -> LHsDecl GhcPs -> TransformT (Either String) (LHsDecl GhcPs)
annotateDecl dflags ast = do
    uniq <- show <$> uniqueSrcSpanT
    let rendered = render dflags ast
#if MIN_VERSION_ghc(9,4,0)
    expr' <- TransformT $ lift $ mapLeft (showSDoc dflags . ppr) $ parseDecl dflags uniq rendered
    pure $ setPrecedingLines expr' 1 0
#else
    expr' <- lift $ mapLeft show $ parseDecl dflags uniq rendered
    pure $ setPrecedingLines expr' 1 0
#endif

------------------------------------------------------------------------------

-- | Print out something 'Outputable'.
render :: Outputable a => DynFlags -> a -> String
render dflags = showSDoc dflags . ppr

------------------------------------------------------------------------------

-- | Put parentheses around an expression if required.
parenthesize :: LHsExpr GhcPs -> LHsExpr GhcPs
parenthesize = parenthesizeHsExpr appPrec

------------------------------------------------------------------------------

-- | Equality on SrcSpan's.
-- Ignores the (Maybe BufSpan) field of SrcSpan's.
eqSrcSpan :: SrcSpan -> SrcSpan -> Bool
eqSrcSpan l r = leftmost_smallest l r == EQ

addParensToCtxt :: Maybe EpaLocation -> AnnContext -> AnnContext
addParensToCtxt close_dp = addOpen . addClose
  where
      addOpen it@AnnContext{ac_open = []} = it{ac_open = [epl 0]}
      addOpen other                       = other
      addClose it
        | Just c <- close_dp = it{ac_close = [c]}
        | AnnContext{ac_close = []} <- it = it{ac_close = [epl 0]}
        | otherwise = it

epl :: Int -> EpaLocation
epl n = EpaDelta (SameLine n) []

epAnn :: SrcSpan -> ann -> EpAnn ann
epAnn srcSpan anns = EpAnn (spanAsAnchor srcSpan) anns emptyComments

modifyAnns :: LocatedAn a ast -> (a -> a) -> LocatedAn a ast
#if MIN_VERSION_ghc(9,9,0)
modifyAnns x f = first (fmap f) x
#else
modifyAnns x f = first ((fmap.fmap) f) x
#endif

removeComma :: SrcSpanAnnA -> SrcSpanAnnA
#if MIN_VERSION_ghc(9,9,0)
removeComma  (EpAnn anc (AnnListItem as) cs)
  = EpAnn anc (AnnListItem (filter (not . isCommaAnn) as)) cs
  where
      isCommaAnn AddCommaAnn{} = True
      isCommaAnn _             = False
#else
removeComma it@(SrcSpanAnn EpAnnNotUsed _) = it
removeComma (SrcSpanAnn (EpAnn anc (AnnListItem as) cs) l)
  = SrcSpanAnn (EpAnn anc (AnnListItem (filter (not . isCommaAnn) as)) cs) l
  where
      isCommaAnn AddCommaAnn{} = True
      isCommaAnn _             = False
#endif

addParens :: Bool -> GHC.NameAnn -> GHC.NameAnn
addParens True it@NameAnn{} =
        it{nann_adornment = NameParens, nann_open = epl 0, nann_close = epl 0 }
addParens True it@NameAnnCommas{} =
        it{nann_adornment = NameParens, nann_open = epl 0, nann_close = epl 0 }
addParens True it@NameAnnOnly{} =
        it{nann_adornment = NameParens, nann_open = epl 0, nann_close = epl 0 }
addParens True NameAnnTrailing{..} =
        NameAnn{nann_adornment = NameParens, nann_open = epl 0, nann_close = epl 0, nann_name = epl 0, ..}
addParens _ it = it

removeTrailingComma :: GenLocated SrcSpanAnnA ast -> GenLocated SrcSpanAnnA ast
removeTrailingComma = flip modifyAnns $ \(AnnListItem l) -> AnnListItem $ filter (not . isCommaAnn) l

isCommaAnn :: TrailingAnn -> Bool
isCommaAnn AddCommaAnn{} = True
isCommaAnn _             = False
