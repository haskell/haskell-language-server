{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TypeFamilies           #-}

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
#if !MIN_VERSION_ghc(9,2,0)
      Anns,
      Annotate,
      setPrecedingLinesT,
#else
      addParens,
      addParensToCtxt,
      modifyAnns,
      removeComma,
      -- * Helper function
      eqSrcSpan,
      epl,
      epAnn,
      removeTrailingComma,
#endif
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
import           Control.Arrow                           (right, (***))
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
import           Data.Foldable                           (Foldable (fold))
import           Data.Functor.Classes
import           Data.Functor.Contravariant
import           Data.Monoid                             (All (All), getAll)
import qualified Data.Text                               as T
import           Data.Traversable                        (for)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Service            (runAction)
import           Development.IDE.Core.Shake              hiding (Log)
import qualified Development.IDE.Core.Shake              as Shake
import           Development.IDE.GHC.Compat              hiding (parseImport,
                                                          parsePattern,
                                                          parseType)
import           Development.IDE.Graph                   (RuleResult, Rules)
import           Development.IDE.Graph.Classes
import           Development.IDE.Types.Location
import           Development.IDE.Types.Logger            (Pretty (pretty),
                                                          Recorder,
                                                          WithPriority,
                                                          cmapWithPrio)
import qualified GHC.Generics                            as GHC
import           Generics.SYB
import           Generics.SYB.GHC
import           Ide.PluginUtils
import           Language.Haskell.GHC.ExactPrint.Parsers
import           Language.LSP.Types
import           Language.LSP.Types.Capabilities         (ClientCapabilities)
import           Retrie.ExactPrint                       hiding (Annotated (..),
                                                          parseDecl, parseExpr,
                                                          parsePattern,
                                                          parseType)
#if MIN_VERSION_ghc(9,2,0)
import           GHC                                     (EpAnn (..),
                                                          NameAdornment (NameParens),
                                                          NameAnn (..),
                                                          SrcSpanAnn' (SrcSpanAnn),
                                                          SrcSpanAnnA,
                                                          TrailingAnn (AddCommaAnn),
                                                          emptyComments,
                                                          spanAsAnchor)
import           GHC.Parser.Annotation                   (AnnContext (..),
                                                          DeltaPos (SameLine),
                                                          EpaLocation (EpaDelta))
#endif

------------------------------------------------------------------------------

data Log = LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog

data GetAnnotatedParsedSource = GetAnnotatedParsedSource
  deriving (Eq, Show, Typeable, GHC.Generic)

instance Hashable GetAnnotatedParsedSource
instance NFData GetAnnotatedParsedSource
type instance RuleResult GetAnnotatedParsedSource = Annotated ParsedSource

-- | Get the latest version of the annotated parse source with comments.
getAnnotatedParsedSourceRule :: Recorder (WithPriority Log) -> Rules ()
getAnnotatedParsedSourceRule recorder = define (cmapWithPrio LogShake recorder) $ \GetAnnotatedParsedSource nfp -> do
  pm <- use GetParsedModuleWithComments nfp
  return ([], fmap annotateParsedSource pm)

#if MIN_VERSION_ghc(9,2,0)
annotateParsedSource :: ParsedModule -> Annotated ParsedSource
annotateParsedSource (ParsedModule _ ps _ _) = unsafeMkA (makeDeltaAst ps) 0
#else
annotateParsedSource :: ParsedModule -> Annotated ParsedSource
annotateParsedSource = fixAnns
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
    Uri ->
    Graft (Either String) ParsedSource ->
    Annotated ParsedSource ->
    Either String WorkspaceEdit
transform dflags ccs uri f a = do
    let src = printA a
    a' <- transformA a $ runGraft f dflags
    let res = printA a'
    pure $ diffText ccs (uri, T.pack src) (T.pack res) IncludeDeletions

------------------------------------------------------------------------------

-- | Convert a 'Graft' into a 'WorkspaceEdit'.
transformM ::
    Monad m =>
    DynFlags ->
    ClientCapabilities ->
    Uri ->
    Graft (ExceptStringT m) ParsedSource ->
    Annotated ParsedSource ->
    m (Either String WorkspaceEdit)
transformM dflags ccs uri f a = runExceptT $
    runExceptString $ do
        let src = printA a
        a' <- transformA a $ runGraft f dflags
        let res = printA a'
        pure $ diffText ccs (uri, T.pack src) (T.pack res) IncludeDeletions


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
needsParensSpace HsLamCase{}     = (All False, All True)
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
    (Data a, Typeable l, ASTElement l ast) =>
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
#if MIN_VERSION_ghc(9,2,0)
    val' <- annotate dflags needs_space val
#else
    (anns, val') <- annotate dflags needs_space val
    modifyAnnsT $ mappend anns
#endif
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
  -- lets us determine wehther or not we need parentheses.
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
#if MIN_VERSION_ghc(9,2,0)
                                val'' <-
                                    hoistTransform (either Fail.fail pure)
                                        (annotate @AnnListItem @(HsExpr GhcPs) dflags needs_space (mk_parens val'))
                                pure val''
#else
                                (anns, val'') <-
                                    hoistTransform (either Fail.fail pure)
                                        (annotate @AnnListItem @(HsExpr GhcPs) dflags needs_space (mk_parens val'))
                                modifyAnnsT $ mappend anns
                                pure val''
#endif
                            Nothing -> pure val
                l -> pure l
        )
        a

graftWithM ::
    forall ast m a l.
    (Fail.MonadFail m, Data a, Typeable l, ASTElement l ast) =>
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
#if MIN_VERSION_ghc(9,2,0)
                                val'' <-
                                    hoistTransform (either Fail.fail pure) $
                                        annotate dflags True $ maybeParensAST val'
                                pure val''
#else
                                (anns, val'') <-
                                    hoistTransform (either Fail.fail pure) $
                                        annotate dflags True $ maybeParensAST val'
                                modifyAnnsT $ mappend anns
                                pure val''
#endif
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


class (Data ast, Typeable l, Outputable l, Outputable ast) => ASTElement l ast | ast -> l where
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
#if __GLASGOW_HASKELL__ == 808
    parseAST = fmap (fmap $ right $ second dL) . parsePattern
    maybeParensAST = dL . parenthesizePat appPrec . unLoc
#else
    parseAST = parsePattern
    maybeParensAST = parenthesizePat appPrec
#endif

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

#if !MIN_VERSION_ghc(9,2,0)
-- | Dark magic I stole from retrie. No idea what it does.
fixAnns :: ParsedModule -> Annotated ParsedSource
fixAnns ParsedModule {..} =
    let ranns = relativiseApiAnns pm_parsed_source pm_annotations
     in unsafeMkA pm_parsed_source ranns 0
#endif

------------------------------------------------------------------------------

-- | Given an 'LHSExpr', compute its exactprint annotations.
--   Note that this function will throw away any existing annotations (and format)
annotate :: (ASTElement l ast, Outputable l)
#if MIN_VERSION_ghc(9,2,0)
    => DynFlags -> Bool -> LocatedAn l ast -> TransformT (Either String) (LocatedAn l ast)
#else
    => DynFlags -> Bool -> LocatedAn l ast -> TransformT (Either String) (Anns, LocatedAn l ast)
#endif
annotate dflags needs_space ast = do
    uniq <- show <$> uniqueSrcSpanT
    let rendered = render dflags ast
#if MIN_VERSION_ghc(9,2,0)
    expr' <- lift $ mapLeft show $ parseAST dflags uniq rendered
    pure expr'
#else
    (anns, expr') <- lift $ mapLeft show $ parseAST dflags uniq rendered
    let anns' = setPrecedingLines expr' 0 (bool 0 1 needs_space) anns
    pure (anns',expr')
#endif

-- | Given an 'LHsDecl', compute its exactprint annotations.
annotateDecl :: DynFlags -> LHsDecl GhcPs -> TransformT (Either String) (LHsDecl GhcPs)
-- The 'parseDecl' function fails to parse 'FunBind' 'ValD's which contain
-- multiple matches. To work around this, we split the single
-- 'FunBind'-of-multiple-'Match'es into multiple 'FunBind's-of-one-'Match',
-- and then merge them all back together.
annotateDecl dflags
            (L src (
                ValD ext fb@FunBind
                  { fun_matches = mg@MG { mg_alts = L alt_src alts@(_:_)}
                  })) = do
    let set_matches matches =
          ValD ext fb { fun_matches = mg { mg_alts = L alt_src matches }}

#if MIN_VERSION_ghc(9,2,0)
    alts' <- for alts $ \alt -> do
      uniq <- show <$> uniqueSrcSpanT
      let rendered = render dflags $ set_matches [alt]
      lift (mapLeft show $ parseDecl dflags uniq rendered) >>= \case
        (L _ (ValD _ FunBind { fun_matches = MG { mg_alts = L _ [alt']}}))
           -> pure alt'
        _ ->  lift $ Left "annotateDecl: didn't parse a single FunBind match"

    pure $ L src $ set_matches alts'
#else
    (anns', alts') <- fmap unzip $ for alts $ \alt -> do
      uniq <- show <$> uniqueSrcSpanT
      let rendered = render dflags $ set_matches [alt]
      lift (mapLeft show $ parseDecl dflags uniq rendered) >>= \case
        (ann, L _ (ValD _ FunBind { fun_matches = MG { mg_alts = L _ [alt']}}))
           -> pure (setPrecedingLines alt' 1 0 ann, alt')
        _ ->  lift $ Left "annotateDecl: didn't parse a single FunBind match"

    modifyAnnsT $ mappend $ fold anns'
    pure $ L src $ set_matches alts'
#endif
annotateDecl dflags ast = do
    uniq <- show <$> uniqueSrcSpanT
    let rendered = render dflags ast
#if MIN_VERSION_ghc(9,2,0)
    lift $ mapLeft show $ parseDecl dflags uniq rendered
#else
    (anns, expr') <- lift $ mapLeft show $ parseDecl dflags uniq rendered
    let anns' = setPrecedingLines expr' 1 0 anns
    modifyAnnsT $ mappend anns'
    pure expr'
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

-- | Equality on SrcSpan's.
-- Ignores the (Maybe BufSpan) field of SrcSpan's.
#if MIN_VERSION_ghc(9,2,0)
eqSrcSpanA :: SrcAnn la -> SrcAnn b -> Bool
eqSrcSpanA l r = leftmost_smallest (locA l) (locA r) == EQ
#else
eqSrcSpanA :: SrcSpan -> SrcSpan -> Bool
eqSrcSpanA l r = leftmost_smallest l r == EQ
#endif

#if MIN_VERSION_ghc(9,2,0)
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
modifyAnns x f = first ((fmap.fmap) f) x

removeComma :: SrcSpanAnnA -> SrcSpanAnnA
removeComma it@(SrcSpanAnn EpAnnNotUsed _) = it
removeComma (SrcSpanAnn (EpAnn anc (AnnListItem as) cs) l)
  = (SrcSpanAnn (EpAnn anc (AnnListItem (filter (not . isCommaAnn) as)) cs) l)
  where
      isCommaAnn AddCommaAnn{} = True
      isCommaAnn _             = False

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
#endif
