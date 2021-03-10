{-# LANGUAGE CPP          #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Development.IDE.GHC.ExactPrint
    ( Graft(..),
      graftDecls,
      graftDeclsWithM,
      annotate,
      annotateDecl,
      hoistGraft,
      graftWithM,
      genericGraftWithSmallestM,
      genericGraftWithLargestM,
      graftSmallestDeclsWithM,
      transform,
      transformM,
      useAnnotatedSource,
      annotateParsedSource,
      getAnnotatedParsedSourceRule,
      GetAnnotatedParsedSource(..),
      ASTElement (..),
      ExceptStringT (..),
      Annotated(..),
      TransformT,
      Anns,
      Annotate,
      mkBindListT,
    )
where

import BasicTypes (appPrec)
import Control.Applicative (Alternative)
import Control.Monad
import qualified Control.Monad.Fail as Fail
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Zip
import qualified Data.DList as DL
import Data.Either.Extra (mapLeft)
import Data.Functor.Classes
import Data.Functor.Contravariant
import qualified Data.Text as T
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Service (runAction)
import Development.IDE.Core.Shake
import Development.IDE.GHC.Compat hiding (parseExpr)
import Development.IDE.Types.Location
import Development.Shake (RuleResult, Rules)
import Development.Shake.Classes
import qualified GHC.Generics as GHC
import Generics.SYB
import Ide.PluginUtils
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.LSP.Types
import Language.LSP.Types.Capabilities (ClientCapabilities)
import Outputable (Outputable, ppr, showSDoc)
import Retrie.ExactPrint hiding (parseDecl, parseExpr, parsePattern, parseType)
import Parser (parseIdentifier)
import Data.Traversable (for)
import Data.Foldable (Foldable(fold))
import Data.Bool (bool)
import Data.Monoid (All(All), Any(Any))
import Data.Functor.Compose (Compose(Compose))
#if __GLASGOW_HASKELL__ == 808
import Control.Arrow
#endif


------------------------------------------------------------------------------

data GetAnnotatedParsedSource = GetAnnotatedParsedSource
  deriving (Eq, Show, Typeable, GHC.Generic)

instance Hashable GetAnnotatedParsedSource
instance NFData GetAnnotatedParsedSource
instance Binary GetAnnotatedParsedSource
type instance RuleResult GetAnnotatedParsedSource = Annotated ParsedSource

-- | Get the latest version of the annotated parse source with comments.
getAnnotatedParsedSourceRule :: Rules ()
getAnnotatedParsedSourceRule = define $ \GetAnnotatedParsedSource nfp -> do
  pm <- use GetParsedModuleWithComments nfp
  return ([], fmap annotateParsedSource pm)

annotateParsedSource :: ParsedModule -> Annotated ParsedSource
annotateParsedSource = fixAnns

useAnnotatedSource ::
  String ->
  IdeState ->
  NormalizedFilePath ->
  IO (Maybe (Annotated ParsedSource))
useAnnotatedSource herald state nfp =
    runAction herald state (use GetAnnotatedParsedSource nfp)
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
needsParensSpace HsLamCase{}     = (All False, All False)
needsParensSpace HsApp{}         = mempty
needsParensSpace HsAppType{}     = mempty
needsParensSpace OpApp{}         = mempty
needsParensSpace HsPar{}         = (All False, All False)
needsParensSpace SectionL{}      = (All False, All False)
needsParensSpace SectionR{}      = (All False, All False)
needsParensSpace ExplicitTuple{} = (All False, All False)
needsParensSpace ExplicitSum{}   = (All False, All False)
needsParensSpace HsCase{}        = (All False, All False)
needsParensSpace HsIf{}          = (All False, All False)
needsParensSpace HsMultiIf{}     = (All False, All False)
needsParensSpace HsLet{}         = (All False, All False)
needsParensSpace HsDo{}          = (All False, All False)
needsParensSpace ExplicitList{}  = (All False, All False)
needsParensSpace RecordCon{}     = (All False, All False)
needsParensSpace RecordUpd{}     = mempty
needsParensSpace _               = mempty


------------------------------------------------------------------------------

{- | Construct a 'Graft', replacing the node at the given 'SrcSpan' with the
 given @Located ast@. The node at that position must already be a @Located
 ast@, or this is a no-op.
-}
graft' ::
    forall ast a.
    (Data a, ASTElement ast) =>
    -- | Do we need to insert a space before this grafting? In do blocks, the
    -- answer is no, or we will break layout. But in function applications,
    -- the answer is yes, or the function call won't get its argument. Yikes!
    --
    -- More often the answer is yes, so when in doubt, use that.
    Bool ->
    SrcSpan ->
    Located ast ->
    Graft (Either String) a
graft' needs_space dst val = Graft $ \dflags a -> do
    (anns, val') <- annotate dflags needs_space val
    modifyAnnsT $ mappend anns
    pure $
        everywhere'
            ( mkT $
                \case
                    (L src _ :: Located ast) | src == dst -> val'
                    l -> l
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
    -- Traverse the tree, looking for our replacement node. But keep track of
    -- the context (parent HsExpr constructor) we're in while we do it. This
    -- lets us determine wehther or not we need parentheses.
    let (All needs_parens, All needs_space) =
          everythingWithContext (All True, All True) (<>)
            ( mkQ (mempty, ) $ \x s -> case x of
                (L src _ :: LHsExpr GhcPs) | src == dst ->
                  (s, s)
                L _ x' -> (mempty, needsParensSpace x')
            ) a

    runGraft
      (graft' needs_space dst $ bool id maybeParensAST needs_parens val)
      dflags
      a


------------------------------------------------------------------------------

graftWithM ::
    forall ast m a.
    (Fail.MonadFail m, Data a, ASTElement ast) =>
    SrcSpan ->
    (Located ast -> TransformT m (Maybe (Located ast))) ->
    Graft m a
graftWithM dst trans = Graft $ \dflags a -> do
    everywhereM'
        ( mkM $
            \case
                val@(L src _ :: Located ast)
                    | src == dst -> do
                        mval <- trans val
                        case mval of
                            Just val' -> do
                                (anns, val'') <-
                                    hoistTransform (either Fail.fail pure) $
                                        annotate dflags True $ maybeParensAST val'
                                modifyAnnsT $ mappend anns
                                pure val''
                            Nothing -> pure val
                l -> pure l
        )
        a

genericIsSubspan :: forall ast. Typeable ast => Proxy (Located ast) -> SrcSpan -> GenericQ (Maybe Bool)
genericIsSubspan _ dst = mkQ Nothing $ \case
  (L span _ :: Located ast) -> Just $ dst `isSubspanOf` span

-- | Run the given transformation only on the smallest node in the tree that
-- contains the 'SrcSpan'.
genericGraftWithSmallestM ::
    forall m a ast.
    (Monad m, Data a, Typeable ast) =>
    Proxy (Located ast) ->
    SrcSpan ->
    (DynFlags -> GenericM (TransformT m)) ->
    Graft m a
genericGraftWithSmallestM proxy dst trans = Graft $ \dflags ->
    smallestM (genericIsSubspan proxy dst) (trans dflags)

-- | Run the given transformation only on the largest node in the tree that
-- contains the 'SrcSpan'.
genericGraftWithLargestM ::
    forall m a ast.
    (Monad m, Data a, Typeable ast) =>
    Proxy (Located ast) ->
    SrcSpan ->
    (DynFlags -> GenericM (TransformT m)) ->
    Graft m a
genericGraftWithLargestM proxy dst trans = Graft $ \dflags ->
    largestM (genericIsSubspan proxy dst) (trans dflags)


-- | Lift a function that replaces a value with several values into a generic
-- function. The result doesn't perform any searching, so should be driven via
-- 'everywhereM' or friends.
mkBindListT :: forall b m. (Typeable b, Data b, Monad m) => (b -> m [b]) -> GenericM m
mkBindListT f = mkM $ fmap join . traverse f


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
            | src == dst = DL.fromList decs <> DL.fromList rest
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
            | dst `isSubspanOf` src = toDecls e >>= \case
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
            | src == dst = toDecls e >>= \case
                Just decs0 -> do
                    decs <- forM decs0 $ \decl ->
                        hoistTransform (either Fail.fail pure) $
                          annotateDecl dflags decl
                    pure $ DL.fromList decs <> DL.fromList rest
                Nothing -> (DL.singleton e <>) <$> go rest
            | otherwise = (DL.singleton e <>) <$> go rest
    modifyDeclsT (fmap DL.toList . go) a


everywhereM' :: forall m. Monad m => GenericM m -> GenericM m
everywhereM' f = go
    where
        go :: GenericM m
        go = gmapM go <=< f

class (Data ast, Outputable ast) => ASTElement ast where
    parseAST :: Parser (Located ast)
    maybeParensAST :: Located ast -> Located ast
    {- | Construct a 'Graft', replacing the node at the given 'SrcSpan' with
        the given @Located ast@. The node at that position must already be
        a @Located ast@, or this is a no-op.
    -}
    graft ::
        forall a.
        (Data a) =>
        SrcSpan ->
        Located ast ->
        Graft (Either String) a
    graft dst = graft' True dst . maybeParensAST

instance p ~ GhcPs => ASTElement (HsExpr p) where
    parseAST = parseExpr
    maybeParensAST = parenthesize
    graft = graftExpr

instance p ~ GhcPs => ASTElement (Pat p) where
#if __GLASGOW_HASKELL__ == 808
    parseAST = fmap (fmap $ right $ second dL) . parsePattern
    maybeParensAST = dL . parenthesizePat appPrec . unLoc
#else
    parseAST = parsePattern
    maybeParensAST = parenthesizePat appPrec
#endif

instance p ~ GhcPs => ASTElement (HsType p) where
    parseAST = parseType
    maybeParensAST = parenthesizeHsType appPrec

instance p ~ GhcPs => ASTElement (HsDecl p) where
    parseAST = parseDecl
    maybeParensAST = id

instance p ~ GhcPs => ASTElement (ImportDecl p) where
    parseAST = parseImport
    maybeParensAST = id

instance ASTElement RdrName where
    parseAST df fp = parseWith df fp parseIdentifier
    maybeParensAST = id

------------------------------------------------------------------------------

-- | Dark magic I stole from retrie. No idea what it does.
fixAnns :: ParsedModule -> Annotated ParsedSource
fixAnns ParsedModule {..} =
    let ranns = relativiseApiAnns pm_parsed_source pm_annotations
     in unsafeMkA pm_parsed_source ranns 0

------------------------------------------------------------------------------

-- | Given an 'LHSExpr', compute its exactprint annotations.
--   Note that this function will throw away any existing annotations (and format)
annotate :: ASTElement ast => DynFlags -> Bool -> Located ast -> TransformT (Either String) (Anns, Located ast)
annotate dflags needs_space ast = do
    uniq <- show <$> uniqueSrcSpanT
    let rendered = render dflags ast
    (anns, expr') <- lift $ mapLeft show $ parseAST dflags uniq rendered
    let anns' = setPrecedingLines expr' 0 (bool 0 1 needs_space) anns
    pure (anns', expr')

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

    (anns', alts') <- fmap unzip $ for (zip [0..] alts) $ \(ix :: Int, alt) -> do
      uniq <- show <$> uniqueSrcSpanT
      let rendered = render dflags $ set_matches [alt]
      lift (mapLeft show $ parseDecl dflags uniq rendered) >>= \case
        (ann, L _ (ValD _ FunBind { fun_matches = MG { mg_alts = L _ [alt']}}))
           -> pure (bool id (setPrecedingLines alt' 1 0) (ix /= 0) ann, alt')
        _ ->  lift $ Left "annotateDecl: didn't parse a single FunBind match"

    let expr' = L src $ set_matches alts'
        anns'' = setPrecedingLines expr' 1 0 $ fold anns'

    modifyAnnsT $ mappend anns''
    pure expr'
annotateDecl dflags ast = do
    uniq <- show <$> uniqueSrcSpanT
    let rendered = render dflags ast
    (anns, expr') <- lift $ mapLeft show $ parseDecl dflags uniq rendered
    let anns' = setPrecedingLines expr' 1 0 anns
    modifyAnnsT $ mappend anns'
    pure expr'

------------------------------------------------------------------------------

-- | Print out something 'Outputable'.
render :: Outputable a => DynFlags -> a -> String
render dflags = showSDoc dflags . ppr

------------------------------------------------------------------------------

-- | Put parentheses around an expression if required.
parenthesize :: LHsExpr GhcPs -> LHsExpr GhcPs
parenthesize = parenthesizeHsExpr appPrec


------------------------------------------------------------------------------
-- Custom SYB machinery
------------------------------------------------------------------------------

-- | Generic monadic transformations that return side-channel data.
type GenericMQ r m = forall a. Data a => a -> m (r, a)

------------------------------------------------------------------------------
-- | Apply the given 'GenericM' at all every node whose children fail the
-- 'GenericQ', but which passes the query itself.
smallestM :: forall m. Monad m => GenericQ (Maybe Bool) -> GenericM m -> GenericM m
smallestM q f = fmap snd . go
  where
    go :: GenericMQ Any m
    go x = do
      case q x of
        Nothing -> gmapMQ go x
        Just True -> do
          it@(r, x') <- gmapMQ go x
          case r of
            Any True -> pure it
            Any False -> fmap (Any True,) $ f x'
        Just False -> pure (mempty, x)

------------------------------------------------------------------------------
-- | Apply the given 'GenericM' at every node that passes the 'GenericQ', but
-- don't descend into children if the query matches.
largestM :: forall m. Monad m => GenericQ (Maybe Bool) -> GenericM m -> GenericM m
largestM q f = go
  where
    go :: GenericM m
    go x = do
      case q x of
        Just True -> f x
        Just False -> pure x
        Nothing -> gmapM go x

newtype MonadicQuery r m a = MonadicQuery
  { runMonadicQuery :: m (r, a)
  }
  deriving stock (Functor)
  deriving Applicative via Compose m ((,) r)


------------------------------------------------------------------------------
-- | Like 'gmapM', but also returns side-channel data.
gmapMQ ::
    forall f r a. (Monoid r, Data a, Applicative f) =>
    (forall d. Data d => d -> f (r, d)) ->
    a ->
    f (r, a)
gmapMQ f = runMonadicQuery . gfoldl k pure
  where
    k :: Data d => MonadicQuery r f (d -> b) -> d -> MonadicQuery r f b
    k c x = c <*> MonadicQuery (f x)

