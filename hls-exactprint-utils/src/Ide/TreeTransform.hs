{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Ide.TreeTransform
    ( Graft(..),
      graft,
      graftDecls,
      graftDeclsWithM,
      hoistGraft,
      graftWithM,
      graftWithSmallestM,
      transform,
      transformM,
      useAnnotatedSource,
      annotateParsedSource,
      ASTElement (..),
      ExceptStringT (..),
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
import Data.Functor.Classes
import Data.Functor.Contravariant
import qualified Data.Text as T
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Rules
import Development.IDE.Core.Shake
import Development.IDE.GHC.Compat hiding (parseExpr)
import Development.IDE.Types.Location
import Generics.SYB
import Ide.PluginUtils
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Capabilities (ClientCapabilities)
import Outputable (Outputable, ppr, showSDoc)
import Retrie.ExactPrint hiding (parseDecl, parseExpr, parsePattern, parseType)
import qualified "ghc" SrcLoc

------------------------------------------------------------------------------

-- | Get the latest version of the annotated parse source.
useAnnotatedSource ::
    String ->
    IdeState ->
    NormalizedFilePath ->
    IO (Maybe (Annotated ParsedSource))
useAnnotatedSource herald state nfp =
    fmap annotateParsedSource
        <$> runAction herald state (use GetParsedModule nfp)

annotateParsedSource :: ParsedModule -> Annotated ParsedSource
annotateParsedSource = fixAnns

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

------------------------------------------------------------------------------

{- | Construct a 'Graft', replacing the node at the given 'SrcSpan' with the
 given 'LHSExpr'. The node at that position must already be a 'LHsExpr', or
 this is a no-op.
-}
graft ::
    forall ast a.
    (Data a, ASTElement ast) =>
    SrcSpan ->
    ToL ast GhcPs ->
    Graft (Either String) a
graft dst val = Graft $ \dflags a -> do
    (anns, val') <- annotate dflags $ maybeParensAST val
    modifyAnnsT $ mappend anns
    pure $
        everywhere'
            ( mkT $
                \case
                    (src :: ToL ast GhcPs) | location src == dst -> val'
                    l -> l
            )
            a

------------------------------------------------------------------------------

graftWithM ::
    forall ast m a.
    (Fail.MonadFail m, Data a, ASTElement ast) =>
    SrcSpan ->
    (ToL ast GhcPs -> TransformT m (Maybe (ToL ast GhcPs))) ->
    Graft m a
graftWithM dst trans = Graft $ \dflags a -> do
    everywhereM'
        ( mkM $
            \case
                (val :: ToL ast GhcPs)
                    | getLoc val == dst -> do
                        mval <- trans val
                        case mval of
                            Just val' -> do
                                (anns, val'') <-
                                    hoistTransform (either Fail.fail pure) $
                                        annotate dflags $ maybeParensAST val'
                                modifyAnnsT $ mappend anns
                                pure val''
                            Nothing -> pure val
                l -> pure l
        )
        a

graftWithSmallestM ::
    forall ast m a.
    (Fail.MonadFail m, Data a, ASTElement ast) =>
    SrcSpan ->
    (ToL ast GhcPs -> TransformT m (Maybe (ToL ast GhcPs))) ->
    Graft m a
graftWithSmallestM dst trans = Graft $ \dflags a -> do
    everywhereM'
        ( mkM $
            \case
                (val :: ToL ast GhcPs)
                    | dst `isSubspanOf` getLoc val -> do
                        mval <- trans val
                        case mval of
                            Just val' -> do
                                (anns, val'') <-
                                    hoistTransform (either Fail.fail pure) $
                                        annotate dflags $ maybeParensAST val'
                                modifyAnnsT $ mappend anns
                                pure val''
                            Nothing -> pure val
                l -> pure l
        )
        a

graftDecls ::
    forall a.
    (HasDecls a) =>
    SrcSpan ->
    [LHsDecl GhcPs] ->
    Graft (Either String) a
graftDecls dst decs0 = Graft $ \dflags a -> do
    decs <- forM decs0 $ \decl -> do
        (anns, decl') <- annotateDecl dflags decl
        modifyAnnsT $ mappend anns
        pure decl'
    let go [] = DL.empty
        go (L src e : rest)
            | src == dst = DL.fromList decs <> DL.fromList rest
            | otherwise = DL.singleton (L src e) <> go rest
    modifyDeclsT (pure . DL.toList . go) a

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
                    decs <- forM decs0 $ \decl -> do
                        (anns, decl') <-
                            hoistTransform (either Fail.fail pure) $
                            annotateDecl dflags decl
                        modifyAnnsT $ mappend anns
                        pure decl'
                    pure $ DL.fromList decs <> DL.fromList rest
                Nothing -> (DL.singleton e <>) <$> go rest
            | otherwise = (DL.singleton e <>) <$> go rest
    modifyDeclsT (fmap DL.toList . go) a


everywhereM' :: forall m. Monad m => GenericM m -> GenericM m
everywhereM' f = go
    where
        go :: GenericM m
        go = gmapM go <=< f

class
    (   Data (ast GhcPs), Outputable (ast GhcPs),
        HasSrcSpan (ToL ast GhcPs), Data (ToL ast GhcPs),
        Outputable (ToL ast GhcPs)
    )
    => ASTElement ast where
    -- | This is to absorb the implementation difference of 'LPat',
    --   which is equal to Located Pat in 8.6 and 8.10, but
    --   is isomorphic to Pat in 8.8.
    type ToL ast p = (r :: *) | r -> ast
    type ToL ast p = Located (ast p)
    withL :: SrcSpan -> ast GhcPs -> ToL ast GhcPs
    default withL
        :: ToL ast GhcPs ~ Located (ast GhcPs)
        => SrcSpan -> ast GhcPs -> ToL ast GhcPs
    withL = L
    toLocated :: ToL ast GhcPs -> Located (ast GhcPs)
    default toLocated
        :: ToL ast GhcPs ~ Located (ast GhcPs) => ToL ast GhcPs -> Located (ast GhcPs)
    toLocated = id
    unLocated :: ToL ast GhcPs -> ast GhcPs
    default unLocated
        :: ToL ast GhcPs ~ Located (ast GhcPs) => ToL ast GhcPs -> ast GhcPs
    unLocated = unLoc
    location :: ToL ast GhcPs -> SrcSpan
    location = SrcLoc.getLoc . toLocated

    parseAST :: Parser (ToL ast GhcPs)
    maybeParensAST :: ToL ast GhcPs -> ToL ast GhcPs

instance ASTElement HsExpr where
    type ToL HsExpr p = LHsExpr p
    parseAST = parseExpr
    maybeParensAST = parenthesize

instance ASTElement Pat where
    type ToL Pat p = LPat p
#if __GLASGOW_HASKELL__ == 808
    toLocated p@(XPat (L loc _))= L loc p
    toLocated p = L noSrcSpan p
    unLocated = id
    withL = flip const
#else
    toLocated = id
    unLocated = unLoc
#endif

    parseAST = parsePattern
    maybeParensAST = parenthesizePat appPrec


instance ASTElement HsType where
    type ToL HsType p = LHsType p
    parseAST = parseType
    maybeParensAST = parenthesizeHsType appPrec

instance ASTElement HsDecl where
    type ToL HsDecl p = LHsDecl p
    parseAST = parseDecl
    maybeParensAST = id

------------------------------------------------------------------------------

-- | Dark magic I stole from retrie. No idea what it does.
fixAnns :: ParsedModule -> Annotated ParsedSource
fixAnns ParsedModule {..} =
    let ranns = relativiseApiAnns pm_parsed_source pm_annotations
     in unsafeMkA pm_parsed_source ranns 0

------------------------------------------------------------------------------

-- | Given an 'LHSExpr', compute its exactprint annotations.
annotate
    :: forall ast. ASTElement ast
    => DynFlags -> ToL ast GhcPs
    -> TransformT (Either String) (Anns, ToL ast GhcPs)
annotate dflags ast = do
    uniq <- show <$> uniqueSrcSpanT
    let rendered = render dflags ast
    (anns, expr') <- lift $ either (Left . show) Right $ parseAST dflags uniq rendered
    let anns' = setPrecedingLines
            (toLocated expr' :: Located (ast GhcPs))
            0 1 anns
    pure (anns', expr')

-- | Given an 'LHsDecl', compute its exactprint annotations.
annotateDecl :: DynFlags -> LHsDecl GhcPs -> TransformT (Either String) (Anns, LHsDecl GhcPs)
annotateDecl dflags ast = do
    uniq <- show <$> uniqueSrcSpanT
    let rendered = render dflags ast
    (anns, expr') <- lift $ either (Left . show) Right $ parseDecl dflags uniq rendered
    let anns' = setPrecedingLines expr' 1 0 anns
    pure (anns', expr')
------------------------------------------------------------------------------

-- | Print out something 'Outputable'.
render :: Outputable a => DynFlags -> a -> String
render dflags = showSDoc dflags . ppr

------------------------------------------------------------------------------

-- | Put parentheses around an expression if required.
parenthesize :: LHsExpr GhcPs -> LHsExpr GhcPs
parenthesize = parenthesizeHsExpr appPrec
