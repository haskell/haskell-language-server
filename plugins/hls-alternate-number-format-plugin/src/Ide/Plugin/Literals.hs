{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
module Ide.Plugin.Literals where
import           Data.Maybe                    (maybeToList)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Util      (unsafePrintSDoc)
import           Development.IDE.Graph.Classes (NFData)
import qualified GHC.Generics                  as GHC
import           Generics.SYB

-- data type to capture what type of literal we are dealing with
-- provides location and possibly source text (for OverLits) as well as it's value
-- we currently don't have any use for PrimLiterals. They never have source text so we always drop them
data Literal = IntLiteral      SrcSpan (Maybe Text) Integer
             | FracLiteral     SrcSpan (Maybe Text) Rational
             | IntPrimLiteral  SrcSpan (Maybe Text) Integer
             | FracPrimLiteral SrcSpan (Maybe Text) Rational
             deriving (GHC.Generic, Show)

instance NFData Literal

getSrcText :: Literal -> Maybe Text
getSrcText = \case
  IntLiteral _ txt _      -> txt
  FracLiteral _ txt _     -> txt
  IntPrimLiteral _ txt _  -> txt
  FracPrimLiteral _ txt _ -> txt

getSrcSpan :: Literal -> SrcSpan
getSrcSpan = \case
    IntLiteral ss _ _      -> ss
    FracLiteral ss _ _     -> ss
    IntPrimLiteral ss _ _  -> ss
    FracPrimLiteral ss _ _ -> ss


-- | Find all literals in a Parsed Source File
collectLiterals :: (Data ast, Typeable ast) => ast -> [Literal]
collectLiterals = everything (<>) (mkQ2 ([] :: [Literal])  traverseLExpr traverseLPat)

-- Located Patterns for whatever reason don't get picked up when using `(mkQ ([] :: [Literal]) traverseLExpr)
-- as such we need to explicit traverse those in order to pull out any literals
mkQ2 :: (Typeable a, Typeable b, Typeable c) => r -> (b -> r) -> (c -> r) -> a -> r
mkQ2 def left right datum = case cast datum of
  Just datum' -> left datum'
  Nothing     -> maybe def right (cast datum)

traverseLPat :: GenLocated SrcSpan (Pat GhcPs) -> [Literal]
traverseLPat (L sSpan pat) = traversePat sSpan pat

traversePat :: SrcSpan -> Pat GhcPs -> [Literal]
traversePat sSpan = \case
    LitPat _ lit                             -> getLiteralAsList sSpan lit
    NPat _ (L olSpan overLit) sexpr1 sexpr2  -> getOverLiteralAsList olSpan overLit
                                            <> collectLiterals sexpr1
                                            <> collectLiterals sexpr2
    NPlusKPat _ _ (L olSpan loverLit) overLit sexpr1 sexpr2 -> getOverLiteralAsList olSpan loverLit
                                                    <> getOverLiteralAsList sSpan overLit
                                                    <> collectLiterals sexpr1
                                                    <> collectLiterals sexpr2
    ast -> collectLiterals ast

traverseLExpr :: GenLocated SrcSpan (HsExpr GhcPs) -> [Literal]
traverseLExpr (L sSpan hsExpr) = traverseExpr sSpan hsExpr

traverseExpr :: SrcSpan -> HsExpr GhcPs -> [Literal]
traverseExpr sSpan = \case
      HsOverLit _ overLit -> getOverLiteralAsList sSpan overLit
      HsLit _ lit         -> getLiteralAsList sSpan lit
      expr                -> collectLiterals expr

getLiteralAsList :: SrcSpan -> HsLit GhcPs -> [Literal]
getLiteralAsList sSpan = maybeToList . getLiteral sSpan

-- Translate from Hs Type to our Literal type
getLiteral :: SrcSpan -> HsLit GhcPs -> Maybe Literal
getLiteral sSpan = \case
  HsInt _ val                 -> Just $ fromIntegralLit sSpan val
  HsIntPrim _ val             -> Just $ IntPrimLiteral sSpan Nothing val
  HsWordPrim _ val            -> Just $ IntPrimLiteral sSpan Nothing val
  HsInt64Prim _ val           -> Just $ IntPrimLiteral sSpan Nothing val
  HsWord64Prim _ val          -> Just $ IntPrimLiteral sSpan Nothing val
  HsInteger _ val _           -> Just $ IntLiteral sSpan Nothing val
  HsRat _ val _               -> Just $ fromFractionalLit sSpan val
  HsFloatPrim _ (FL _ _ val)  -> Just $ FracPrimLiteral sSpan Nothing val
  HsDoublePrim _ (FL _ _ val) -> Just $ FracPrimLiteral sSpan Nothing val
  _                           -> Nothing

getOverLiteralAsList :: SrcSpan -> HsOverLit GhcPs -> [Literal]
getOverLiteralAsList sSpan = maybeToList . getOverLiteral sSpan

getOverLiteral :: SrcSpan -> HsOverLit GhcPs -> Maybe Literal
getOverLiteral sSpan OverLit{..} = case ol_val of
  HsIntegral il   -> Just $ fromIntegralLit sSpan il
  HsFractional fl -> Just $ fromFractionalLit sSpan fl
  _               -> Nothing
getOverLiteral _ _ = Nothing

fromIntegralLit :: SrcSpan -> IntegralLit -> Literal
fromIntegralLit s (IL txt _ val) = IntLiteral s (fromSourceText txt) val

fromFractionalLit  :: SrcSpan -> FractionalLit -> Literal
fromFractionalLit s (FL txt _ val) = FracLiteral s (fromSourceText txt) val

fromSourceText :: SourceText -> Maybe Text
fromSourceText = \case
  SourceText s -> Just $ T.pack s
  NoSourceText -> Nothing

-- mostly for debugging purposes
literalToString :: HsLit p -> String
literalToString = \case
  HsChar _ c        -> "Char: " <> show c
  HsCharPrim _ c    -> "CharPrim: " <> show c
  HsString _ fs     -> "String: " <> show fs
  HsStringPrim _ bs -> "StringPrim: " <> show bs
  HsInt _ il        -> "Int: " <> show il
  HsIntPrim _ n     -> "IntPrim: " <> show n
  HsWordPrim _ n    -> "WordPrim: " <> show n
  HsInt64Prim _ n   -> "Int64Prim: " <> show n
  HsWord64Prim _ n  -> "Word64Prim: " <> show n
  HsInteger _ n ty  -> "Integer: " <> show n <> " Type: " <> tyToLiteral ty
  HsRat _ fl ty     -> "Rat: " <> show fl <> " Type: " <> tyToLiteral ty
  HsFloatPrim _ fl  -> "FloatPrim: " <> show fl
  HsDoublePrim _ fl -> "DoublePrim: " <>  show fl
  _                 -> "XHsLit"
  where
    tyToLiteral :: Type -> String
    tyToLiteral = unsafePrintSDoc .  ppr

overLitToString :: OverLitVal -> String
overLitToString = \case
     HsIntegral int -> case int of { IL _ _ val -> "IntegralOverLit: " <> show val }
     HsFractional frac -> case frac of { FL _ _ val -> "RationalOverLit: " <> show val }
     HsIsString _ str -> "HIsString: " <> show str
