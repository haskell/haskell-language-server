{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes        #-}
module Ide.Plugin.Literals (
    collectLiterals
    , Literal(..)
    , getSrcText
    , getSrcSpan
) where

import           Data.Set                      (Set)
import qualified Data.Set                      as S
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Development.IDE.GHC.Compat    hiding (getSrcSpan)
import           Development.IDE.GHC.Util      (unsafePrintSDoc)
import           Development.IDE.Graph.Classes (NFData (rnf))
import qualified GHC.Generics                  as GHC
import           Generics.SYB                  (Data, Typeable, cast,
                                                everything)

-- data type to capture what type of literal we are dealing with
-- provides location and possibly source text (for OverLits) as well as it's value
-- we currently don't have any use for PrimLiterals. They never have source text so we always drop them
-- | Captures a Numeric Literals Location, Source Text, and Value.
data Literal = IntLiteral      RealSrcSpan Text Integer
             | FracLiteral     RealSrcSpan Text Rational
             deriving (GHC.Generic, Show, Ord, Eq)

instance NFData RealSrcSpan where
    rnf x = x `seq` ()

instance NFData Literal

-- | Return a Literal's Source representation
getSrcText :: Literal -> Text
getSrcText = \case
  IntLiteral _ txt _  -> txt
  FracLiteral _ txt _ -> txt

-- | Return a Literal's Real Source location
getSrcSpan :: Literal -> RealSrcSpan
getSrcSpan = \case
    IntLiteral ss _ _  -> ss
    FracLiteral ss _ _ -> ss

-- | Find all literals in a Parsed Source File
collectLiterals :: (Data ast, Typeable ast) => ast -> [Literal]
collectLiterals = S.toList . collectLiterals'

collectLiterals' :: (Data ast, Typeable ast) => ast -> Set Literal
collectLiterals' = everything (<>) (mkQ2 (S.empty :: Set Literal)  traverseLExpr traverseLPat)

-- Located Patterns for whatever reason don't get picked up when using `(mkQ (S.empty :: Set Literal) traverseLExpr)
-- as such we need to explicit traverse those in order to pull out any literals
mkQ2 :: (Typeable a, Typeable b, Typeable c) => r -> (b -> r) -> (c -> r) -> a -> r
mkQ2 def left right datum = case cast datum of
        Just datum' -> left datum'
        Nothing     -> maybe def right (cast datum)

traverseLPat :: GenLocated SrcSpan (Pat GhcPs) -> Set Literal
traverseLPat (L sSpan pat) = traversePat sSpan pat

traversePat :: SrcSpan -> Pat GhcPs -> Set Literal
traversePat sSpan = \case
    LitPat _ lit                             -> getLiteralAsList sSpan lit
    NPat _ (L olSpan overLit) sexpr1 sexpr2  -> getOverLiteralAsList olSpan overLit
                                            <> collectLiterals' sexpr1
                                            <> collectLiterals' sexpr2
    NPlusKPat _ _ (L olSpan loverLit) overLit sexpr1 sexpr2 -> getOverLiteralAsList olSpan loverLit
                                                    <> getOverLiteralAsList sSpan overLit
                                                    <> collectLiterals' sexpr1
                                                    <> collectLiterals' sexpr2
    ast -> collectLiterals' ast

traverseLExpr :: GenLocated SrcSpan (HsExpr GhcPs) -> Set Literal
traverseLExpr (L sSpan hsExpr) = traverseExpr sSpan hsExpr

traverseExpr :: SrcSpan -> HsExpr GhcPs -> Set Literal
traverseExpr sSpan = \case
      HsOverLit _ overLit -> getOverLiteralAsList sSpan overLit
      HsLit _ lit         -> getLiteralAsList sSpan lit
      expr                -> collectLiterals' expr

getLiteralAsList :: SrcSpan -> HsLit GhcPs -> Set Literal
getLiteralAsList sSpan lit =  case sSpan of
    RealSrcSpan rss _ -> getLiteralAsList' lit rss
    _                 -> S.empty

getLiteralAsList' :: HsLit GhcPs -> RealSrcSpan -> Set Literal
getLiteralAsList' lit = maybe S.empty S.singleton . flip getLiteral lit

-- Translate from Hs Type to our Literal type
getLiteral :: RealSrcSpan -> HsLit GhcPs -> Maybe Literal
getLiteral sSpan = \case
  HsInt _ val   -> fromIntegralLit sSpan val
  HsRat _ val _ -> fromFractionalLit sSpan val
  _             -> Nothing

getOverLiteralAsList :: SrcSpan -> HsOverLit GhcPs -> Set Literal
getOverLiteralAsList sSpan lit =  case sSpan of
    RealSrcSpan rss _ -> getOverLiteralAsList' lit rss
    _                 -> S.empty

getOverLiteralAsList' :: HsOverLit GhcPs -> RealSrcSpan -> Set Literal
getOverLiteralAsList' lit sSpan = maybe S.empty S.singleton (getOverLiteral sSpan lit)

getOverLiteral :: RealSrcSpan -> HsOverLit GhcPs -> Maybe Literal
getOverLiteral sSpan OverLit{..} = case ol_val of
  HsIntegral il   -> fromIntegralLit sSpan il
  HsFractional fl -> fromFractionalLit sSpan fl
  _               -> Nothing
getOverLiteral _ _ = Nothing

fromIntegralLit :: RealSrcSpan -> IntegralLit -> Maybe Literal
fromIntegralLit s (IL txt _ val) = fmap (\txt' -> IntLiteral s txt' val) (fromSourceText txt)

fromFractionalLit  :: RealSrcSpan -> FractionalLit -> Maybe Literal
fromFractionalLit s (FL txt _ val) = fmap (\txt' -> FracLiteral s txt' val) (fromSourceText txt)

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
