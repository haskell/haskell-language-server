{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE RankNTypes         #-}
module Ide.Plugin.Literals (
    collectLiterals
    , Literal(..)
    , getSrcText
    , getSrcSpan
) where

import           Data.Maybe                    (maybeToList)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Development.IDE.GHC.Compat    hiding (getSrcSpan)
import           Development.IDE.GHC.Util      (unsafePrintSDoc)
import           Development.IDE.Graph.Classes (NFData (rnf))
import qualified GHC.Generics                  as GHC
import           Generics.SYB                  (Data, Typeable, everything,
                                                extQ)

-- data type to capture what type of literal we are dealing with
-- provides location and possibly source text (for OverLits) as well as it's value
-- we currently don't have any use for PrimLiterals. They never have source text so we always drop them
-- | Captures a Numeric Literals Location, Source Text, and Value.
data Literal = IntLiteral  RealSrcSpan Text Integer
             | FracLiteral RealSrcSpan Text Rational
             deriving (GHC.Generic, Show, Ord, Eq, Data)

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
collectLiterals = everything (<>) (maybeToList . (const Nothing `extQ` getLiteral `extQ` getPattern))

-- | Translate from HsLit and HsOverLit Types to our Literal Type
getLiteral :: GenLocated SrcSpan (HsExpr GhcPs) -> Maybe Literal
getLiteral (L (UnhelpfulSpan _) _) = Nothing
getLiteral (L (RealSrcSpan sSpan _ ) expr) = case expr of
    HsLit _ lit         -> fromLit lit sSpan
    HsOverLit _ overLit -> fromOverLit overLit sSpan
    _                   -> Nothing

-- | Destructure Patterns to unwrap any Literals
getPattern :: GenLocated SrcSpan (Pat GhcPs) -> Maybe Literal
getPattern (L (UnhelpfulSpan _) _)       = Nothing
getPattern (L (RealSrcSpan patSpan _) pat) = case pat of
    LitPat _ lit -> case lit of
        HsInt _ val   -> fromIntegralLit patSpan val
        HsRat _ val _ -> fromFractionalLit patSpan val
        _             -> Nothing
    NPat _ (L (RealSrcSpan sSpan _) overLit) _ _ -> fromOverLit overLit sSpan
    NPlusKPat _ _ (L (RealSrcSpan sSpan _) overLit1) _ _ _ -> fromOverLit overLit1 sSpan
    _ -> Nothing

fromLit :: HsLit p -> RealSrcSpan -> Maybe Literal
fromLit lit sSpan = case lit of
        HsInt _ val   -> fromIntegralLit sSpan val
        HsRat _ val _ -> fromFractionalLit sSpan val
        _             -> Nothing

fromOverLit :: HsOverLit p -> RealSrcSpan -> Maybe Literal
fromOverLit OverLit{..} sSpan = case ol_val of
        HsIntegral il   -> fromIntegralLit sSpan il
        HsFractional fl -> fromFractionalLit sSpan fl
        _               -> Nothing
fromOverLit _ _ = Nothing

fromIntegralLit :: RealSrcSpan -> IntegralLit -> Maybe Literal
fromIntegralLit s IL{..} = fmap (\txt' -> IntLiteral s txt' il_value) (fromSourceText il_text)

fromFractionalLit  :: RealSrcSpan -> FractionalLit -> Maybe Literal
fromFractionalLit s fl@FL{fl_text} = fmap (\txt' -> FracLiteral s txt' (rationalFromFractionalLit fl)) (fromSourceText fl_text)

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
     HsIntegral int -> case int of { IL{il_value} -> "IntegralOverLit: " <> show il_value}
     HsFractional frac -> case frac of { fl -> "RationalOverLit: " <> show (rationalFromFractionalLit fl)}
     HsIsString _ str -> "HIsString: " <> show str
