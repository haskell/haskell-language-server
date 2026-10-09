{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}
module Ide.Plugin.Literals (
    collectLiterals
    , Literal(..)
    , getSrcText
    , getSrcSpan
) where

import           Data.Maybe                    (maybeToList)
import           Data.Text                     (Text)
#if __GLASGOW_HASKELL__ >= 908
import qualified Data.Text.Encoding            as T
#else
import qualified Data.Text                     as T
#endif
import           Development.IDE.GHC.Compat    hiding (getSrcSpan)
import           Development.IDE.Graph.Classes (NFData (rnf))
import           Generics.SYB                  (Data, everything, extQ)
import qualified GHC.Generics                  as GHC

-- data type to capture what type of literal we are dealing with
-- provides location and possibly source text (for OverLits) as well as it's value
-- we currently don't have any use for PrimLiterals. They never have source text so we always drop them
-- | Captures a Numeric Literals Location, Source Text, and Value.
data Literal = IntLiteral  LiteralSrcSpan Text Integer
             | FracLiteral LiteralSrcSpan Text Rational
             deriving (GHC.Generic, Show, Ord, Eq, Data)

newtype LiteralSrcSpan = LiteralSrcSpan { unLit :: RealSrcSpan }
                        deriving (GHC.Generic, Show, Ord, Eq, Data)

instance NFData LiteralSrcSpan where
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
    IntLiteral ss _ _  -> unLit ss
    FracLiteral ss _ _ -> unLit ss

-- | Find all literals in a Parsed Source File
collectLiterals :: Data ast => ast -> [Literal]
collectLiterals = everything (<>) (maybeToList . (const Nothing `extQ` getLiteral `extQ` getPattern))


-- | Translate from HsLit and HsOverLit Types to our Literal Type
getLiteral :: LHsExpr GhcPs -> Maybe Literal
getLiteral (L (locA -> (RealSrcSpan sSpan _)) expr) = case expr of
    HsLit _ lit         -> fromLit lit sSpan
    HsOverLit _ overLit -> fromOverLit overLit sSpan
    _                   -> Nothing
getLiteral _ = Nothing

-- | Destructure Patterns to unwrap any Literals
getPattern :: LPat GhcPs -> Maybe Literal
getPattern (L (locA -> (RealSrcSpan patSpan _)) pat) = case pat of
    LitPat _ lit -> case lit of
        HsInt _ val   -> fromIntegralLit patSpan val
#if __GLASGOW_HASKELL__ < 913
        HsRat _ val _ -> fromFractionalLit patSpan val
#endif
        _             -> Nothing
    NPat _ (L (locA -> (RealSrcSpan sSpan _)) overLit) _ _ -> fromOverLit overLit sSpan
    NPlusKPat _ _ (L (locA -> (RealSrcSpan sSpan _)) overLit1) _ _ _ -> fromOverLit overLit1 sSpan
    _ -> Nothing
getPattern _ = Nothing

fromLit :: HsLit GhcPs -> RealSrcSpan -> Maybe Literal
fromLit lit sSpan = case lit of
        HsInt _ val   -> fromIntegralLit sSpan val
#if __GLASGOW_HASKELL__ < 913
        HsRat _ val _ -> fromFractionalLit sSpan val
#endif
        _             -> Nothing

fromOverLit :: HsOverLit p -> RealSrcSpan -> Maybe Literal
fromOverLit OverLit{..} sSpan = case ol_val of
        HsIntegral il   -> fromIntegralLit sSpan il
        HsFractional fl -> fromFractionalLit sSpan fl
        _               -> Nothing
fromOverLit _ _ = Nothing

fromIntegralLit :: RealSrcSpan -> IntegralLit -> Maybe Literal
fromIntegralLit s IL{..} = fmap (\txt' -> IntLiteral (LiteralSrcSpan s) txt' il_value) (fromSourceText il_text)

fromFractionalLit  :: RealSrcSpan -> FractionalLit -> Maybe Literal
fromFractionalLit s fl@FL{fl_text} = fmap (\txt' -> FracLiteral (LiteralSrcSpan s) txt' (rationalFromFractionalLit fl)) (fromSourceText fl_text)

fromSourceText :: SourceText -> Maybe Text
fromSourceText = \case
#if __GLASGOW_HASKELL__ >= 908
  SourceText s -> Just $ T.decodeUtf8 $ bytesFS s
#else
  SourceText s -> Just $ T.pack s
#endif
  NoSourceText -> Nothing
