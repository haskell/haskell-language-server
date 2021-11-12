{-# LANGUAGE DeriveGeneric #-}
module Ide.Plugin.Conversion where

import           Data.Char                     (toUpper)
import           Data.List                     (delete)
import           Data.Maybe                    (mapMaybe)
import           Data.Ratio                    (denominator, numerator)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Development.IDE.Graph.Classes (NFData)
import           GHC.Generics                  (Generic)
import           GHC.LanguageExtensions.Type   (Extension (..))
import           GHC.Show                      (intToDigit)
import           Ide.Plugin.Literals           (Literal (..), getSrcText)
import           Numeric

data FormatType = IntFormat IntFormatType
                | FracFormat FracFormatType
                | AnyFormat AnyFormatType
                | NoFormat
                deriving (Show, Eq, Generic)

instance NFData FormatType

data IntFormatType = HexFormat
                   | OctalFormat
                   | BinaryFormat
                   | NumDecimalFormat
                   deriving (Show, Eq, Generic)

instance NFData IntFormatType

data FracFormatType = HexFloatFormat
                    | ExponentFormat
                    deriving (Show, Eq, Generic)

instance NFData FracFormatType

data AnyFormatType = DecimalFormat
                    deriving (Show, Eq, Generic)

instance NFData AnyFormatType

alternateFormat :: [FormatType] -> Literal -> [Text]
alternateFormat fmts lit = case lit of
  IntLiteral _ (Just _) val  -> concatMap (alternateIntFormat val) (removeCurrentFormat lit fmts)
  FracLiteral _ (Just _) val -> if denominator val == 1 -- floats that can be integers we can represent as ints
      then concatMap (alternateIntFormat (numerator val)) (removeCurrentFormat lit fmts)
      else concatMap (alternateFracFormat val) (removeCurrentFormat lit fmts)
  _                          -> ["Uh Oh"] -- This means there is no Source Text so we just ignore it

alternateIntFormat :: Integer -> FormatType -> [Text]
alternateIntFormat val fmt = case fmt of
  IntFormat ift           -> case ift of
    HexFormat        -> [T.pack $ toHex val]
    OctalFormat      -> [T.pack $ toOctal val]
    BinaryFormat     -> [T.pack $ toBinary val]
    NumDecimalFormat -> toNumDecimal val  -- this is the only reason we return List of Text :/
  AnyFormat DecimalFormat -> [T.pack $ toDecimal val]
  _                       -> []

alternateFracFormat :: Rational -> FormatType -> [Text]
alternateFracFormat val fmt = case fmt of
  AnyFormat DecimalFormat   -> [T.pack $ toFloatDecimal (fromRational val)]
  FracFormat HexFloatFormat -> [T.pack $ toHexFloat (fromRational val)]
  FracFormat ExponentFormat -> [T.pack $ toFloatExpDecimal (fromRational val)]
  _                         -> []

-- TODO: implement logic
removeCurrentFormat :: Literal -> [FormatType] -> [FormatType]
removeCurrentFormat lit fmts = case getSrcText lit of
    Just src -> delete (sourceToFormatType src) fmts
    Nothing  -> fmts

sourceToFormatType :: Text -> FormatType
sourceToFormatType srcText
    -- is in Hex format, if so, is it a float format or regular format?
    | T.isPrefixOf "0x" srcText ||  T.isPrefixOf "0X" srcText = if T.any (== '.') srcText then FracFormat HexFloatFormat else IntFormat HexFormat
    -- Octal
    | T.isPrefixOf "0o" srcText ||  T.isPrefixOf "0O" srcText = IntFormat OctalFormat
    -- Binary
    | T.isPrefixOf "0b" srcText ||  T.isPrefixOf "0B" srcText = IntFormat BinaryFormat
    -- Note: the text package doesn't export T.elem until 1.2.5(!!)
    -- Num Decimal Format (we just assume this is an Int using NumDecimal Extension)
    -- nothing happens if a Fractional value is the source text
    | T.any (== 'e') srcText || T.any (== 'E') srcText = IntFormat NumDecimalFormat
    -- just assume we are in base 10
    | otherwise = AnyFormat DecimalFormat

-- grab the Numeric related extensions that are turned on for a file
toFormatTypes :: [Extension] -> [FormatType]
toFormatTypes =  (<>) [IntFormat HexFormat, IntFormat OctalFormat, AnyFormat DecimalFormat] . mapMaybe (`lookup` numericPairs)

-- current list of Numeric related extensions
-- LexicalNegation --- 9.0.1 > --- superset of NegativeLiterals
numericPairs :: [(Extension, FormatType)]
numericPairs = [(NumericUnderscores, NoFormat), (NegativeLiterals, NoFormat)] <> intPairs <> fracPairs

numericFormats :: [FormatType]
numericFormats = map snd numericPairs

intPairs :: [(Extension, FormatType)]
intPairs = [(BinaryLiterals, IntFormat BinaryFormat), (NumDecimals, IntFormat NumDecimalFormat)]

intFormats :: [FormatType]
intFormats = map snd intPairs

fracPairs :: [(Extension, FormatType)]
fracPairs = [(HexFloatLiterals, FracFormat HexFloatFormat)]

fracFormats :: [FormatType]
fracFormats = map snd fracPairs

-- Generate up to 3 possible choices where:
-- dropWhile (\d -> val `div` d) > 1000) implies we want at MOST 3 digits to left of decimal
-- takeWhile (val >) implies we want to stop once we start to get numbers like: 0.1e[N]
-- take 3 implies we want at most three choices which will center around the format:
--    - 500.123e4
--    - 50.0123e5
--    - 5e.00123e6
-- NOTE: showEFloat would also work, but results in only one option
toNumDecimal :: Integer -> [Text]
toNumDecimal val = map (showNumDecimal val) $ take 3 $ takeWhile (val >= ) $ dropWhile (\d -> (val `div` d) > 1000) divisors
    where
        divisors = 10 : map (*10) divisors

showNumDecimal :: Integer -> Integer -> Text
showNumDecimal val divisor = let (q, r) = val `quotRem` divisor
                                 numExponent = length $ filter (== '0') $ show divisor
                                 in T.pack $ show q <> "." <> show r <> "e" <> show numExponent

toBase :: (Num a, Ord a) => (a -> ShowS) -> String -> a -> String
toBase conv header n
  | n < 0 = '-' : header <> map toUpper (conv (abs n) "")
  | otherwise = header <> map toUpper (conv n "")

toOctal :: (Integral a, Show a) => a -> String
toOctal = toBase showOct "0o"

toDecimal :: Integral a => a -> String
toDecimal = toBase showInt ""

toBinary :: (Integral a, Show a) => a -> String
toBinary = toBase showBin "0b"
  where
    -- this is not defined in versions of Base < 4.16-ish
    showBin = showIntAtBase 2 intToDigit

toHex :: (Integral a, Show a) => a  -> String
toHex = toBase showHex "0x"

toFloatDecimal :: RealFloat a => a -> String
toFloatDecimal val = showFFloat Nothing val ""

toFloatExpDecimal :: RealFloat a => a -> String
toFloatExpDecimal val = showEFloat Nothing val ""

toHexFloat :: RealFloat a => a -> String
toHexFloat val = showHFloat val ""
