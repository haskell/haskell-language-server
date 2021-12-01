{-# LANGUAGE DeriveGeneric #-}
module Ide.Plugin.Conversion (
    alternateFormat
    , hexRegex
    , hexFloatRegex
    , binaryRegex
    , octalRegex
    , decimalRegex
    , numDecimalRegex
    , matchLineRegex
    , toFormatTypes
    , FormatType
    , generateNumDecimal
    , toNumDecimal
    , toBinary
    , toOctal
    , toHex
    , toHexFloat
    , toFloatDecimal
    , toFloatExpDecimal
) where

import           Data.Char                     (toUpper)
import           Data.List                     (delete, dropWhileEnd)
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
import           Text.Regex.TDFA               ((=~))

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

-- | Generate alternate formats for a single Literal based on FormatType's given.
alternateFormat :: [FormatType] -> Literal -> [Text]
alternateFormat fmts lit = case lit of
  IntLiteral _ _ val  -> concatMap (alternateIntFormat val) (removeCurrentFormat lit fmts)
  FracLiteral _ _  val -> if denominator val == 1 -- floats that can be integers we can represent as ints
      then concatMap (alternateIntFormat (numerator val)) (removeCurrentFormat lit fmts)
      else concatMap (alternateFracFormat val) (removeCurrentFormat lit fmts)

alternateIntFormat :: Integer -> FormatType -> [Text]
alternateIntFormat val fmt = case fmt of
  IntFormat ift           -> case ift of
    HexFormat        -> [T.pack $ toHex val]
    OctalFormat      -> [T.pack $ toOctal val]
    BinaryFormat     -> [T.pack $ toBinary val]
    NumDecimalFormat -> generateNumDecimal val  -- this is the only reason we return List of Text :/
  AnyFormat DecimalFormat -> [T.pack $ toDecimal val]
  _                       -> []

alternateFracFormat :: Rational -> FormatType -> [Text]
alternateFracFormat val fmt = case fmt of
  AnyFormat DecimalFormat   -> [T.pack $ toFloatDecimal (fromRational val)]
  FracFormat ExponentFormat -> [T.pack $ toFloatExpDecimal (fromRational val)]
  FracFormat HexFloatFormat -> [T.pack $ toHexFloat (fromRational val)]
  _                         -> []

removeCurrentFormat :: Literal -> [FormatType] -> [FormatType]
removeCurrentFormat lit fmts = let srcText = getSrcText lit
                                in foldl (flip delete) fmts (sourceToFormatType srcText)

-- | Regex to match a Haskell Hex Literal
hexRegex :: Text
hexRegex = "0[xX][a-fA-F0-9]+"

-- | Regex to match a Haskell Hex Float Literal
hexFloatRegex :: Text
hexFloatRegex = "0[xX][a-fA-F0-9]+(\\.)?[a-fA-F0-9]*(p[+-]?[0-9]+)?"

-- | Regex to match a Haskell Binary Literal
binaryRegex :: Text
binaryRegex = "0[bB][0|1]+"

-- | Regex to match a Haskell Octal Literal
octalRegex :: Text
octalRegex = "0[oO][0-8]+"

-- | Regex to match a Haskell Decimal Literal (no decimal points)
decimalRegex :: Text
decimalRegex = "[0-9]+(\\.[0-9]+)?"

-- | Regex to match a Haskell Literal with an explicit exponent
numDecimalRegex :: Text
numDecimalRegex = "[0-9]+\\.[0-9]+[eE][+-]?[0-9]+"

-- we want to be explicit in our matches
-- so we need to match the beginning/end of the source text
-- | Wraps a Regex with a beginning ("^") and end ("$") token
matchLineRegex :: Text -> Text
matchLineRegex regex = "^" <> regex <> "$"

sourceToFormatType :: Text -> [FormatType]
sourceToFormatType srcText
    | srcText =~ matchLineRegex hexRegex = [IntFormat HexFormat]
    | srcText =~ matchLineRegex hexFloatRegex = [FracFormat HexFloatFormat]
    | srcText =~ matchLineRegex octalRegex = [IntFormat OctalFormat]
    | srcText =~ matchLineRegex binaryRegex = [IntFormat BinaryFormat]
    -- can either be a NumDecimal or just a regular Fractional with an exponent
    -- otherwise we wouldn't need to return a list
    | srcText =~ matchLineRegex numDecimalRegex  = [IntFormat NumDecimalFormat, FracFormat ExponentFormat]
    -- just assume we are in base 10 with no decimals
    | otherwise = [AnyFormat DecimalFormat]

-- | Translate a list of Extensions into Format Types (plus a base set of Formats)
toFormatTypes :: [Extension] -> [FormatType]
toFormatTypes =  (<>) baseFormatTypes . mapMaybe (`lookup` numericPairs)
    where
        baseFormatTypes = [IntFormat HexFormat, IntFormat OctalFormat, FracFormat ExponentFormat, AnyFormat DecimalFormat]

-- current list of Numeric related extensions
-- LexicalNegation --- 9.0.1 > --- superset of NegativeLiterals
numericPairs :: [(Extension, FormatType)]
numericPairs = [(NumericUnderscores, NoFormat), (NegativeLiterals, NoFormat)] <> intPairs <> fracPairs

intPairs :: [(Extension, FormatType)]
intPairs = [(BinaryLiterals, IntFormat BinaryFormat), (NumDecimals, IntFormat NumDecimalFormat)]

fracPairs :: [(Extension, FormatType)]
fracPairs = [(HexFloatLiterals, FracFormat HexFloatFormat)]

-- Generate up to 3 possible choices where:
-- dropWhile (\d -> val `div` d) > 1000) implies we want at MOST 3 digits to left of decimal
-- takeWhile (val >) implies we want to stop once we start to get numbers like: 0.1e[N]
-- take 3 implies we want at most three choices which will center around the format:
--    - 500.123e4
--    - 50.0123e5
--    - 5e.00123e6
-- NOTE: showEFloat would also work, but results in only one option
generateNumDecimal :: Integer -> [Text]
generateNumDecimal val = map (toNumDecimal val) $ take 3 $ takeWhile (val >= ) $ dropWhile (\d -> (val `div` d) > 1000) divisors
    where
        divisors = 10 : map (*10) divisors

toNumDecimal :: Integer -> Integer -> Text
toNumDecimal val divisor = let (q, r) = val `quotRem` divisor
                               numExponent = length $ filter (== '0') $ show divisor
                               -- remove unnecessary trailing zeroes from output
                               r' = dropWhileEnd (== '0') $ show r
                               -- but make sure there are still digits left!!!
                               r'' = if null r' then "0" else r'
                               in T.pack $ show q <> "." <> r'' <> "e" <> show numExponent

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
