{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}
module Ide.Plugin.Conversion (
    alternateFormat
    , hexRegex
    , hexFloatRegex
    , binaryRegex
    , octalRegex
    , decimalRegex
    , numDecimalRegex
    , matchLineRegex
    , toOctal
    , toDecimal
    , toBinary
    , toHex
    , toFloatDecimal
    , toFloatExpDecimal
    , toHexFloat
    , intFormats
    , fracFormats
    , AlternateFormat
    , ExtensionNeeded(..)
    , UnderscoreFormatType(..)
) where

import           Data.List                     (delete)
import           Data.List.Extra               (enumerate, upper)
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
                | NoFormat
                deriving (Show, Eq, Generic)

instance NFData FormatType

data IntFormatType = IntDecimalFormat
                   | HexFormat
                   | OctalFormat
                   | BinaryFormat
                   | NumDecimalFormat
                   deriving (Show, Eq, Generic, Bounded, Enum)

instance NFData IntFormatType

data FracFormatType = FracDecimalFormat
                    | HexFloatFormat
                    | ExponentFormat
                    deriving (Show, Eq, Generic, Bounded, Enum)

instance NFData FracFormatType

data ExtensionNeeded = NoExtension
                     | NeedsExtension Extension

type AlternateFormat = (Text, ExtensionNeeded)

-- | Generate alternate formats for a single Literal based on FormatType's given.
alternateFormat :: Literal -> [AlternateFormat]
alternateFormat lit = case lit of
  IntLiteral _ _ val   -> map (alternateIntFormat val) (removeCurrentFormatInt lit)
  FracLiteral _ _  val -> if denominator val == 1 -- floats that can be integers we can represent as ints
      then map (alternateIntFormat (numerator val)) (removeCurrentFormatInt lit)
      else map (alternateFracFormat val) (removeCurrentFormatFrac lit)

alternateIntFormat :: Integer -> IntFormatType -> AlternateFormat
alternateIntFormat val = \case
    IntDecimalFormat -> (T.pack $ toDecimal val, NoExtension)
    HexFormat        -> (T.pack $ toHex val, NoExtension)
    OctalFormat      -> (T.pack $ toOctal val, NoExtension)
    BinaryFormat     -> (T.pack $ toBinary val, NeedsExtension BinaryLiterals)
    NumDecimalFormat -> (T.pack $ toFloatExpDecimal (fromInteger @Double val), NeedsExtension NumDecimals)

alternateFracFormat :: Rational -> FracFormatType -> AlternateFormat
alternateFracFormat val = \case
  FracDecimalFormat -> (T.pack $ toFloatDecimal (fromRational @Double val), NoExtension)
  ExponentFormat    -> (T.pack $ toFloatExpDecimal (fromRational @Double val), NoExtension)
  HexFloatFormat    -> (T.pack $ toHexFloat (fromRational @Double val), NeedsExtension HexFloatLiterals)

-- given a Literal compute it's current Format and delete it from the list of available formats
removeCurrentFormat :: (Foldable t, Eq a) => [a] -> t a -> [a]
removeCurrentFormat fmts toRemove = foldl (flip delete) fmts toRemove

removeCurrentFormatInt :: Literal -> [IntFormatType]
removeCurrentFormatInt (getSrcText -> srcText) = removeCurrentFormat intFormats (filterIntFormats $ sourceToFormatType srcText)

removeCurrentFormatFrac :: Literal -> [FracFormatType]
removeCurrentFormatFrac (getSrcText -> srcText) = removeCurrentFormat fracFormats (filterFracFormats $ sourceToFormatType srcText)

filterIntFormats :: [FormatType] -> [IntFormatType]
filterIntFormats = mapMaybe getIntFormat
    where
        getIntFormat (IntFormat f) = Just f
        getIntFormat _             = Nothing

filterFracFormats :: [FormatType] -> [FracFormatType]
filterFracFormats = mapMaybe getFracFormat
    where
        getFracFormat (FracFormat f) = Just f
        getFracFormat _              = Nothing

intFormats :: [IntFormatType]
intFormats = enumerate

fracFormats :: [FracFormatType]
fracFormats = enumerate

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
    | otherwise = [IntFormat IntDecimalFormat, FracFormat FracDecimalFormat]

toBase :: (Num a, Ord a) => (a -> ShowS) -> String -> a -> String
toBase conv header n
  | n < 0 = '-' : header <> upper (conv (abs n) "")
  | otherwise = header <> upper (conv n "")

#if MIN_VERSION_base(4,17,0)
toOctal, toBinary, toHex :: Integral a => a -> String
#else
toOctal, toBinary, toHex:: (Integral a, Show a) => a -> String
#endif

toBinary = toBase showBin_ "0b"
  where
    -- this is not defined in base < 4.16
    showBin_ = showIntAtBase 2 intToDigit

toOctal = toBase showOct "0o"
data UnderscoreFormatType
    = NoUnderscores
    | UseUnderscores Int
    deriving (Show, Eq)

toHex = toBase showHex "0x"

toDecimal :: Integral a => a -> String
toDecimal = toBase showInt ""

intFormats :: Map.Map IntFormatType [UnderscoreFormatType]
intFormats = Map.fromList $ map (\t -> (t, intFormatUnderscore t)) enumerate

intFormatUnderscore :: IntFormatType -> [UnderscoreFormatType]
intFormatUnderscore formatType = NoUnderscores : map UseUnderscores (case formatType of
    IntDecimalFormat -> [3, 4]
    HexFormat        -> [2, 4]
    OctalFormat      -> [2, 4, 8]
    BinaryFormat     -> [4]
    NumDecimalFormat -> [3, 4])

toHexFloat :: RealFloat a => a -> String
toHexFloat val = showHFloat val ""
