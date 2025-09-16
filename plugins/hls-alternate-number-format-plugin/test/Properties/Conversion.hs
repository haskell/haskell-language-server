module Properties.Conversion where

import qualified Data.Map              as Map
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import           Ide.Plugin.Conversion
import           Test.Hls              (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Regex.TDFA       ((=~))

conversions :: TestTree
conversions = testGroup "Conversions"
    [ testGroup "integral literals"
        [ testGroup "Match NumDecimal" prop_regexMatchesNumDecimal
        , testGroup "Match Hex" prop_regexMatchesHex
        , testGroup "Match Octal" prop_regexMatchesOctal
        , testGroup "Match Binary" prop_regexMatchesBinary
        ]
    , testGroup "fractional literals"
        [ testGroup "Match HexFloat" prop_regexMatchesHexFloat
        , testGroup "Match FloatDecimal" prop_regexMatchesFloatDecimal
        , testGroup "Match FloatExpDecimal" prop_regexMatchesFloatExpDecimal
        ]
    ]

allIntFormatOf :: IntFormatType -> [UnderscoreFormatType]
allIntFormatOf formatType = fromMaybe [] (Map.lookup formatType intFormats)

prop_regexMatchesNumDecimal :: [TestTree]
prop_regexMatchesNumDecimal =
    [ testProperty (show underscoreFormat) (prop underscoreFormat)
    | underscoreFormat <- allIntFormatOf IntDecimalFormat ]
  where
    prop :: UnderscoreFormatType -> Integer -> Bool
    prop underscoreFormat = (=~ numDecimalRegex) . toFloatExpDecimal @Double underscoreFormat . fromInteger

prop_regexMatchesHex :: [TestTree]
prop_regexMatchesHex =
    [ testProperty (show underscoreFormat) (prop underscoreFormat)
    | underscoreFormat <- allIntFormatOf IntDecimalFormat ]
  where
    prop :: UnderscoreFormatType -> Integer -> Bool
    prop underscoreFormat = (=~ hexRegex ) . toHex underscoreFormat

prop_regexMatchesOctal :: [TestTree]
prop_regexMatchesOctal =
    [ testProperty (show underscoreFormat) (prop underscoreFormat)
    | underscoreFormat <- allIntFormatOf IntDecimalFormat ]
  where
    prop :: UnderscoreFormatType -> Integer -> Bool
    prop underscoreFormat = (=~ octalRegex) . toOctal underscoreFormat

prop_regexMatchesBinary :: [TestTree]
prop_regexMatchesBinary =
    [ testProperty (show underscoreFormat) (prop underscoreFormat)
    | underscoreFormat <- allIntFormatOf IntDecimalFormat ]
  where
    prop :: UnderscoreFormatType -> Integer -> Bool
    prop underscoreFormat = (=~ binaryRegex) . toBinary underscoreFormat

allFracFormatOf :: FracFormatType -> [UnderscoreFormatType]
allFracFormatOf formatType = fromMaybe [] (Map.lookup formatType fracFormats)

prop_regexMatchesHexFloat :: [TestTree]
prop_regexMatchesHexFloat =
    [ testProperty (show underscoreFormat) (prop underscoreFormat)
    | underscoreFormat <- allFracFormatOf HexFloatFormat ]
  where
    prop :: UnderscoreFormatType -> Double -> Bool
    prop underscoreFormat = (=~ hexFloatRegex) . toHexFloat underscoreFormat

prop_regexMatchesFloatDecimal :: [TestTree]
prop_regexMatchesFloatDecimal =
    [ testProperty (show underscoreFormat) (prop underscoreFormat)
    | underscoreFormat <- allFracFormatOf FracDecimalFormat ]
  where
    prop :: UnderscoreFormatType -> Double -> Bool
    prop underscoreFormat = (=~ decimalRegex ) . toFloatDecimal underscoreFormat

prop_regexMatchesFloatExpDecimal :: [TestTree]
prop_regexMatchesFloatExpDecimal =
    [ testProperty (show underscoreFormat) (prop underscoreFormat)
    | underscoreFormat <- allFracFormatOf ExponentFormat ]
  where
    prop :: UnderscoreFormatType -> Double -> Bool
    prop underscoreFormat = (=~ numDecimalRegex ) . toFloatExpDecimal underscoreFormat

-- | Regex to match a Haskell Hex Literal
hexRegex :: Text
hexRegex = "0[xX][a-fA-F0-9_]+"

-- | Regex to match a Haskell Hex Float Literal
hexFloatRegex :: Text
hexFloatRegex = "0[xX][a-fA-F0-9_]+(\\.)?[a-fA-F0-9_]*(p[+-]?[0-9]+)?"

-- | Regex to match a Haskell Binary Literal
binaryRegex :: Text
binaryRegex = "0[bB][0|1_]+"

-- | Regex to match a Haskell Octal Literal
octalRegex :: Text
octalRegex = "0[oO][0-8_]+"

-- | Regex to match a Haskell Decimal Literal (no decimal points)
decimalRegex :: Text
decimalRegex = "[0-9_]+(\\.[0-9_]+)?"

-- | Regex to match a Haskell Literal with an explicit exponent
numDecimalRegex :: Text
numDecimalRegex = "[0-9_]+\\.[0-9_]+[eE][+-]?[0-9]+"

-- we want to be explicit in our matches
-- so we need to match the beginning/end of the source text
-- | Wraps a Regex with a beginning ("^") and end ("$") token
matchLineRegex :: Text -> Text
matchLineRegex regex = "^" <> regex <> "$"

