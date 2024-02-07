module Properties.Conversion where

import           Ide.Plugin.Conversion
import           Test.Hls              (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Regex.TDFA       ((=~))

conversions :: TestTree
conversions = testGroup "Conversions" $
    map (uncurry testProperty)
    [ ("Match NumDecimal", prop_regexMatchesNumDecimal)
    , ("Match Hex", prop_regexMatchesHex)
    , ("Match Octal", prop_regexMatchesOctal)
    , ("Match Binary", prop_regexMatchesBinary)
    ]
    <>
    map (uncurry testProperty)
    [ ("Match HexFloat", prop_regexMatchesHexFloat)
    , ("Match FloatDecimal", prop_regexMatchesFloatDecimal)
    , ("Match FloatExpDecimal", prop_regexMatchesFloatExpDecimal)
    ]

prop_regexMatchesNumDecimal :: Integer -> Bool
prop_regexMatchesNumDecimal = (=~ numDecimalRegex) . toFloatExpDecimal @Double . fromInteger

prop_regexMatchesHex :: Integer -> Bool
prop_regexMatchesHex = (=~ hexRegex ) . toHex

prop_regexMatchesOctal :: Integer -> Bool
prop_regexMatchesOctal = (=~ octalRegex) . toOctal

prop_regexMatchesBinary :: Integer -> Bool
prop_regexMatchesBinary = (=~ binaryRegex) . toBinary

prop_regexMatchesHexFloat :: Double -> Bool
prop_regexMatchesHexFloat = (=~ hexFloatRegex) . toHexFloat

prop_regexMatchesFloatDecimal :: Double -> Bool
prop_regexMatchesFloatDecimal = (=~ decimalRegex ) . toFloatDecimal

prop_regexMatchesFloatExpDecimal :: Double -> Bool
prop_regexMatchesFloatExpDecimal = (=~ numDecimalRegex ) . toFloatExpDecimal
