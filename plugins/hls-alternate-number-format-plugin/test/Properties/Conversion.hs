{-# LANGUAGE TypeApplications #-}
module Properties.Conversion where

import           Ide.Plugin.Conversion
import           Test.Hls              (TestTree, testGroup)
import           Test.Tasty.QuickCheck (testProperty)
import           Text.Regex.TDFA       ((=~))

conversions :: TestTree
conversions = testGroup "Conversions" $ map (uncurry testProperty) [("Match NumDecimal", prop_regexMatchesNumDecimal)
    , ("Match Hex", prop_regexMatchesHex)
    , ("Match Octal", prop_regexMatchesOctal)
    , ("Match Binary", prop_regexMatchesBinary)
    ] <> map (uncurry testProperty) [("Match HexFloat", prop_regexMatchesHexFloat @Double)
    , ("Match FloatDecimal", prop_regexMatchesFloatDecimal)
    , ("Match FloatExpDecimal", prop_regexMatchesFloatExpDecimal)
    ]

prop_regexMatchesNumDecimal :: Integer -> Bool
prop_regexMatchesNumDecimal = all (=~ numDecimalRegex) . generateNumDecimal

prop_regexMatchesHex :: (Integral a, Show a) => a -> Bool
prop_regexMatchesHex = (=~ hexRegex ) . toHex

prop_regexMatchesOctal :: (Integral a, Show a) => a -> Bool
prop_regexMatchesOctal = (=~ octalRegex) . toOctal

prop_regexMatchesBinary :: (Integral a, Show a) => a -> Bool
prop_regexMatchesBinary = (=~ binaryRegex) . toBinary

prop_regexMatchesHexFloat :: (RealFloat a) =>  a -> Bool
prop_regexMatchesHexFloat = (=~ hexFloatRegex) . toHexFloat

prop_regexMatchesFloatDecimal :: (RealFloat a) =>  a -> Bool
prop_regexMatchesFloatDecimal = (=~ decimalRegex ) . toFloatDecimal

prop_regexMatchesFloatExpDecimal :: (RealFloat a) =>  a -> Bool
prop_regexMatchesFloatExpDecimal = (=~ numDecimalRegex ) . toFloatExpDecimal
