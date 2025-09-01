{-# LANGUAGE CPP                #-}
{-# LANGUAGE DerivingStrategies #-}
module Ide.Plugin.Conversion (
    alternateFormat
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
    , FormatType(..)
    , IntFormatType(..)
    , FracFormatType(..)
    , UnderscoreFormatType(..)
) where

import           Data.List                     (intercalate)
import           Data.List.Extra               (chunksOf, enumerate, nubOrdOn,
                                                upper)
import qualified Data.Map                      as Map
import           Data.Ratio                    (denominator, numerator)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Development.IDE.Graph.Classes (NFData)
import           GHC.Generics                  (Generic)
import           GHC.LanguageExtensions.Type   (Extension (..))
import           Ide.Plugin.Literals           (Literal (..), getSrcText)
import           Numeric

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
                   deriving (Show, Eq, Generic, Ord, Bounded, Enum)

instance NFData IntFormatType

data FracFormatType = FracDecimalFormat
                    | HexFloatFormat
                    | ExponentFormat
                    deriving (Show, Eq, Generic, Ord, Bounded, Enum)

instance NFData FracFormatType

newtype ExtensionNeeded = ExtensionNeeded [Extension]
    deriving newtype (Semigroup, Monoid)

type AlternateFormat = (Text, ExtensionNeeded)

-- | Generate alternate formats for a single Literal based on FormatType's given.
alternateFormat :: Literal -> [AlternateFormat]
alternateFormat lit = nubOrdOn fst $ removeIdentical $ case lit of
  IntLiteral _ _ val   -> alternateIntFormatsOf id val
  FracLiteral _ _  val -> if denominator val == 1 -- floats that can be integers we can represent as ints
    then alternateIntFormatsOf numerator val
    else alternateFracFormatsOf val
  where
    removeIdentical = filter ((/= getSrcText lit) . fst)
    alternateIntFormatsOf with val = [ alternateIntFormat (with val) formatType f | (formatType, formats) <- Map.toList intFormats, f <- formats]
    alternateFracFormatsOf val = [ alternateFracFormat val formatType f | (formatType, formats) <- Map.toList fracFormats, f <- formats]

data UnderscoreFormatType
    = NoUnderscores
    | UseUnderscores Int
    deriving (Show, Eq)

underscoreExtensions :: UnderscoreFormatType -> ExtensionNeeded
underscoreExtensions = \case
    NoUnderscores -> mempty
    UseUnderscores _ -> ExtensionNeeded [NumericUnderscores]

alternateIntFormat :: Integer -> IntFormatType -> UnderscoreFormatType -> AlternateFormat
alternateIntFormat val formatType underscoreFormat = case formatType of
    IntDecimalFormat -> (T.pack $ toDecimal underscoreFormat val, underscoreExtensions underscoreFormat)
    HexFormat        -> (T.pack $ toHex underscoreFormat val, underscoreExtensions underscoreFormat)
    OctalFormat      -> (T.pack $ toOctal underscoreFormat val, underscoreExtensions underscoreFormat)
    BinaryFormat     -> (T.pack $ toBinary underscoreFormat val, underscoreExtensions underscoreFormat <> ExtensionNeeded [BinaryLiterals])
    NumDecimalFormat -> (T.pack $ toFloatExpDecimal underscoreFormat (fromInteger @Double val), underscoreExtensions underscoreFormat <> ExtensionNeeded [NumDecimals])

alternateFracFormat :: Rational -> FracFormatType -> UnderscoreFormatType -> AlternateFormat
alternateFracFormat val formatType underscoreFormat = case formatType of
  FracDecimalFormat -> (T.pack $ toFloatDecimal underscoreFormat (fromRational @Double val), mempty)
  ExponentFormat    -> (T.pack $ toFloatExpDecimal underscoreFormat (fromRational @Double val), mempty)
  HexFloatFormat    -> (T.pack $ toHexFloat underscoreFormat (fromRational @Double val), underscoreExtensions underscoreFormat <> ExtensionNeeded [HexFloatLiterals])

intFormats :: Map.Map IntFormatType [UnderscoreFormatType]
intFormats = Map.fromList $ map (\t -> (t, intFormatUnderscore t)) enumerate

intFormatUnderscore :: IntFormatType -> [UnderscoreFormatType]
intFormatUnderscore formatType = NoUnderscores : map UseUnderscores (case formatType of
    IntDecimalFormat -> [3, 4]
    HexFormat        -> [2, 4]
    OctalFormat      -> [2, 4, 8]
    BinaryFormat     -> [4]
    NumDecimalFormat -> [3, 4])

fracFormats :: Map.Map FracFormatType [UnderscoreFormatType]
fracFormats = Map.fromList $ map (\t -> (t, fracFormatUnderscore t)) enumerate

fracFormatUnderscore :: FracFormatType -> [UnderscoreFormatType]
fracFormatUnderscore formatType = NoUnderscores : map UseUnderscores (case formatType of
  FracDecimalFormat -> [3, 4]
  ExponentFormat    -> [3, 4]
  HexFloatFormat    -> [2, 4])

addMinus :: (Ord n, Num n) => (n -> String) -> n -> String
addMinus f n
  | n < 0 = '-' : f (abs n)
  | otherwise = f n

toBase :: (a -> ShowS) -> a -> String
toBase conv n = upper (conv n "")

toBaseFmt :: (Ord a, Num a) => (a -> ShowS) -> [Char] -> UnderscoreFormatType ->  a -> [Char]
toBaseFmt conv header underscoreFormat = addMinus $ \val ->
    header ++ addUnderscoresInt underscoreFormat (toBase conv val)

toBinary  :: Integral a => UnderscoreFormatType -> a -> String
toBinary = toBaseFmt showBin "0b"

toOctal  :: Integral a => UnderscoreFormatType -> a -> String
toOctal = toBaseFmt showOct "0o"

toHex :: Integral a => UnderscoreFormatType -> a -> String
toHex = toBaseFmt showHex "0x"

toDecimal :: Integral a => UnderscoreFormatType -> a -> String
toDecimal = toBaseFmt showInt ""

addUnderscoresInt :: UnderscoreFormatType -> String -> String
addUnderscoresInt = \case
    NoUnderscores -> id
    -- Chunk starting from the least significant numeral.
    UseUnderscores n -> reverse . intercalate "_" . chunksOf n . reverse

toFracFormat :: (Ord t, Num t) => (t -> String) -> String -> UnderscoreFormatType -> t -> String
toFracFormat f header underScoreFormat = addMinus $ \val ->
    header <> addUnderscoresFloat underScoreFormat (f val)

toFloatDecimal :: RealFloat a => UnderscoreFormatType -> a -> String
toFloatDecimal = toFracFormat (\v -> showFFloat Nothing (abs v) "") ""

toFloatExpDecimal :: RealFloat a => UnderscoreFormatType -> a -> String
toFloatExpDecimal underscoreFormat val =
    let (n, e) = break (=='e') $ showEFloat Nothing (abs val) ""
    in toFracFormat (const n) "" underscoreFormat val <> e

toHexFloat :: RealFloat a => UnderscoreFormatType -> a -> String
toHexFloat underscoreFormat val =
    let (header, n) = splitAt 2 $ showHFloat (abs val) ""
        (n', e) = break (=='p') n
    in toFracFormat (const n') header underscoreFormat val <> e

addUnderscoresFloat :: UnderscoreFormatType -> String -> String
addUnderscoresFloat = \case
    NoUnderscores -> id
    UseUnderscores n -> \s ->
        let (integral, decimal) = break (=='.') s
            addUnderscores = reverse . intercalate "_" . chunksOf n . reverse
        in intercalate "." [addUnderscores integral, intercalate "_" $ chunksOf n $ drop 1 decimal]
