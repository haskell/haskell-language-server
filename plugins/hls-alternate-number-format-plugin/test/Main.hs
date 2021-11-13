{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE ViewPatterns   #-}
module Main ( main ) where

import           Data.List                        (find)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Ide.Plugin.AlternateNumberFormat as AlternateNumberFormat
import qualified Ide.Plugin.Conversion            as Conversion
import           System.FilePath                  ((</>))
import           Test.Hls
import           Text.Regex.TDFA                  ((=~))

main :: IO ()
main = defaultTestRunner test

alternateNumberFormatPlugin :: PluginDescriptor IdeState
alternateNumberFormatPlugin = AlternateNumberFormat.descriptor "alternateNumberFormat"

test :: TestTree
test = testGroup "alternateNumberFormat" [
    codeActionHex "TIntDtoH" 3 13]

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

-- most helpers derived from explicit-imports-plugin Main Test file

goldenAlternateFormat :: FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenAlternateFormat fp = goldenWithHaskellDoc alternateNumberFormatPlugin (fp <> " (golden)") testDataDir fp "expected" "hs"

codeActionTest :: (Maybe Text -> Bool) -> FilePath -> Int -> Int -> TestTree
codeActionTest filter' fp line col = goldenAlternateFormat fp $ \doc -> do
  actions <- getAllCodeActions doc
  -- can't generate code actions?
  liftIO $ assertBool "Actions is Not Null" (not $ null actions)
--   case find (filter' . caTitle) actions of
--     Just (InR x)  -> executeCodeAction x
--     _             -> liftIO $ assertFailure "Unable to find CodeAction"

codeActionHex :: FilePath -> Int -> Int -> TestTree
codeActionHex = codeActionTest isHexCodeAction

codeActionFloatHex :: FilePath -> Int -> Int -> TestTree
codeActionFloatHex = codeActionTest isHexFloatCodeAction

caTitle :: (Command |? CodeAction) -> Maybe Text
caTitle (InR CodeAction {_title}) = Just _title
caTitle _                         = Nothing

pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> line)
  (subtract 1 -> col) =
    Range (Position line col) (Position line $ col + 2)

convertPrefix, intoInfix, hexRegex, hexFloatRegex, binaryRegex, octalRegex, numDecimalRegex, decimalRegex :: Text
convertPrefix = "Convert (" <> T.intercalate "|" [Conversion.hexRegex, Conversion.hexFloatRegex, Conversion.binaryRegex, Conversion.octalRegex, Conversion.numDecimalRegex, Conversion.decimalRegex] <> ")"
intoInfix = " into "
hexRegex = intoInfix <> Conversion.hexRegex
hexFloatRegex = intoInfix <> Conversion.hexFloatRegex
binaryRegex = intoInfix <> Conversion.binaryRegex
octalRegex = intoInfix <> Conversion.octalRegex
numDecimalRegex = intoInfix <> Conversion.numDecimalRegex
decimalRegex = intoInfix <> Conversion.decimalRegex

isCodeAction :: Text -> Maybe Text -> Bool
isCodeAction userRegex (Just txt) = txt =~ Conversion.wrap (convertPrefix <> userRegex)
isCodeAction _ _                  = False

isHexCodeAction :: Maybe Text -> Bool
isHexCodeAction = isCodeAction hexRegex

isHexFloatCodeAction :: Maybe Text -> Bool
isHexFloatCodeAction = isCodeAction hexFloatRegex

isBinaryCodeAction :: Maybe Text -> Bool
isBinaryCodeAction = isCodeAction binaryRegex

isOctalCodeAction :: Maybe Text -> Bool
isOctalCodeAction = isCodeAction octalRegex

isNumDecimalCodeAction :: Maybe Text -> Bool
isNumDecimalCodeAction = isCodeAction numDecimalRegex

isDecimalCodeAction :: Maybe Text -> Bool
isDecimalCodeAction = isCodeAction decimalRegex
