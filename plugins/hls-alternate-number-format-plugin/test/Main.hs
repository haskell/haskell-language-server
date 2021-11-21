{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE ViewPatterns   #-}
module Main ( main ) where

import           Control.Concurrent               (threadDelay)
import           Data.List                        (find)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Debug.Trace
import qualified Ide.Plugin.AlternateNumberFormat as AlternateNumberFormat
import qualified Ide.Plugin.Conversion            as Conversion
import           Properties.Conversion
import           System.FilePath                  ((</>))
import           Test.Hls
import           Text.Regex.TDFA                  ((=~))

main :: IO ()
main = defaultTestRunner test

alternateNumberFormatPlugin :: PluginDescriptor IdeState
alternateNumberFormatPlugin = AlternateNumberFormat.descriptor "alternateNumberFormat"

test :: TestTree
test = testGroup "alternateNumberFormat" [
    -- codeActionHex "TIntDtoH" 4 13
    conversions ]

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

-- most helpers derived from explicit-imports-plugin Main Test file

goldenAlternateFormat :: FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenAlternateFormat fp = goldenWithHaskellDoc alternateNumberFormatPlugin (fp <> " (golden)") testDataDir fp "expected" "hs"

codeActionTest :: (Maybe Text -> Bool) -> FilePath -> Int -> Int -> TestTree
codeActionTest filter' fp line col = goldenAlternateFormat fp $ \doc -> do
  -- _ <- waitForDiagnostics
  actions <- getCodeActions doc (pointRange line col)
  traceM $ "Code actions: " ++ show actions
  -- can't generate code actions?
  case find (filter' . codeActionTitle) actions of
    Just (InR x) -> executeCodeAction x
    _            -> liftIO $ assertFailure "Unable to find CodeAction"

codeActionHex :: FilePath -> Int -> Int -> TestTree
codeActionHex = codeActionTest isHexCodeAction

codeActionFloatHex :: FilePath -> Int -> Int -> TestTree
codeActionFloatHex = codeActionTest isHexFloatCodeAction

codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle (InR CodeAction {_title}) = Just _title
codeActionTitle _                         = Nothing

pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> line)
  (subtract 1 -> col) =
    Range (Position line col) (Position line $ col + 1)

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
isCodeAction userRegex (Just txt) = txt =~ Conversion.matchLineRegex (convertPrefix <> userRegex)
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
