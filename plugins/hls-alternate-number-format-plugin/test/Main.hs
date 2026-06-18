{-# LANGUAGE ViewPatterns #-}
module Main ( main ) where

import           Data.Either                      (rights)
import           Data.List                        (find)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Ide.Plugin.AlternateNumberFormat as AlternateNumberFormat
import qualified Properties.Conversion            as Conversion
import           System.FilePath                  ((<.>), (</>))
import           Test.Hls
import           Text.Regex.TDFA                  ((=~))

main :: IO ()
main = defaultTestRunner test

alternateNumberFormatPlugin :: PluginTestDescriptor AlternateNumberFormat.Log
alternateNumberFormatPlugin = mkPluginTestDescriptor AlternateNumberFormat.descriptor "alternateNumberFormat"

-- NOTE: For whatever reason, this plugin does not play nice with creating Code Actions on time.
-- As a result tests will mostly pass if `import Prelude` is added at the top. We (mostly fendor) surmise this has something
-- to do with how
test :: TestTree
test = testGroup "alternateNumberFormat" [
    codeActionHex 0 "TIntDtoH" 3 13
    , codeActionOctal 0 "TIntDtoO" 3 13
    , codeActionBinary 0 "TIntDtoB" 4 13
    , codeActionBinary 6 "TIntDtoBU0toU4MultiplePragma" 4 13
    , codeActionNumDecimal 0 "TIntDtoND" 5 13
    , codeActionDecimal 2 "TIntDtoDU0toU3" 4 13
    , codeActionFracExp 0 "TFracDtoE" 3 13
    , codeActionFracExp 3 "TFracDtoEU0toU3" 3 13
    , codeActionFloatHex 0 "TFracDtoHF" 4 13
    , codeActionFloatHex 6 "TFracDtoHFU0toU2" 4 13
    , codeActionDecimal 0 "TIntHtoD" 3 13
    , codeActionDecimal 0 "TFracHFtoD" 4 13
    , codeActionDecimal 3 "TFracDtoDU0toU3" 3 13
    , codeActionDecimal 2 "TFracDtoDU3toU4" 3 13
    , codeActionDecimal 3 "TFracDtoDU3toU0" 3 13
    -- to test we don't duplicate pragmas
    , codeActionFloatHex 0 "TFracDtoHFWithPragma" 4 13
    , codeActionProperties "TFindLiteralIntPattern" [(4, 25), (5,25)] $ \actions -> do
        liftIO $ length actions @?= 8
    , codeActionProperties "TFindLiteralIntCase" [(4, 29)] $ \actions -> do
        liftIO $ length actions @?= 5
    , codeActionProperties "TFindLiteralIntCase2" [(5, 21)] $ \actions -> do
        liftIO $ length actions @?= 5
    , codeActionProperties "TFindLiteralDoReturn" [(6, 10)] $ \actions -> do
        liftIO $ length actions @?= 5
    , codeActionProperties "TFindLiteralDoLet" [(6, 13), (7, 13)] $ \actions -> do
        liftIO $ length actions @?= 12
    , codeActionProperties "TFindLiteralList" [(4, 28)] $ \actions -> do
        liftIO $ length actions @?= 5
    , Conversion.conversions
    ]

codeActionProperties :: TestName -> [(Int, Int)] -> ([CodeAction] -> Session ()) -> TestTree
codeActionProperties fp locs assertions = testCase fp $ do
    runSessionWithServer def alternateNumberFormatPlugin testDataDir $ do
        openDoc (fp <.> ".hs") "haskell" >>= codeActionsFromLocs >>= findAlternateNumberActions >>= assertions
    where
        -- similar to codeActionTest
        codeActionsFromLocs doc = concat <$> mapM (getCodeActions doc . uncurry pointRange) locs

findAlternateNumberActions :: [Command |? CodeAction] -> Session [CodeAction]
findAlternateNumberActions = pure . filter isAlternateNumberCodeAction . rights . map toEither
    where
        isAlternateNumberCodeAction CodeAction{_kind} = case _kind of
          Nothing -> False
          Just kind -> case kind of
            CodeActionKind_Custom txt -> txt == "quickfix.literals.style"
            _                         -> False

-- most helpers derived from explicit-imports-plugin Main Test file

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-alternate-number-format-plugin" </> "test" </> "testdata"

goldenAlternateFormat :: FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenAlternateFormat fp = goldenWithHaskellDoc def alternateNumberFormatPlugin (fp <> " (golden)") testDataDir fp "expected" "hs"

codeActionTest :: (Maybe Text -> Bool) -> FilePath -> Int -> Int -> TestTree
codeActionTest filter' fp line col = goldenAlternateFormat fp $ \doc -> do
  actions <- getCodeActions doc (pointRange line col)
  -- can't generate code actions?
  case find (filter' . codeActionTitle) actions of
    Just (InR x) -> executeCodeAction x
    _            -> liftIO $ assertFailure "Unable to find CodeAction"

codeActionDecimal :: Int -> FilePath -> Int -> Int -> TestTree
codeActionDecimal nrUnderscores = codeActionTest (isDecimalCodeAction nrUnderscores)

codeActionHex :: Int -> FilePath -> Int -> Int -> TestTree
codeActionHex nrUnderscores = codeActionTest (isHexCodeAction nrUnderscores)

codeActionOctal :: Int -> FilePath -> Int -> Int -> TestTree
codeActionOctal nrUnderscores = codeActionTest (isOctalCodeAction nrUnderscores)

codeActionBinary :: Int -> FilePath -> Int -> Int -> TestTree
codeActionBinary nrUnderscores = codeActionTest (isBinaryCodeAction nrUnderscores)

codeActionNumDecimal :: Int -> FilePath -> Int -> Int -> TestTree
codeActionNumDecimal nrUnderscores = codeActionTest (isNumDecimalCodeAction nrUnderscores)

codeActionFracExp :: Int -> FilePath -> Int -> Int -> TestTree
codeActionFracExp nrUnderscores = codeActionTest (isNumDecimalCodeAction nrUnderscores)

codeActionFloatHex :: Int -> FilePath -> Int -> Int -> TestTree
codeActionFloatHex nrUnderscores = codeActionTest (isHexFloatCodeAction nrUnderscores)

codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle (InR CodeAction {_title}) = Just _title
codeActionTitle _                         = Nothing

pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> fromIntegral -> line)
  (subtract 1 -> fromIntegral -> col) =
    Range (Position line col) (Position line $ col + 1)

convertPrefix, intoInfix, maybeExtension, hexRegex, hexFloatRegex, binaryRegex, octalRegex, numDecimalRegex, decimalRegex :: Text
convertPrefix = "Convert (" <> T.intercalate "|" [Conversion.hexRegex, Conversion.hexFloatRegex, Conversion.binaryRegex, Conversion.octalRegex, Conversion.numDecimalRegex, Conversion.decimalRegex] <> ")"
intoInfix = " into "
maybeExtension = "( \\(needs extensions: .*)?"
hexRegex = intoInfix <> Conversion.hexRegex <> maybeExtension
hexFloatRegex = intoInfix <> Conversion.hexFloatRegex <> maybeExtension
binaryRegex = intoInfix <> Conversion.binaryRegex <> maybeExtension
octalRegex = intoInfix <> Conversion.octalRegex <> maybeExtension
numDecimalRegex = intoInfix <> Conversion.numDecimalRegex <> maybeExtension
decimalRegex = intoInfix <> Conversion.decimalRegex <> maybeExtension

isCodeAction :: Text -> Int -> Maybe Text -> Bool
isCodeAction userRegex nrUnderscores (Just txt)
    | matchesUnderscores txt nrUnderscores
    = txt =~ Conversion.matchLineRegex (convertPrefix <> userRegex)
isCodeAction _ _ _ = False

matchesUnderscores :: Text -> Int -> Bool
matchesUnderscores txt nrUnderscores = T.count "_" txt == nrUnderscores

isHexCodeAction :: Int -> Maybe Text -> Bool
isHexCodeAction = isCodeAction hexRegex

isHexFloatCodeAction :: Int -> Maybe Text -> Bool
isHexFloatCodeAction = isCodeAction hexFloatRegex

isBinaryCodeAction :: Int -> Maybe Text -> Bool
isBinaryCodeAction = isCodeAction binaryRegex

isOctalCodeAction :: Int -> Maybe Text -> Bool
isOctalCodeAction = isCodeAction octalRegex

-- This can match EITHER an integer as NumDecimal extension or a Fractional
-- as in 1.23e-3 (so anything with an exponent really)
isNumDecimalCodeAction :: Int -> Maybe Text -> Bool
isNumDecimalCodeAction = isCodeAction numDecimalRegex

isDecimalCodeAction :: Int -> Maybe Text -> Bool
isDecimalCodeAction = isCodeAction decimalRegex
