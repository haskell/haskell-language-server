{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE ViewPatterns   #-}
module Main ( main ) where

import           Data.Either                      (rights)
import           Data.List                        (find)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Debug.Trace
import qualified Ide.Plugin.AlternateNumberFormat as AlternateNumberFormat
import qualified Ide.Plugin.Conversion            as Conversion
import           Language.LSP.Types               (toEither)
import           Language.LSP.Types.Lens          (kind)
import           Properties.Conversion            (conversions)
import           System.FilePath                  ((<.>), (</>))
import           Test.Hls
import           Text.Regex.TDFA                  ((=~))

main :: IO ()
main = defaultTestRunner test

alternateNumberFormatPlugin :: PluginDescriptor IdeState
alternateNumberFormatPlugin = AlternateNumberFormat.descriptor mempty "alternateNumberFormat"


-- NOTE: For whatever reason, this plugin does not play nice with creating Code Actions on time.
-- As a result tests will mostly pass if `import Prelude` is added at the top. We (mostly fendor) surmise this has something
-- to do with how
test :: TestTree
test = testGroup "alternateNumberFormat" [
    codeActionHex "TIntDtoH" 3 13
    , codeActionOctal "TIntDtoO" 3 13
    , codeActionBinary "TIntDtoB" 4 13
    , codeActionNumDecimal "TIntDtoND" 5 13
    , codeActionFracExp "TFracDtoE" 3 13
    , codeActionFloatHex "TFracDtoHF" 4 13
    , codeActionDecimal "TIntHtoD" 3 13
    , codeActionDecimal "TFracHFtoD" 4 13
    , codeActionProperties "TFindLiteralIntPattern" [(3, 25), (4,25)] $ \actions -> do
        liftIO $ length actions @?= 4
    , codeActionProperties "TFindLiteralIntCase" [(3, 29)] $ \actions -> do
        liftIO $ length actions @?= 2
    , codeActionProperties "TFindLiteralIntCase2" [(4, 21)] $ \actions -> do
        liftIO $ length actions @?= 2
    , codeActionProperties "TFindLiteralDoReturn" [(5, 10)] $ \actions -> do
        liftIO $ length actions @?= 2
    , codeActionProperties "TFindLiteralDoLet" [(5, 13), (6, 13)] $ \actions -> do
        liftIO $ length actions @?= 4
    , codeActionProperties "TFindLiteralList" [(3, 28)] $ \actions -> do
        liftIO $ length actions @?= 2
    , codeActionProperties "TExpectNoBinaryFormat" [(3, 12)] $ \actions -> do
        liftIO $ length actions @?= 2
        liftIO $ actions `doesNotContain` binaryRegex @? "Contains binary codeAction"
    , codeActionProperties "TExpectBinaryFormat" [(4, 10)] $ \actions -> do
        liftIO $ length actions @?= 3
        liftIO $ actions `contains` binaryRegex @? "Does not contain binary codeAction"
    , codeActionProperties "TExpectNoHexFloatFormat" [(3, 14)] $ \actions -> do
        liftIO $ length actions @?= 1
        liftIO $ actions `doesNotContain` hexFloatRegex @? "Contains hex float codeAction"
    , codeActionProperties "TExpectHexFloatFormat" [(4, 12)] $ \actions -> do
        liftIO $ length actions @?= 2
        liftIO $ actions `contains` hexFloatRegex @? "Does not contain hex float codeAction"
    , codeActionProperties "TExpectNoNumDecimalFormat" [(3, 16)] $ \actions -> do
        liftIO $ length actions @?= 2
        liftIO $ actions `doesNotContain` numDecimalRegex @? "Contains numDecimal codeAction"
    , codeActionProperties "TExpectNumDecimalFormat" [(4, 14)] $ \actions -> do
        liftIO $ length actions @?= 5
        liftIO $ actions `contains` numDecimalRegex @? "Contains numDecimal codeAction"
    , conversions
    ]

codeActionProperties :: TestName -> [(Int, Int)] -> ([CodeAction] -> Session ()) -> TestTree
codeActionProperties fp locs assertions = testCase fp $ do
    runSessionWithServer alternateNumberFormatPlugin testDataDir $ do
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
            CodeActionUnknown txt -> txt == "quickfix.literals.style"
            _                     -> False

-- most helpers derived from explicit-imports-plugin Main Test file

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

goldenAlternateFormat :: FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenAlternateFormat fp = goldenWithHaskellDoc alternateNumberFormatPlugin (fp <> " (golden)") testDataDir fp "expected" "hs"

codeActionTest :: (Maybe Text -> Bool) -> FilePath -> Int -> Int -> TestTree
codeActionTest filter' fp line col = goldenAlternateFormat fp $ \doc -> do
  actions <- getCodeActions doc (pointRange line col)
  -- can't generate code actions?
  case find (filter' . codeActionTitle) actions of
    Just (InR x) -> executeCodeAction x
    _            -> liftIO $ assertFailure "Unable to find CodeAction"

codeActionDecimal :: FilePath -> Int -> Int -> TestTree
codeActionDecimal = codeActionTest isDecimalCodeAction

codeActionHex :: FilePath -> Int -> Int -> TestTree
codeActionHex = codeActionTest isHexCodeAction

codeActionOctal :: FilePath -> Int -> Int -> TestTree
codeActionOctal = codeActionTest isOctalCodeAction

codeActionBinary :: FilePath -> Int -> Int -> TestTree
codeActionBinary = codeActionTest isBinaryCodeAction

codeActionNumDecimal :: FilePath -> Int -> Int -> TestTree
codeActionNumDecimal = codeActionTest isNumDecimalCodeAction

codeActionFracExp :: FilePath -> Int -> Int -> TestTree
codeActionFracExp = codeActionTest isNumDecimalCodeAction

codeActionFloatHex :: FilePath -> Int -> Int -> TestTree
codeActionFloatHex = codeActionTest isHexFloatCodeAction

codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle (InR CodeAction {_title}) = Just _title
codeActionTitle _                         = Nothing

codeActionTitle' :: CodeAction -> Text
codeActionTitle' CodeAction{_title} = _title

pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> fromIntegral -> line)
  (subtract 1 -> fromIntegral -> col) =
    Range (Position line col) (Position line $ col + 1)

contains :: [CodeAction] -> Text -> Bool
acts `contains` regex = any (\action -> codeActionTitle' action =~ regex) acts

doesNotContain :: [CodeAction] -> Text -> Bool
acts `doesNotContain` regex = not $ acts `contains` regex

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

-- This can match EITHER an integer as NumDecimal extension or a Fractional
-- as in 1.23e-3 (so anything with an exponent really)
isNumDecimalCodeAction :: Maybe Text -> Bool
isNumDecimalCodeAction = isCodeAction numDecimalRegex

isDecimalCodeAction :: Maybe Text -> Bool
isDecimalCodeAction = isCodeAction decimalRegex
