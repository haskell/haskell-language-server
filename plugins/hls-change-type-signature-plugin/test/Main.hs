module Main where

import           Control.Monad                  (forM)
import           Data.Maybe                     (mapMaybe)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           Ide.Plugin.ChangeTypeSignature (errorMessageRegexes)
import qualified Ide.Plugin.ChangeTypeSignature as ChangeTypeSignature
import           System.FilePath                ((<.>), (</>))
import           Test.Hls                       (CodeAction (..), Command,
                                                 IdeState, PluginDescriptor,
                                                 Position (Position),
                                                 Range (Range), Session,
                                                 TestTree,
                                                 TextDocumentIdentifier,
                                                 assertBool, assertFailure,
                                                 defaultTestRunner,
                                                 executeCodeAction,
                                                 getCodeActions,
                                                 goldenWithHaskellDoc, liftIO,
                                                 testCase, testGroup,
                                                 type (|?) (InR),
                                                 waitForDiagnostics,
                                                 waitForProgressDone)
import           Test.Hls.Util                  (inspectCodeAction)
import           Text.Regex.TDFA                ((=~))

main :: IO ()
main = defaultTestRunner test

changeTypeSignaturePlugin :: PluginDescriptor IdeState
changeTypeSignaturePlugin = ChangeTypeSignature.descriptor "changeTypeSignature"

test :: TestTree
test = testGroup "changeTypeSignature" [ codeActionTest "TExpectedActualFullSignature" 4 11 , testRegexes ]

testRegexes :: TestTree
testRegexes = testGroup "Regex Testing" [
        testRegexOne
        , testRegexTwo
    ]
    where
        regex1 = errorMessageRegexes !! 0
        regex2 = errorMessageRegexes !! 1

testRegexOne :: TestTree
testRegexOne = testGroup "Regex One" [
        regexTest "error1.txt" regex True
        , regexTest "error2.txt" regex True
        , regexTest "error3.txt" regex False
        , regexTest "error4.txt" regex True
    ]
    where
        regex = errorMessageRegexes !! 0

testRegexTwo :: TestTree
testRegexTwo = testGroup "Regex Two" [
        regexTest "error1.txt" regex False
        , regexTest "error2.txt" regex False
        , regexTest "error3.txt" regex True
        , regexTest "error4.txt" regex False
    ]
    where
        regex = errorMessageRegexes !! 1

testDataDir :: FilePath
testDataDir = "test" </> "testdata"

goldenChangeSignature :: FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenChangeSignature fp = goldenWithHaskellDoc changeTypeSignaturePlugin (fp <> " (golden)") testDataDir fp "expected" "hs"

codeActionTest :: FilePath -> Int -> Int -> TestTree
codeActionTest fp line col = goldenChangeSignature fp $ \doc -> do
    waitForDiagnostics  -- code actions are triggered from Diagnostics
    actions <- getCodeActions doc (pointRange line col)
    foundAction <- liftIO $ inspectCodeAction actions ["change signature"]
    executeCodeAction foundAction


regexTest :: FilePath -> Text -> Bool -> TestTree
regexTest fp regex shouldPass = testCase fp $ do
    msg <- TIO.readFile (testDataDir </> fp)
    case (msg =~ regex  :: (Text, Text, Text, [Text]), shouldPass) of
        ((_, _, _, [_, _, _, _]), True) -> pure ()
        ((_, _, _, [_, _, _, _]), False) -> assertFailure $  "Unexpected match: " <> fp <> " with " <> T.unpack regex
        (_, True) -> assertFailure $ "Failed to match: " <> fp <> " with " <> T.unpack regex
        (_, False) -> pure ()


pointRange :: Int -> Int -> Range
pointRange
  (subtract 1 -> fromIntegral -> line)
  (subtract 1 -> fromIntegral -> col) =
    Range (Position line col) (Position line $ col + 1)

-- fn :: Int -> Int
-- fn = forM
