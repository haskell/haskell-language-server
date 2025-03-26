module Main where

import           Control.Monad                  (void)
import           Data.Either                    (rights)
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           Ide.Plugin.ChangeTypeSignature (errorMessageRegexes)
import qualified Ide.Plugin.ChangeTypeSignature as ChangeTypeSignature
import           System.FilePath                ((<.>), (</>))
import           Test.Hls                       (CodeAction (..), Command,
                                                 GhcVersion (..),
                                                 PluginTestDescriptor,
                                                 Position (Position),
                                                 Range (Range), Session,
                                                 TestName, TestTree,
                                                 TextDocumentIdentifier,
                                                 assertFailure, def,
                                                 defaultTestRunner,
                                                 executeCodeAction,
                                                 getCodeActions,
                                                 goldenWithHaskellDoc,
                                                 knownBrokenForGhcVersions,
                                                 liftIO,
                                                 mkPluginTestDescriptor',
                                                 openDoc, runSessionWithServer,
                                                 testCase, testGroup, toEither,
                                                 type (|?), waitForBuildQueue,
                                                 waitForDiagnostics, (@?=))
import           Text.Regex.TDFA                ((=~))

main :: IO ()
main = defaultTestRunner test

changeTypeSignaturePlugin :: PluginTestDescriptor ()
changeTypeSignaturePlugin = mkPluginTestDescriptor' ChangeTypeSignature.descriptor "changeTypeSignature"

test :: TestTree
test = testGroup "changeTypeSignature" [
        testRegexes
        , codeActionTest "TExpectedActual" 4 11
        , knownBrokenForGhcVersions [GHC94 .. GHC912] "Error Message in 9.2+ does not provide enough info" $
            codeActionTest "TRigidType" 4 14
        , codeActionTest "TRigidType2" 4 6
        , codeActionTest "TLocalBinding" 7 22
        , codeActionTest "TLocalBindingShadow1" 11 8
        , codeActionTest "TLocalBindingShadow2" 7 22
        , codeActionProperties "TErrorGivenPartialSignature" [(4, 13)] $ \actions -> liftIO $ length actions @?= 0
    ]

testRegexes :: TestTree
testRegexes = testGroup "Regex Testing" [
        testRegexOne
        , testRegexTwo
        , testRegex921One
    ]

testRegexOne :: TestTree
testRegexOne = testGroup "Regex One" [
        regexTest "error1.txt" regex True
        , regexTest "error2.txt" regex True
        , regexTest "error3.txt" regex False
        , regexTest "error4.txt" regex True
        , regexTest "error5.txt" regex True
    ]
    where
        regex = errorMessageRegexes !! 0

testRegexTwo :: TestTree
testRegexTwo = testGroup "Regex Two" [
        regexTest "error1.txt" regex False
        , regexTest "error2.txt" regex False
        , regexTest "error3.txt" regex True
        , regexTest "error4.txt" regex False
        , regexTest "error5.txt" regex False
    ]
    where
        regex = errorMessageRegexes !! 1

-- test ghc-9.2 error message regex
testRegex921One :: TestTree
testRegex921One = testGroup "Regex One" [
        regexTest "ghc921-error1.txt" regex True
        , regexTest "ghc921-error2.txt" regex True
        , regexTest "ghc921-error3.txt" regex True
    ]
    where
        regex = errorMessageRegexes !! 2

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-change-type-signature-plugin" </> "test" </> "testdata"

goldenChangeSignature :: FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenChangeSignature fp = goldenWithHaskellDoc def changeTypeSignaturePlugin (fp <> " (golden)") testDataDir fp "expected" "hs"

codeActionTest :: FilePath -> Int -> Int -> TestTree
codeActionTest fp line col = goldenChangeSignature fp $ \doc -> do
    void waitForDiagnostics  -- code actions are triggered from Diagnostics
    void waitForBuildQueue  -- apparently some tests need this to get the CodeAction to show up
    actions <- getCodeActions doc (pointRange line col)
    foundActions <- findChangeTypeActions actions
    liftIO $ length foundActions @?= 1
    executeCodeAction (head foundActions)

codeActionProperties :: TestName -> [(Int, Int)] -> ([CodeAction] -> Session ()) -> TestTree
codeActionProperties fp locs assertions = testCase fp $ do
    runSessionWithServer def changeTypeSignaturePlugin testDataDir $ do
        openDoc (fp <.> ".hs") "haskell" >>= codeActionsFromLocs >>= findChangeTypeActions >>= assertions
    where
        codeActionsFromLocs doc = concat <$> mapM (getCodeActions doc . uncurry pointRange) locs

findChangeTypeActions :: [Command |? CodeAction] -> Session [CodeAction]
findChangeTypeActions = pure . filter isChangeTypeAction . rights . map toEither
    where
        isChangeTypeAction CodeAction{_kind} = case _kind of
          Nothing -> False
          Just kind -> case kind of
            "quickfix.changeTypeSignature" -> True
            _                              -> False


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
