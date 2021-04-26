import           Data.List.Extra    (trimEnd)
import           Data.Maybe
import           System.Environment
import           System.Process
import           Test.Hls

main :: IO ()
main = do
  flushStackEnvironment
  defaultTestRunner $ testGroup "haskell-language-server-wrapper" [projectGhcVersionTests]

projectGhcVersionTests :: TestTree
projectGhcVersionTests = testGroup "--project-ghc-version"
  [ testCase "stack with ghc 8.10.4" $
      testDir "test/wrapper/testdata/stack-8.10.4" "8.10.4"
  , testCase "stack with ghc 8.8.3" $
      testDir "test/wrapper/testdata/stack-8.8.3" "8.8.3"
  , testCase "cabal with global ghc" $ do
      ghcVer <- trimEnd <$> readProcess "ghc" ["--numeric-version"] ""
      testDir "test/wrapper/testdata/cabal-cur-ver" ghcVer
  ]

testDir :: FilePath -> String -> Assertion
testDir dir expectedVer =
  withCurrentDirectoryInTmp dir $ do
    testExe <- fromMaybe "haskell-language-server-wrapper"
      <$> lookupEnv "HLS_WRAPPER_TEST_EXE"
    actualVer <- trimEnd <$> readProcess testExe ["--project-ghc-version"] ""
    actualVer @?= expectedVer
