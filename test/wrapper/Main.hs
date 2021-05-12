import           Data.List.Extra    (isInfixOf, trimEnd)
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
  , testCase "stack with existing cabal build artifact" $ do
      -- Should report cabal as existing build artifacts are more important than
      -- the existence of 'stack.yaml'
      testProjectType "test/wrapper/testdata/stack-with-dist-newstyle"
        ("cradleOptsProg = CradleAction: Cabal" `isInfixOf`)
  ]

testDir :: FilePath -> String -> Assertion
testDir dir expectedVer =
  withCurrentDirectoryInTmp dir $ do
    testExe <- fromMaybe "haskell-language-server-wrapper"
      <$> lookupEnv "HLS_WRAPPER_TEST_EXE"
    actualVer <- trimEnd <$> readProcess testExe ["--project-ghc-version"] ""
    actualVer @?= expectedVer

testProjectType :: FilePath -> (String -> Bool) -> Assertion
testProjectType dir matcher =
  withCurrentDirectoryInTmp' [".stack-work", "dist"] dir $ do
    wrapperTestExe <- fromMaybe "haskell-language-server-wrapper"
      <$> lookupEnv "HLS_WRAPPER_TEST_EXE"
    hlsTestExe <- fromMaybe "haskell-language-server"
      <$> lookupEnv "HLS_TEST_EXE"
    actualWrapperCradle <- trimEnd <$> readProcess wrapperTestExe ["--print-cradle"] ""
    actualHlsCradle <- trimEnd <$> readProcess hlsTestExe ["--print-cradle"] ""
    matcher actualWrapperCradle @? "Wrapper reported wrong project type: " ++ actualWrapperCradle
    matcher actualHlsCradle @? "HLS reported wrong project type: " ++ actualHlsCradle
