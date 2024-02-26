import           Data.List.Extra    (isInfixOf, trimEnd)
import           Data.Maybe
import           System.Environment
import           System.Process
import           Test.Hls

main :: IO ()
main = defaultTestRunner $ testGroup "haskell-language-server-wrapper" [projectGhcVersionTests]

projectGhcVersionTests :: TestTree
projectGhcVersionTests = testGroup "--project-ghc-version"
  [ let ghcVer = case ghcVersion of
           GHC92 -> "9.2.8"
           GHC94 -> "9.4.8"
           GHC96 -> "9.6.4"
           GHC98 -> "9.8.1"
        writeStackYaml = writeFile "stack.yaml"
            -- Use system-ghc and install-ghc to avoid stack downloading ghc in CI
            -- (and use ghcup-managed ghc instead)
            ("{resolver: ghc-"++ ghcVer ++", system-ghc: true, install-ghc: false}")
    in testCase ("stack with ghc " ++ ghcVer) $
          testDir writeStackYaml "test/wrapper/testdata/stack-specific-ghc" ghcVer
  , testCase "cabal with global ghc" $ do
      ghcVer <- trimEnd <$> readProcess "ghc" ["--numeric-version"] ""
      testDir (pure ()) "test/wrapper/testdata/cabal-cur-ver" ghcVer
  , testCase "stack with existing cabal build artifact" $ do
      -- Should report cabal as existing build artifacts are more important than
      -- the existence of 'stack.yaml'
      testProjectType "test/wrapper/testdata/stack-with-dist-newstyle"
        ("cradleOptsProg = CradleAction: Cabal" `isInfixOf`)
  ]

testDir :: IO () -> FilePath -> String -> Assertion
testDir extraSetup dir expectedVer =
  withCurrentDirectoryInTmp dir $ do
    extraSetup
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
