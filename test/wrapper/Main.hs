import           Data.List.Extra    (isInfixOf, trimEnd)
import           Data.Maybe
import           System.Directory
import           System.Environment
import           System.Process
import           Test.Hls

main :: IO ()
main = defaultTestRunner $ testGroup "haskell-language-server-wrapper" [projectGhcVersionTests]

projectGhcVersionTests :: TestTree
projectGhcVersionTests = testGroup "--project-ghc-version"
  [ stackTest "9.2.8"
  , testCase "cabal with global ghc" $ do
      ghcVer <- trimEnd <$> readProcess "ghc" ["--numeric-version"] ""
      testDir "test/wrapper/testdata/cabal-cur-ver" ghcVer
  , testCase "stack with existing cabal build artifact" $ do
      -- Should report cabal as existing build artifacts are more important than
      -- the existence of 'stack.yaml'
      testProjectType "test/wrapper/testdata/stack-with-dist-newstyle"
        ("cradleOptsProg = CradleAction: Cabal" `isInfixOf`)
  ]
  where
      stackTest ghcVer= testCase ("stack with ghc " ++ ghcVer) $
        testDir ("test/wrapper/testdata/stack-" ++ ghcVer) ghcVer

testDir :: FilePath -> String -> Assertion
testDir dir expectedVer =
  withCurrentDirectoryInTmp dir $ do
    testExe <- fromMaybe "haskell-language-server-wrapper"
      <$> lookupEnv "HLS_WRAPPER_TEST_EXE"
    assertExecutableExists testExe
    cwd <- getCurrentDirectory
    putStrLn cwd
    print =<< listDirectory cwd
    print =<< readFile (cwd ++ "/stack.yaml")
    print =<< readProcess "stack" ["--stack-yaml", cwd ++ "/stack.yaml", "setup"] ""
    actualVer <- trimEnd <$> readProcess testExe ["--project-ghc-version"] ""
    actualVer @?= expectedVer

testProjectType :: FilePath -> (String -> Bool) -> Assertion
testProjectType dir matcher =
  withCurrentDirectoryInTmp' [".stack-work", "dist"] dir $ do
    wrapperTestExe <- fromMaybe "haskell-language-server-wrapper"
      <$> lookupEnv "HLS_WRAPPER_TEST_EXE"
    assertExecutableExists wrapperTestExe
    hlsTestExe <- fromMaybe "haskell-language-server"
      <$> lookupEnv "HLS_TEST_EXE"
    assertExecutableExists hlsTestExe
    actualWrapperCradle <- trimEnd <$> readProcess wrapperTestExe ["--print-cradle"] ""
    actualHlsCradle <- trimEnd <$> readProcess hlsTestExe ["--print-cradle"] ""
    matcher actualWrapperCradle @? "Wrapper reported wrong project type: " ++ actualWrapperCradle
    matcher actualHlsCradle @? "HLS reported wrong project type: " ++ actualHlsCradle

assertExecutableExists :: FilePath -> IO ()
assertExecutableExists exe = do
    mbExe <- findExecutable exe
    case mbExe of
        Nothing   -> assertFailure $ "Could not find executable " ++ exe
        Just path -> putStrLn $ "Found " ++ exe ++ " at " ++ path
