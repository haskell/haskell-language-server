import Data.List
import Data.Char
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.HUnit
import System.Directory
import System.Process

main :: IO ()
main = defaultMain $
  testGroup "haskell-language-server-wrapper" [projectGhcVersionTests]

--TODO: WAIT ON HIE-BIOS STOP FILES
projectGhcVersionTests :: TestTree
projectGhcVersionTests = testGroup "--project-ghc-version"
  [ testCase "stack with ghc 8.10.1" $
      testDir "test/wrapper/testdata/stack-8.10.1" "8.10.1"
  , testCase "stack with ghc 8.8.3" $
      testDir "test/wrapper/testdata/stack-8.8.3" "8.8.3"
  , testCase "cabal with global ghc" $ do
      ghcVer <- trim <$> readProcess "ghc" ["--numeric-version"] ""
      testDir "test/wrapper/testdata/cabal-cur-ver" ghcVer
  ]

testDir :: FilePath -> String -> Assertion
testDir dir expectedVer = do
  wrapper <- findExe "haskell-language-server-wrapper"
  withCurrentDirectory dir $ do
    actualVer <- trim <$> readProcess wrapper ["--project-ghc-version"] ""
    actualVer @?= expectedVer

trim :: String -> String
trim = dropWhileEnd isSpace

