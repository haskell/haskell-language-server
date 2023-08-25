module Main (main) where

import           Parser
import           System.Directory (getCurrentDirectory)
import           System.FilePath  (addTrailingPathSeparator, (</>))
import           Test.Tasty


main :: IO ()
main = do
  testDir <- getTestDir
  defaultMain $
    testGroup
      "Cabal Plugin Tests"
      [ parserTests testDir
      ]

getTestDir :: IO FilePath
getTestDir = do
    cwd <- getCurrentDirectory
    pure $ addTrailingPathSeparator $ cwd </> "test" </> "testdata"
