{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Ide.Plugin.Rename as Rename
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

renamePlugin :: PluginDescriptor IdeState
renamePlugin = Rename.descriptor "pragmas"

tests :: TestTree
tests = testGroup "rename"
    [ goldenWithRename "function name" "FunctionName" $ \doc -> do
        rename doc (Position 3 1) "baz" -- foo :: Int -> Int
    ]

goldenWithRename :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRename title path = goldenWithHaskellDoc renamePlugin title testDataDir path "expected" "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
