{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Ide.Plugin.Rename as Rename
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

renamePlugin :: PluginDescriptor IdeState
renamePlugin = Rename.descriptor "rename"

tests :: TestTree
tests = testGroup "rename"
    [ testGroup "Top-level renames"
        [ goldenWithRename "function name" "FunctionName" $ \doc -> do
            rename doc (Position 3 1) "baz"
        , goldenWithRename "GADT" "Gadt" $ \doc -> do
            rename doc (Position 6 37) "Expr"
        , goldenWithRename "imported function" "ImportedFunction" $ \doc -> do
            rename doc (Position 3 8) "baz"
        , goldenWithRename "exported function" "ExportedFunction" $ \doc -> do
            rename doc (Position 2 1) "quux"
        , goldenWithRename "hidden function" "HiddenFunction" $ \doc -> do
            rename doc (Position 0 32) "quux"
        , goldenWithRename "allign indentation" "Indentation" $ \doc -> do
            rename doc (Position 0 2) "fooBarQuux"
        , goldenWithRename "shadowed name" "ShadowedName" $ \doc -> do
            rename doc (Position 1 1) "baz"
        , goldenWithRename "qualified function" "QualifiedFunction" $ \doc -> do
            rename doc (Position 3 12) "baz"
        , goldenWithRename "type constructor" "TypeConstructor" $ \doc -> do
            rename doc (Position 2 17) "BinaryTree"
        , goldenWithRename "data constructor" "DataConstructor" $ \doc -> do
            rename doc (Position 0 15) "Op"
        , goldenWithRename "import hiding" "ImportHiding" $ \doc -> do
            rename doc (Position 0 22) "hiddenFoo"
        , goldenWithRename "qualified as" "QualifiedAs" $ \doc -> do
            rename doc (Position 3 10) "baz"
        , goldenWithRename "qualified shadowing" "QualifiedShadowing" $ \doc -> do
            rename doc (Position 3 12) "foobar"
        ]
    , testGroup "non Top-level renames"
        [ goldenWithRename "function argument" "FunctionArgument" $ \doc -> do
            rename doc (Position 3 4) "y"
        , goldenWithRename "record field" "RecordField" $ \doc -> do
            rename doc (Position 6 9) "number"
        , goldenWithRename "type variable" "TypeVariable" $ \doc -> do
            rename doc (Position 0 13) "b"
        ]
    ]

goldenWithRename :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRename title path act =
    goldenWithHaskellDoc renamePlugin title testDataDir path "expected" "hs" $ \doc -> do
        waitForProgressDone
        waitForProgressDone
        act doc

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
