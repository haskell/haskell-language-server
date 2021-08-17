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
        [ goldenWithRename "data constructor" "DataConstructor" $ \doc -> do
            rename doc (Position 0 15) "Op"
        , goldenWithRename "exported function" "ExportedFunction" $ \doc -> do
            rename doc (Position 2 1) "quux"
        , goldenWithRename "function name" "FunctionName" $ \doc -> do
            rename doc (Position 3 1) "baz"
        , goldenWithRename "GADT" "Gadt" $ \doc -> do
            rename doc (Position 6 37) "Expr"
        , goldenWithRename "hidden function" "HiddenFunction" $ \doc -> do
            rename doc (Position 0 32) "quux"
        , goldenWithRename "imported function" "ImportedFunction" $ \doc -> do
            rename doc (Position 3 8) "baz"
        , goldenWithRename "import hiding" "ImportHiding" $ \doc -> do
            rename doc (Position 0 22) "hiddenFoo"
        , goldenWithRename "allign indentation" "Indentation" $ \doc -> do
            rename doc (Position 0 2) "fooBarQuux"
        , goldenWithRename "qualified as" "QualifiedAs" $ \doc -> do
            rename doc (Position 3 10) "baz"
        , goldenWithRename "qualified shadowing" "QualifiedShadowing" $ \doc -> do
            rename doc (Position 3 12) "foobar"
        , goldenWithRename "qualified function" "QualifiedFunction" $ \doc -> do
            rename doc (Position 3 12) "baz"
        , goldenWithRename "shadowed name" "ShadowedName" $ \doc -> do
            rename doc (Position 1 1) "baz"
        , expectFailBecause "Bug: Test case giving different result to editor" $
          goldenWithRename "type constructor" "TypeConstructor" $ \doc -> do
            rename doc (Position 2 17) "BinaryTree"
        ]
    , expectFailBecause "Only top-level renames are implemented" $
      testGroup "non Top-level renames"
        [ goldenWithRename "function argument" "FunctionArgument" $ \doc -> do
            rename doc (Position 3 4) "y"
        , goldenWithRename "record field" "RecordField" $ \doc -> do
            rename doc (Position 6 9) "number"
        , goldenWithRename "type variable" "TypeVariable" $ \doc -> do
            rename doc (Position 0 13) "b"
        ]
    ]

goldenWithRename :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRename title path =
    goldenWithHaskellDoc renamePlugin title testDataDir path "expected" "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
