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
        [ ignoreTestBecause "Inconsistent - need to wait for typecheck" $
          goldenWithRename "function name" "FunctionName" $ \doc -> do
            rename doc (Position 3 1) "baz"
        , ignoreTestBecause "Inconsistent - need to wait for typecheck" $
          goldenWithRename "GADT" "Gadt" $ \doc -> do
            rename doc (Position 6 37) "Expr"
        , ignoreTestBecause "Inconsistent - need to wait for typecheck" $
          goldenWithRename "imported function" "ImportedFunction" $ \doc -> do
            rename doc (Position 3 8) "baz"
        , ignoreTestBecause "Inconsistent - need to wait for typecheck" $
          goldenWithRename "exported function" "ExportedFunction" $ \doc -> do
            rename doc (Position 2 1) "quux"
        , ignoreTestBecause "Inconsistent - need to wait for typecheck" $
          goldenWithRename "hidden function" "HiddenFunction" $ \doc -> do
            rename doc (Position 0 32) "quux"
        , ignoreTestBecause "Inconsistent - need to wait for typecheck" $
          goldenWithRename "allign indentation" "Indentation" $ \doc -> do
            rename doc (Position 0 2) "fooBarQuux"
        ,  ignoreTestBecause "Inconsistent - need to wait for typecheck" $
          goldenWithRename "shadowed name" "ShadowedName" $ \doc -> do
            rename doc (Position 1 1) "baz"
        , ignoreTestBecause "Inconsistent - need to wait for typecheck" $
          goldenWithRename "qualified function" "QualifiedFunction" $ \doc -> do
            rename doc (Position 3 12) "baz"
        , ignoreTestBecause "Inconsistent - need to wait for typecheck" $
          goldenWithRename "type constructor" "TypeConstructor" $ \doc -> do
            rename doc (Position 2 15) "BinaryTree"
        , expectFailBecause "Not implemented yet" $
          goldenWithRename "data constructor" "DataConstructor" $ \doc -> do
            rename doc (Position 0 13) "Apply"
        ]
    , testGroup "non Top-level renames"
        [ expectFailBecause "Only top-level renames are implemented" $
          goldenWithRename "function argument" "FunctionArgument" $ \doc -> do
            rename doc (Position 3 4) "y"
        , expectFailBecause "Only top-level renames are implemented" $
          goldenWithRename "record field" "RecordField" $ \doc -> do
            rename doc (Position 6 9) "number"
        , expectFailBecause "Only top-level renames are implemented" $
          goldenWithRename "type variable" "TypeVariable" $ \doc -> do
            rename doc (Position 0 13) "b"
        ]
    ]

goldenWithRename :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRename title path =
    goldenWithHaskellDoc renamePlugin title testDataDir path "expected" "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
