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
tests = testGroup "Rename"
    [ goldenWithRename "Data constructor" "DataConstructor" $ \doc -> do
        rename doc (Position 0 15) "Op"
    , goldenWithRename "Exported function" "ExportedFunction" $ \doc -> do
        rename doc (Position 2 1) "quux"
    , goldenWithRename "Function argument" "FunctionArgument" $ \doc -> do
        rename doc (Position 3 4) "y"
    , goldenWithRename "Function name" "FunctionName" $ \doc -> do
        rename doc (Position 3 1) "baz"
    , goldenWithRename "GADT" "Gadt" $ \doc -> do
        rename doc (Position 6 37) "Expr"
    , goldenWithRename "Hidden function" "HiddenFunction" $ \doc -> do
        rename doc (Position 0 32) "quux"
    , goldenWithRename "Imported function" "ImportedFunction" $ \doc -> do
        rename doc (Position 3 8) "baz"
    , goldenWithRename "Import hiding" "ImportHiding" $ \doc -> do
        rename doc (Position 0 22) "hiddenFoo"
    , goldenWithRename "Let expression" "LetExpression" $ \doc -> do
        rename doc (Position 5 11) "foobar"
    , goldenWithRename "Qualified as" "QualifiedAs" $ \doc -> do
        rename doc (Position 3 10) "baz"
    , goldenWithRename "Qualified shadowing" "QualifiedShadowing" $ \doc -> do
        rename doc (Position 3 12) "foobar"
    , goldenWithRename "Qualified function" "QualifiedFunction" $ \doc -> do
        rename doc (Position 3 12) "baz"
    , goldenWithRename "Realigns do block indentation" "RealignDo" $ \doc -> do
        rename doc (Position 0 2) "fooBarQuux"
    , goldenWithRename "Record field" "RecordField" $ \doc -> do
        rename doc (Position 6 9) "number"
    , goldenWithRename "Shadowed name" "ShadowedName" $ \doc -> do
        rename doc (Position 1 1) "baz"
    , goldenWithRename "Typeclass" "Typeclass" $ \doc -> do
        rename doc (Position 8 15) "Equal"
    , goldenWithRename "Type constructor" "TypeConstructor" $ \doc -> do
        rename doc (Position 2 17) "BinaryTree"
    , goldenWithRename "Type variable" "TypeVariable" $ \doc -> do
        rename doc (Position 0 13) "b"
    ]

goldenWithRename :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRename title path =
    goldenWithHaskellDoc renamePlugin title testDataDir path "expected" "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
