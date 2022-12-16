{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Data.Aeson
import qualified Data.Map          as M
import           Ide.Plugin.Config
import qualified Ide.Plugin.Rename as Rename
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

renamePlugin :: PluginTestDescriptor Rename.Log
renamePlugin = mkPluginTestDescriptor Rename.descriptor "rename"

-- See https://github.com/wz1000/HieDb/issues/45
recordConstructorIssue :: String
recordConstructorIssue = "HIE references for record fields incorrect with GHC versions >= 9"

tests :: TestTree
tests = testGroup "Rename"
    [ goldenWithRename "Data constructor" "DataConstructor" $ \doc ->
        rename doc (Position 0 15) "Op"
    , goldenWithRename "Exported function" "ExportedFunction" $ \doc ->
        rename doc (Position 2 1) "quux"
    , ignoreForGhcVersions [GHC90, GHC92] recordConstructorIssue $
      goldenWithRename "Field Puns" "FieldPuns" $ \doc ->
        rename doc (Position 7 13) "bleh"
    , goldenWithRename "Function argument" "FunctionArgument" $ \doc ->
        rename doc (Position 3 4) "y"
    , goldenWithRename "Function name" "FunctionName" $ \doc ->
        rename doc (Position 3 1) "baz"
    , goldenWithRename "GADT" "Gadt" $ \doc ->
        rename doc (Position 6 37) "Expr"
    , goldenWithRename "Hidden function" "HiddenFunction" $ \doc ->
        rename doc (Position 0 32) "quux"
    , goldenWithRename "Imported function" "ImportedFunction" $ \doc ->
        rename doc (Position 3 8) "baz"
    , goldenWithRename "Import hiding" "ImportHiding" $ \doc ->
        rename doc (Position 0 22) "hiddenFoo"
    , ignoreForGhcVersions [GHC90, GHC92] recordConstructorIssue $
      goldenWithRename "Indirect Puns" "IndirectPuns" $ \doc ->
        rename doc (Position 4 23) "blah"
    , goldenWithRename "Let expression" "LetExpression" $ \doc ->
        rename doc (Position 5 11) "foobar"
    , goldenWithRename "Qualified as" "QualifiedAs" $ \doc ->
        rename doc (Position 3 10) "baz"
    , goldenWithRename "Qualified shadowing" "QualifiedShadowing" $ \doc ->
        rename doc (Position 3 12) "foobar"
    , goldenWithRename "Qualified function" "QualifiedFunction" $ \doc ->
        rename doc (Position 3 12) "baz"
    , goldenWithRename "Realigns do block indentation" "RealignDo" $ \doc ->
        rename doc (Position 0 2) "fooBarQuux"
    , ignoreForGhcVersions [GHC90, GHC92] recordConstructorIssue $
      goldenWithRename "Record field" "RecordField" $ \doc ->
        rename doc (Position 6 9) "number"
    , goldenWithRename "Shadowed name" "ShadowedName" $ \doc ->
        rename doc (Position 1 1) "baz"
    , goldenWithRename "Typeclass" "Typeclass" $ \doc ->
        rename doc (Position 8 15) "Equal"
    , goldenWithRename "Type constructor" "TypeConstructor" $ \doc ->
        rename doc (Position 2 17) "BinaryTree"
    , goldenWithRename "Type variable" "TypeVariable" $ \doc ->
        rename doc (Position 0 13) "b"
    ]

goldenWithRename :: TestName-> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRename title path act =
    goldenWithHaskellDoc renamePlugin title testDataDir path "expected" "hs" $ \doc -> do
        sendConfigurationChanged $ toJSON $
            def { plugins = M.fromList [("rename", def { plcConfig = "crossModule" .= True })] }
        act doc


testDataDir :: FilePath
testDataDir = "test" </> "testdata"
