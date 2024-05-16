{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Main (main) where

import           Control.Lens               ((^.))
import           Data.Aeson
import qualified Data.Map                   as M
import           Data.Text                  (Text, pack)
import           Ide.Plugin.Config
import qualified Ide.Plugin.Rename          as Rename
import qualified Language.LSP.Protocol.Lens as L
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
    , ignoreForGhcVersions [GHC92] recordConstructorIssue $
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
    , ignoreForGhcVersions [GHC92] recordConstructorIssue $
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
    , ignoreForGhcVersions [GHC92] recordConstructorIssue $
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
    , goldenWithRename "Rename within comment" "Comment" $ \doc -> do
        let expectedError = ResponseError
                (InR ErrorCodes_InvalidParams)
                "rename: Invalid Params: No symbol to rename at given position"
                Nothing
        renameExpectError expectedError doc (Position 0 10) "ImpossibleRename"

    , testCase "fails when module does not compile" $ runRenameSession "" $ do
        doc <- openDoc "FunctionArgument.hs" "haskell"
        expectNoMoreDiagnostics 3 doc "typecheck"

        -- Update the document so it doesn't compile
        let change = TextDocumentContentChangeEvent $ InL TextDocumentContentChangePartial
              { _range = Range (Position 2 13) (Position 2 17)
              , _rangeLength = Nothing
              , _text = "A"
              }
        changeDoc doc [change]
        diags@(tcDiag : _) <- waitForDiagnosticsFrom doc

        -- Make sure there's a typecheck error
        liftIO $ do
          length diags @?= 1
          tcDiag ^. L.range @?= Range (Position 2 13) (Position 2 14)
          tcDiag ^. L.severity @?= Just DiagnosticSeverity_Error
          tcDiag ^. L.source @?= Just "typecheck"

        -- Make sure renaming fails
        renameErr <- expectRenameError doc (Position 3 0) "foo'"
        liftIO $ do
          renameErr ^. L.code @?= InL LSPErrorCodes_RequestFailed
          renameErr ^. L.message @?= "rename: Rule Failed: GetHieAst"

        -- Update the document so it compiles
        let change' = TextDocumentContentChangeEvent $  InL TextDocumentContentChangePartial
              { _range = Range (Position 2 13) (Position 2 14)
              , _rangeLength = Nothing
              , _text = "Int"
              }
        changeDoc doc [change']
        expectNoMoreDiagnostics 3 doc "typecheck"

        -- Make sure renaming succeeds
        rename doc (Position 3 0) "foo'"
    ]

goldenWithRename :: TestName-> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRename title path act =
    goldenWithHaskellDoc (def { plugins = M.fromList [("rename", def { plcConfig = "crossModule" .= True })] })
       renamePlugin title testDataDir path "expected" "hs" act

renameExpectError :: ResponseError -> TextDocumentIdentifier -> Position -> Text -> Session ()
renameExpectError expectedError doc pos newName = do
  let params = RenameParams Nothing doc pos newName
  rsp <- request SMethod_TextDocumentRename params
  case rsp ^. L.result of
    Right _ -> liftIO $ assertFailure $ "Was expecting " <> show expectedError <> ", got success"
    Left actualError -> liftIO $ assertEqual "ResponseError" expectedError actualError

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-rename-plugin" </> "test" </> "testdata"

-- | Attempts to renames the term at the specified position, expecting a failure
expectRenameError ::
  TextDocumentIdentifier ->
  Position ->
  String ->
  Session ResponseError
expectRenameError doc pos newName = do
  let params = RenameParams Nothing doc pos (pack newName)
  rsp <- request SMethod_TextDocumentRename params
  case rsp ^. L.result of
    Left err -> pure err
    Right _ -> liftIO $ assertFailure $
      "Got unexpected successful rename response for " <> show (doc ^. L.uri)

runRenameSession :: FilePath -> Session a -> IO a
runRenameSession subdir = failIfSessionTimeout
  .  runSessionWithTestConfig def
  {testDirLocation= Left $ testDataDir </> subdir,
  testPluginDescriptor=renamePlugin,
  testConfigCaps=codeActionNoResolveCaps}
  . const
