{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Main (main) where

import           Control.Lens               ((^.))
import           Data.Aeson
import           Data.Functor               (void)
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

tests :: TestTree
tests = testGroup "Rename"
    [ renameTests
    , moduleNameTests
    ]

renameTests :: TestTree
renameTests = testGroup "Identifier"
    [ goldenWithRename "Data constructor" "DataConstructor" $ \doc ->
        rename doc (Position 0 15) "Op"
    , goldenWithRename "Data constructor with fields" "DataConstructorWithFields" $ \doc ->
        rename doc (Position 1 13) "FooRenamed"
    , knownBrokenForGhcVersions [GHC96, GHC98] "renaming Constructor{..} with RecordWildcard removes .." $
        goldenWithRename "Data constructor with fields" "DataConstructorWithFieldsRecordWildcards" $ \doc ->
          rename doc (Position 1 13) "FooRenamed"
    , goldenWithRename "Exported function" "ExportedFunction" $ \doc ->
        rename doc (Position 2 1) "quux"
    , goldenWithRename "Field Puns" "FieldPuns" $ \doc ->
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
    , goldenWithRename "Indirect Puns" "IndirectPuns" $ \doc ->
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
    , goldenWithRename "Record field" "RecordField" $ \doc ->
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
        let expectedError = TResponseError
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

moduleNameTests :: TestTree
moduleNameTests =
  testGroup "ModuleName"
  [ goldenWithModuleName "Add module header to empty module" "TEmptyModule" $ \doc -> do
      [CodeLens { _command = Just c }] <- getCodeLenses doc
      executeCommand c
      void $ skipManyTill anyMessage (message SMethod_WorkspaceApplyEdit)

  , goldenWithModuleName "Fix wrong module name" "TWrongModuleName" $ \doc -> do
      [CodeLens { _command = Just c }] <- getCodeLenses doc
      executeCommand c
      void $ skipManyTill anyMessage (message SMethod_WorkspaceApplyEdit)

  , goldenWithModuleName "Must infer module name as Main, if the file name starts with a lowercase" "mainlike" $ \doc -> do
      [CodeLens { _command = Just c }] <- getCodeLenses doc
      executeCommand c
      void $ skipManyTill anyMessage (message SMethod_WorkspaceApplyEdit)

  , goldenWithModuleName "Fix wrong module name in nested directory" "subdir/TWrongModuleName" $ \doc -> do
      [CodeLens { _command = Just c }] <- getCodeLenses doc
      executeCommand c
      void $ skipManyTill anyMessage (message SMethod_WorkspaceApplyEdit)
  , testCase "Should not show code lens if the module name is correct" $
      runSessionWithServer def renamePlugin modNameTestDataDir $ do
        doc <- openDoc "CorrectName.hs" "haskell"
        lenses <- getCodeLenses doc
        liftIO $ lenses @?= []
        closeDoc doc
  -- https://github.com/haskell/haskell-language-server/issues/3047
  , goldenWithModuleName "Fix#3047" "canonicalize/Lib/A" $ \doc -> do
      [CodeLens { _command = Just c }] <- getCodeLenses doc
      executeCommand c
      void $ skipManyTill anyMessage (message SMethod_WorkspaceApplyEdit)
  , testCase "Keep stale lens even if parse failed" $ do
      runSessionWithServer def renamePlugin modNameTestDataDir $ do
        doc <- openDoc "Stale.hs" "haskell"
        oldLens <- getCodeLenses doc
        let edit = TextEdit (mkRange 1 0 1 0) "f ="
        _ <- applyEdit doc edit
        newLens <- getCodeLenses doc
        liftIO $ newLens @?= oldLens
        closeDoc doc
  ]

goldenWithModuleName :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithModuleName title path = goldenWithHaskellDoc def renamePlugin title modNameTestDataDir path "expected" "hs"

modNameTestDataDir :: FilePath
modNameTestDataDir = testDataDir </> "mod_name"

goldenWithRename :: TestName-> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRename title path act =
    goldenWithHaskellDoc (def { plugins = M.fromList [("rename", def { plcConfig = "crossModule" .= True })] })
       renamePlugin title testDataDir path "expected" "hs" act

renameExpectError :: TResponseError Method_TextDocumentRename -> TextDocumentIdentifier -> Position -> Text -> Session ()
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
  Session (TResponseError Method_TextDocumentRename)
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
  { testDirLocation = Left $ testDataDir </> subdir
  , testPluginDescriptor = renamePlugin
  , testConfigCaps = codeActionNoResolveCaps }
  . const
