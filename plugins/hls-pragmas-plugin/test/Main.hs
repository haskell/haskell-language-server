{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import           Control.Lens            ((^.))
import qualified Ide.Plugin.Pragmas      as Pragmas
import qualified Language.LSP.Types.Lens as L
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

pragmasPlugin :: PluginDescriptor IdeState
pragmasPlugin = Pragmas.descriptor "pragmas"

tests :: TestTree
tests =
  testGroup "pragmas"
  [ codeActionTests
  , completionTests
  ]

codeActionTests :: TestTree
codeActionTests =
  testGroup "code actions"
  [ goldenWithPragmas "adds TypeSynonymInstances pragma" "NeedsPragmas" $ \doc -> do
      _ <- waitForDiagnosticsFromSource doc "typecheck"
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Add \"TypeSynonymInstances\"" `elem` map (^. L.title) cas @? "Contains TypeSynonymInstances code action"
      liftIO $ "Add \"FlexibleInstances\"" `elem` map (^. L.title) cas @? "Contains FlexibleInstances code action"
      executeCodeAction $ head cas

  , goldenWithPragmas "adds TypeApplications pragma" "TypeApplications" $ \doc -> do
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Add \"TypeApplications\"" `elem` map (^. L.title) cas @? "Contains TypeApplications code action"
      executeCodeAction $ head cas

  , goldenWithPragmas "no duplication" "NamedFieldPuns" $ \doc -> do
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getCodeActions doc (Range (Position 8 9) (Position 8 9))
      liftIO $ length cas == 1 @? "Expected one code action, but got: " <> show cas
      let ca = head cas
      liftIO $ (ca ^. L.title == "Add \"NamedFieldPuns\"") @? "NamedFieldPuns code action"
      executeCodeAction ca

  , goldenWithPragmas "after shebang" "AfterShebang" $ \doc -> do
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Add \"NamedFieldPuns\"" `elem` map (^. L.title) cas @? "Contains NamedFieldPuns code action"
      executeCodeAction $ head cas

  , goldenWithPragmas "append to existing pragmas" "AppendToExisting" $ \doc -> do
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Add \"NamedFieldPuns\"" `elem` map (^. L.title) cas @? "Contains NamedFieldPuns code action"
      executeCodeAction $ head cas

  , goldenWithPragmas "before doc comments" "BeforeDocComment" $ \doc -> do
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Add \"NamedFieldPuns\"" `elem` map (^. L.title) cas @? "Contains NamedFieldPuns code action"
      executeCodeAction $ head cas

  , goldenWithPragmas "before doc comments" "MissingSignatures" $ \doc -> do
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Disable \"missing-signatures\" warnings" `elem` map (^. L.title) cas @? "Contains missing-signatures code action"
      executeCodeAction $ head cas

  , goldenWithPragmas "before doc comments" "UnusedImports" $ \doc -> do
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Disable \"unused-imports\" warnings" `elem` map (^. L.title) cas @? "Contains unused-imports code action"
      executeCodeAction $ head cas
  ]

completionTests :: TestTree
completionTests =
  testGroup "completions"
  [ testCase "completes pragmas" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      _ <- waitForDiagnostics
      let te = TextEdit (Range (Position 0 4) (Position 0 34)) ""
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 4)
      let item = head $ filter ((== "LANGUAGE") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "LANGUAGE"
        item ^. L.kind @?= Just CiKeyword
        item ^. L.insertTextFormat @?= Just Snippet
        item ^. L.insertText @?= Just "LANGUAGE ${1:extension} #-}"

  , testCase "completes pragmas no close" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      let te = TextEdit (Range (Position 0 4) (Position 0 24)) ""
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 4)
      let item = head $ filter ((== "LANGUAGE") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "LANGUAGE"
        item ^. L.kind @?= Just CiKeyword
        item ^. L.insertTextFormat @?= Just Snippet
        item ^. L.insertText @?= Just "LANGUAGE ${1:extension}"

  , testCase "completes options pragma" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      _ <- waitForDiagnostics
      let te = TextEdit (Range (Position 0 4) (Position 0 34)) "OPTIONS"
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 4)
      let item = head $ filter ((== "OPTIONS_GHC") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "OPTIONS_GHC"
        item ^. L.kind @?= Just CiKeyword
        item ^. L.insertTextFormat @?= Just Snippet
        item ^. L.insertText @?= Just "OPTIONS_GHC -${1:option} #-}"

  , testCase "completes ghc options pragma values" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      let te = TextEdit (Range (Position 0 0) (Position 0 0)) "{-# OPTIONS_GHC -Wno-red  #-}\n"
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 24)
      let item = head $ filter ((== "Wno-redundant-constraints") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "Wno-redundant-constraints"
        item ^. L.kind @?= Just CiKeyword
        item ^. L.insertTextFormat @?= Nothing
        item ^. L.insertText @?= Nothing

  , testCase "completes language extensions" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      _ <- waitForDiagnostics
      let te = TextEdit (Range (Position 0 24) (Position 0 31)) ""
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 24)
      let item = head $ filter ((== "OverloadedStrings") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "OverloadedStrings"
        item ^. L.kind @?= Just CiKeyword

  , testCase "completes the Strict language extension" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      _ <- waitForDiagnostics
      let te = TextEdit (Range (Position 0 13) (Position 0 31)) "Str"
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 16)
      let item = head $ filter ((== "Strict") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "Strict"
        item ^. L.kind @?= Just CiKeyword

  , testCase "completes No- language extensions" $ runSessionWithServer pragmasPlugin testDataDir $ do
      doc <- openDoc "Completion.hs" "haskell"
      _ <- waitForDiagnostics
      let te = TextEdit (Range (Position 0 13) (Position 0 31)) "NoOverload"
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 23)
      let item = head $ filter ((== "NoOverloadedStrings") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "NoOverloadedStrings"
        item ^. L.kind @?= Just CiKeyword
  ]

goldenWithPragmas :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithPragmas title path = goldenWithHaskellDoc pragmasPlugin title testDataDir path "expected" "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
