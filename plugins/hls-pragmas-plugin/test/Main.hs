{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Main (
    main,
) where

import           Control.Lens            ((^.))
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
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
  [ pragmasGolden "adds TypeSynonymInstances pragma" "NeedsPragmas" $ \path -> do
      doc <- openDoc path "haskell"
      _ <- waitForDiagnosticsFromSource doc "typecheck"
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Add \"TypeSynonymInstances\"" `elem` map (^. L.title) cas @? "Contains TypeSynonymInstances code action"
      liftIO $ "Add \"FlexibleInstances\"" `elem` map (^. L.title) cas @? "Contains FlexibleInstances code action"
      executeCodeAction $ head cas
      documentContents doc

  , pragmasGolden "adds TypeApplications pragma" "TypeApplications" $ \path -> do
      doc <- openDoc path "haskell"
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Add \"TypeApplications\"" `elem` map (^. L.title) cas @? "Contains TypeApplications code action"
      executeCodeAction $ head cas
      documentContents doc

  , pragmasGolden "no duplication" "NamedFieldPuns" $ \path -> do
      doc <- openDoc path "haskell"
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getCodeActions doc (Range (Position 8 9) (Position 8 9))
      liftIO $ length cas == 1 @? "Expected one code action, but got: " <> show cas
      let ca = head cas
      liftIO $ (ca ^. L.title == "Add \"NamedFieldPuns\"") @? "NamedFieldPuns code action"
      executeCodeAction ca
      documentContents doc

  , pragmasGolden "after shebang" "AfterShebang" $ \path -> do
      doc <- openDoc path "haskell"
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Add \"NamedFieldPuns\"" `elem` map (^. L.title) cas @? "Contains NamedFieldPuns code action"
      executeCodeAction $ head cas
      documentContents doc

  , pragmasGolden "append to existing pragmas" "AppendToExisting" $ \path -> do
      doc <- openDoc path "haskell"
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Add \"NamedFieldPuns\"" `elem` map (^. L.title) cas @? "Contains NamedFieldPuns code action"
      executeCodeAction $ head cas
      documentContents doc

  , pragmasGolden "before doc comments" "BeforeDocComment" $ \path -> do
      doc <- openDoc path "haskell"
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Add \"NamedFieldPuns\"" `elem` map (^. L.title) cas @? "Contains NamedFieldPuns code action"
      executeCodeAction $ head cas
      documentContents doc

  , pragmasGolden "before doc comments" "MissingSignatures" $ \path -> do
      doc <- openDoc path "haskell"
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Disable \"missing-signatures\" warnings" `elem` map (^. L.title) cas @? "Contains missing-signatures code action"
      executeCodeAction $ head cas
      documentContents doc

  , pragmasGolden "before doc comments" "UnusedImports" $ \path -> do
      doc <- openDoc path "haskell"
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Disable \"unused-imports\" warnings" `elem` map (^. L.title) cas @? "Contains unused-imports code action"
      executeCodeAction $ head cas
      documentContents doc
  ]

completionTests :: TestTree
completionTests =
  testGroup "completions"
  [ testCase "completes pragmas" $ runSessionWithServer pragmasPlugin testDirectory $ do
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

  , testCase "completes pragmas no close" $ runSessionWithServer pragmasPlugin testDirectory $ do
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

  , testCase "completes options pragma" $ runSessionWithServer pragmasPlugin testDirectory $ do
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

  , testCase "completes ghc options pragma values" $ runSessionWithServer pragmasPlugin testDirectory $ do
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

  , testCase "completes language extensions" $ runSessionWithServer pragmasPlugin testDirectory $ do
      doc <- openDoc "Completion.hs" "haskell"
      _ <- waitForDiagnostics
      let te = TextEdit (Range (Position 0 24) (Position 0 31)) ""
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 24)
      let item = head $ filter ((== "OverloadedStrings") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "OverloadedStrings"
        item ^. L.kind @?= Just CiKeyword

  , testCase "completes the Strict language extension" $ runSessionWithServer pragmasPlugin testDirectory $ do
      doc <- openDoc "Completion.hs" "haskell"
      _ <- waitForDiagnostics
      let te = TextEdit (Range (Position 0 13) (Position 0 31)) "Str"
      _ <- applyEdit doc te
      compls <- getCompletions doc (Position 0 16)
      let item = head $ filter ((== "Strict") . (^. L.label)) compls
      liftIO $ do
        item ^. L.label @?= "Strict"
        item ^. L.kind @?= Just CiKeyword

  , testCase "completes No- language extensions" $ runSessionWithServer pragmasPlugin testDirectory $ do
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

pragmasGolden :: TestName -> FilePath -> (FilePath -> Session T.Text) -> TestTree
pragmasGolden title path action =
  goldenGitDiff title (testDirectory </> path <.> "expected.hs")
  $ runSessionWithServer pragmasPlugin testDirectory
  $ TL.encodeUtf8 . TL.fromStrict
  <$> action (path <.> "hs")

testDirectory :: FilePath
testDirectory = "test" </> "testdata"
