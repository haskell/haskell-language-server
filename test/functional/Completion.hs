{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Completion(tests) where

import Control.Applicative.Combinators
import Control.Monad.IO.Class
import Control.Lens hiding ((.=))
-- import Data.Aeson
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens hiding (applyEdit)
import Test.HIE
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Expectations


tests :: TestTree
tests = testGroup "completions" [
    testCase "works" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "put"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 9)
        let item = head $ filter ((== "putStrLn") . (^. label)) compls
        liftIO $ do
            item ^. label `shouldBe` "putStrLn"
            item ^. kind `shouldBe` Just CiFunction
            item ^. detail `shouldBe` Just "Prelude"
        resolvedRes <- request CompletionItemResolve item
        let Just (resolved :: CompletionItem) = resolvedRes ^. result
        liftIO $ do
            resolved ^. label `shouldBe` "putStrLn"
            resolved ^. kind `shouldBe` Just CiFunction
            resolved ^. detail `shouldBe` Just "String -> IO ()\nPrelude"
            resolved ^. insertTextFormat `shouldBe` Just Snippet
            resolved ^. insertText `shouldBe` Just "putStrLn ${1:String}"

    , testCase "completes imports" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 1 17) (Position 1 26)) "Data.M"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 1 22)
        let item = head $ filter ((== "Maybe") . (^. label)) compls
        liftIO $ do
            item ^. label `shouldBe` "Maybe"
            item ^. detail `shouldBe` Just "Data.Maybe"
            item ^. kind `shouldBe` Just CiModule

    , testCase "completes qualified imports" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 2 17) (Position 1 25)) "Dat"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 1 19)
        let item = head $ filter ((== "Data.List") . (^. label)) compls
        liftIO $ do
            item ^. label `shouldBe` "Data.List"
            item ^. detail `shouldBe` Just "Data.List"
            item ^. kind `shouldBe` Just CiModule

    , testCase "completes language extensions" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 0 24) (Position 0 31)) ""
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 0 24)
        let item = head $ filter ((== "OverloadedStrings") . (^. label)) compls
        liftIO $ do
            item ^. label `shouldBe` "OverloadedStrings"
            item ^. kind `shouldBe` Just CiKeyword

    , testCase "completes pragmas" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 0 4) (Position 0 34)) ""
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 0 4)
        let item = head $ filter ((== "LANGUAGE") . (^. label)) compls
        liftIO $ do
            item ^. label `shouldBe` "LANGUAGE"
            item ^. kind `shouldBe` Just CiKeyword
            item ^. insertTextFormat `shouldBe` Just Snippet
            item ^. insertText `shouldBe` Just "LANGUAGE ${1:extension} #-}"

    , testCase "completes pragmas no close" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 0 4) (Position 0 24)) ""
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 0 4)
        let item = head $ filter ((== "LANGUAGE") . (^. label)) compls
        liftIO $ do
            item ^. label `shouldBe` "LANGUAGE"
            item ^. kind `shouldBe` Just CiKeyword
            item ^. insertTextFormat `shouldBe` Just Snippet
            item ^. insertText `shouldBe` Just "LANGUAGE ${1:extension}"

    , testCase "completes options pragma" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 0 4) (Position 0 34)) "OPTIONS"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 0 4)
        let item = head $ filter ((== "OPTIONS_GHC") . (^. label)) compls
        liftIO $ do
            item ^. label `shouldBe` "OPTIONS_GHC"
            item ^. kind `shouldBe` Just CiKeyword
            item ^. insertTextFormat `shouldBe` Just Snippet
            item ^. insertText `shouldBe` Just "OPTIONS_GHC -${1:option} #-}"

  -- -----------------------------------

    , testCase "completes ghc options pragma values" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 0 0) (Position 0 0)) "{-# OPTIONS_GHC -Wno-red  #-}\n"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 0 24)
        let item = head $ filter ((== "Wno-redundant-constraints") . (^. label)) compls
        liftIO $ do
            item ^. label `shouldBe` "Wno-redundant-constraints"
            item ^. kind `shouldBe` Just CiKeyword
            item ^. insertTextFormat `shouldBe` Nothing
            item ^. insertText `shouldBe` Nothing

  -- -----------------------------------

    , testCase "completes with no prefix" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics
        compls <- getCompletions doc (Position 5 7)
        liftIO $ filter ((== "!!") . (^. label)) compls `shouldNotSatisfy` null

    -- See https://github.com/haskell/haskell-ide-engine/issues/903
    , testCase "strips compiler generated stuff from completions" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "DupRecFields.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 5 0) (Position 5 2)) "acc"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 4)
        let item = head $ filter (\c -> c^.label == "accessor") compls
        liftIO $ do
            item ^. label `shouldBe` "accessor"
            item ^. kind `shouldBe` Just CiFunction
            item ^. detail `shouldBe` Just "Two -> Int\nDupRecFields"
            item ^. insertText `shouldBe` Just "accessor ${1:Two}"

    , testCase "have implicit foralls on basic polymorphic types" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics
        let te = TextEdit (Range (Position 5 7) (Position 5 9)) "id"
        _ <- applyEdit doc te
        compls <- getCompletions doc (Position 5 9)
        let item = head $ filter ((== "id") . (^. label)) compls
        resolvedRes <- request CompletionItemResolve item
        let Just (resolved :: CompletionItem) = resolvedRes ^. result
        liftIO $
            resolved ^. detail `shouldBe` Just "a -> a\nPrelude"

    , testCase "have implicit foralls with multiple type variables" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics
        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "flip"
        _ <- applyEdit doc te
        compls <- getCompletions doc (Position 5 11)
        let item = head $ filter ((== "flip") . (^. label)) compls
        resolvedRes <- request CompletionItemResolve item
        let Just (resolved :: CompletionItem) = resolvedRes ^. result
        liftIO $
            resolved ^. detail `shouldBe` Just "(a -> b -> c) -> b -> a -> c\nPrelude"

    , contextTests
    , snippetTests
    ]

snippetTests :: TestTree
snippetTests = testGroup "snippets" [
    testCase "work for argumentless constructors" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "Nothing"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 14)
        let item = head $ filter ((== "Nothing") . (^. label)) compls
        liftIO $ do
            item ^. insertTextFormat `shouldBe` Just Snippet
            item ^. insertText `shouldBe` Just "Nothing"

    , testCase "work for polymorphic types" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "fold"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 11)
        let item = head $ filter ((== "foldl") . (^. label)) compls
        resolvedRes <- request CompletionItemResolve item
        let Just (resolved :: CompletionItem) = resolvedRes ^. result
        liftIO $ do
            resolved ^. label `shouldBe` "foldl"
            resolved ^. kind `shouldBe` Just CiFunction
            resolved ^. insertTextFormat `shouldBe` Just Snippet
            resolved ^. insertText `shouldBe` Just "foldl ${1:b -> a -> b} ${2:b} ${3:t a}"

    , testCase "work for complex types" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "mapM"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 11)
        let item = head $ filter ((== "mapM") . (^. label)) compls
        resolvedRes <- request CompletionItemResolve item
        let Just (resolved :: CompletionItem) = resolvedRes ^. result
        liftIO $ do
            resolved ^. label `shouldBe` "mapM"
            resolved ^. kind `shouldBe` Just CiFunction
            resolved ^. insertTextFormat `shouldBe` Just Snippet
            resolved ^. insertText `shouldBe` Just "mapM ${1:a -> m b} ${2:t a}"

    , testCase "work for infix functions" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "even `filte"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 18)
        let item = head $ filter ((== "filter") . (^. label)) compls
        liftIO $ do
            item ^. label `shouldBe` "filter"
            item ^. kind `shouldBe` Just CiFunction
            item ^. insertTextFormat `shouldBe` Just Snippet
            item ^. insertText `shouldBe` Just "filter`"

    , testCase "work for infix functions in backticks" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "even `filte`"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 18)
        let item = head $ filter ((== "filter") . (^. label)) compls
        liftIO $ do
            item ^. label `shouldBe` "filter"
            item ^. kind `shouldBe` Just CiFunction
            item ^. insertTextFormat `shouldBe` Just Snippet
            item ^. insertText `shouldBe` Just "filter"

    , testCase "work for qualified infix functions" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "\"\" `Data.List.interspe"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 29)
        let item = head $ filter ((== "intersperse") . (^. label)) compls
        liftIO $ do
            item ^. label `shouldBe` "intersperse"
            item ^. kind `shouldBe` Just CiFunction
            item ^. insertTextFormat `shouldBe` Just Snippet
            item ^. insertText `shouldBe` Just "intersperse`"

    , testCase "work for qualified infix functions in backticks" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "\"\" `Data.List.interspe`"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 29)
        let item = head $ filter ((== "intersperse") . (^. label)) compls
        liftIO $ do
            item ^. label `shouldBe` "intersperse"
            item ^. kind `shouldBe` Just CiFunction
            item ^. insertTextFormat `shouldBe` Just Snippet
            item ^. insertText `shouldBe` Just "intersperse"

    -- -- TODO : Fix compile issue in the test "Variable not in scope: object"
    -- , testCase "respects lsp configuration" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
    --     doc <- openDoc "Completion.hs" "haskell"
    --     _   <- count 2 $ skipManyTill loggingNotification noDiagnostics

    --     let config = object [ "languageServerHaskell" .= (object ["completionSnippetsOn" .= False])]

    --     sendNotification WorkspaceDidChangeConfiguration
    --                     (DidChangeConfigurationParams config)

    --     checkNoSnippets doc

    , testCase "respects client capabilities" $ runSession hieCommand noSnippetsCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"
        _ <- count 2 $ skipManyTill loggingNotification noDiagnostics

        checkNoSnippets doc
    ]
    where
        checkNoSnippets doc = do
            let te = TextEdit (Range (Position 5 7) (Position 5 24)) "fold"
            _      <- applyEdit doc te

            compls <- getCompletions doc (Position 5 11)
            let item = head $ filter ((== "foldl") . (^. label)) compls
            liftIO $ do
                item ^. label `shouldBe` "foldl"
                item ^. kind `shouldBe` Just CiFunction
                item ^. insertTextFormat `shouldBe` Just PlainText
                item ^. insertText `shouldBe` Nothing

            resolvedRes <- request CompletionItemResolve item
            let Just (resolved :: CompletionItem) = resolvedRes ^. result
            liftIO $ do
                resolved ^. label `shouldBe` "foldl"
                resolved ^. kind `shouldBe` Just CiFunction
                resolved ^. insertTextFormat `shouldBe` Just PlainText
                resolved ^. insertText `shouldBe` Nothing

        noSnippetsCaps =
            (  textDocument
            .  _Just
            .  completion
            .  _Just
            .  completionItem
            .  _Just
            .  snippetSupport
            ?~ False
            )
            fullCaps

contextTests :: TestTree
contextTests = testGroup "contexts" [
    testCase "only provides type suggestions" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
      doc <- openDoc "Context.hs" "haskell"
      _ <- count 2 $ skipManyTill loggingNotification noDiagnostics
      compls <- getCompletions doc (Position 2 17)
      liftIO $ do
        compls `shouldContainCompl` "Integer"
        compls `shouldNotContainCompl` "interact"

    , testCase "only provides type suggestions" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
      doc <- openDoc "Context.hs" "haskell"
      _ <- count 2 $ skipManyTill loggingNotification noDiagnostics
      compls <- getCompletions doc (Position 3 9)
      liftIO $ do
        compls `shouldContainCompl` "abs"
        compls `shouldNotContainCompl` "Applicative"

    -- This currently fails if , testCase takes too long to typecheck the module
    -- , testCase "completes qualified type suggestions" $ runSession hieCommand fullCaps "test/testdata/completion" $ do
    --     doc <- openDoc "Context.hs" "haskell"
    --     _ <- count 2 $ skipManyTill loggingNotification noDiagnostics
    --     let te = TextEdit (Range (Position 2 17) (Position 2 17)) " -> Conc."
    --     _ <- applyEdit doc te
    --     compls <- getCompletions doc (Position 2 26)
    --     liftIO $ do
    --         compls `shouldNotContainCompl` "forkOn"
    --         compls `shouldContainCompl` "MVar"
    --         compls `shouldContainCompl` "Chan"
    ]
    where
        compls `shouldContainCompl` x  =
            filter ((== x) . (^. label)) compls `shouldNotSatisfy` null
        compls `shouldNotContainCompl` x =
            filter ((== x) . (^. label)) compls `shouldSatisfy` null