{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Completion(tests) where

import Control.Monad.IO.Class
import Control.Lens hiding ((.=))
import Data.Aeson (object, (.=))
import Language.Haskell.LSP.Test
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens hiding (applyEdit)
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit
import qualified Data.Text as T
import Data.Default (def)
import Ide.Plugin.Config (Config (maxCompletions))

tests :: TestTree
tests = testGroup "completions" [
     testCase "works" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         let te = TextEdit (Range (Position 5 7) (Position 5 24)) "put"
         _ <- applyEdit doc te

         compls <- getCompletions doc (Position 5 9)
         let item = head $ filter ((== "putStrLn") . (^. label)) compls
         liftIO $ do
             item ^. label @?= "putStrLn"
             item ^. kind @?= Just CiFunction
             item ^. detail @?= Just ":: String -> IO ()"
             item ^. insertTextFormat @?= Just Snippet
             item ^. insertText @?= Just "putStrLn ${1:String}"

     , ignoreTestBecause "no support for itemCompletion/resolve requests"
       $ testCase "itemCompletion/resolve works" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         let te = TextEdit (Range (Position 5 7) (Position 5 24)) "put"
         _ <- applyEdit doc te

         compls <- getCompletions doc (Position 5 9)
         let item = head $ filter ((== "putStrLn") . (^. label)) compls
         resolvedRes <- request CompletionItemResolve item
         let Right (resolved :: CompletionItem) = resolvedRes ^. result
         liftIO $ print resolved
         liftIO $ do
             resolved ^. label @?= "putStrLn"
             resolved ^. kind @?= Just CiFunction
             resolved ^. detail @?= Just "String -> IO ()\nPrelude"
             resolved ^. insertTextFormat @?= Just Snippet
             resolved ^. insertText @?= Just "putStrLn ${1:String}"

     , testCase "completes imports" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         _ <- waitForDiagnostics

         let te = TextEdit (Range (Position 1 17) (Position 1 26)) "Data.M"
         _ <- applyEdit doc te

         compls <- getCompletions doc (Position 1 23)
         let item = head $ filter ((== "Maybe") . (^. label)) compls
         liftIO $ do
             item ^. label @?= "Maybe"
             item ^. detail @?= Just "Data.Maybe"
             item ^. kind @?= Just CiModule

     , testCase "completes qualified imports" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         _ <- waitForDiagnostics

         let te = TextEdit (Range (Position 2 17) (Position 2 25)) "Data.L"
         _ <- applyEdit doc te

         compls <- getCompletions doc (Position 2 24)
         let item = head $ filter ((== "List") . (^. label)) compls
         liftIO $ do
             item ^. label @?= "List"
             item ^. detail @?= Just "Data.List"
             item ^. kind @?= Just CiModule

     , testCase "completes language extensions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         _ <- waitForDiagnostics

         let te = TextEdit (Range (Position 0 24) (Position 0 31)) ""
         _ <- applyEdit doc te

         compls <- getCompletions doc (Position 0 24)
         let item = head $ filter ((== "OverloadedStrings") . (^. label)) compls
         liftIO $ do
             item ^. label @?= "OverloadedStrings"
             item ^. kind @?= Just CiKeyword

     , testCase "completes the Strict language extension" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         _ <- waitForDiagnostics

         let te = TextEdit (Range (Position 0 13) (Position 0 31)) "Str"
         _ <- applyEdit doc te

         compls <- getCompletions doc (Position 0 16)
         let item = head $ filter ((== "Strict") . (^. label)) compls
         liftIO $ do
             item ^. label @?= "Strict"
             item ^. kind @?= Just CiKeyword

     , testCase "completes No- language extensions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         _ <- waitForDiagnostics

         let te = TextEdit (Range (Position 0 13) (Position 0 31)) "NoOverload"
         _ <- applyEdit doc te

         compls <- getCompletions doc (Position 0 23)
         let item = head $ filter ((== "NoOverloadedStrings") . (^. label)) compls
         liftIO $ do
             item ^. label @?= "NoOverloadedStrings"
             item ^. kind @?= Just CiKeyword

     , testCase "completes pragmas" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         _ <- waitForDiagnostics

         let te = TextEdit (Range (Position 0 4) (Position 0 34)) ""
         _ <- applyEdit doc te

         compls <- getCompletions doc (Position 0 4)
         let item = head $ filter ((== "LANGUAGE") . (^. label)) compls
         liftIO $ do
             item ^. label @?= "LANGUAGE"
             item ^. kind @?= Just CiKeyword
             item ^. insertTextFormat @?= Just Snippet
             item ^. insertText @?= Just "LANGUAGE ${1:extension} #-}"

     , testCase "completes pragmas no close" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         let te = TextEdit (Range (Position 0 4) (Position 0 24)) ""
         _ <- applyEdit doc te

         compls <- getCompletions doc (Position 0 4)
         let item = head $ filter ((== "LANGUAGE") . (^. label)) compls
         liftIO $ do
             item ^. label @?= "LANGUAGE"
             item ^. kind @?= Just CiKeyword
             item ^. insertTextFormat @?= Just Snippet
             item ^. insertText @?= Just "LANGUAGE ${1:extension}"

     , testCase "completes options pragma" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         _ <- waitForDiagnostics

         let te = TextEdit (Range (Position 0 4) (Position 0 34)) "OPTIONS"
         _ <- applyEdit doc te

         compls <- getCompletions doc (Position 0 4)
         let item = head $ filter ((== "OPTIONS_GHC") . (^. label)) compls
         liftIO $ do
             item ^. label @?= "OPTIONS_GHC"
             item ^. kind @?= Just CiKeyword
             item ^. insertTextFormat @?= Just Snippet
             item ^. insertText @?= Just "OPTIONS_GHC -${1:option} #-}"

     , testCase "completes ghc options pragma values" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         let te = TextEdit (Range (Position 0 0) (Position 0 0)) "{-# OPTIONS_GHC -Wno-red  #-}\n"
         _ <- applyEdit doc te

         compls <- getCompletions doc (Position 0 24)
         let item = head $ filter ((== "Wno-redundant-constraints") . (^. label)) compls
         liftIO $ do
             item ^. label @?= "Wno-redundant-constraints"
             item ^. kind @?= Just CiKeyword
             item ^. insertTextFormat @?= Nothing
             item ^. insertText @?= Nothing

     , testCase "completes with no prefix" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         compls <- getCompletions doc (Position 5 7)
         liftIO $ assertBool "Expected completions" $ not $ null compls

     -- See https://github.com/haskell/haskell-ide-engine/issues/903
     , testCase "strips compiler generated stuff from completions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "DupRecFields.hs" "haskell"

         let te = TextEdit (Range (Position 5 0) (Position 5 2)) "acc"
         _ <- applyEdit doc te

         compls <- getCompletions doc (Position 5 4)
         let item = head $ filter (\c -> c^.label == "accessor") compls
         liftIO $ do
             item ^. label @?= "accessor"
             item ^. kind @?= Just CiFunction

     , testCase "have implicit foralls on basic polymorphic types" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         let te = TextEdit (Range (Position 5 7) (Position 5 9)) "id"
         _ <- applyEdit doc te
         compls <- getCompletions doc (Position 5 9)
         let item = head $ filter ((== "id") . (^. label)) compls
         liftIO $ do
             item ^. detail @?= Just ":: a -> a"

     , testCase "have implicit foralls with multiple type variables" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         let te = TextEdit (Range (Position 5 7) (Position 5 24)) "flip"
         _ <- applyEdit doc te
         compls <- getCompletions doc (Position 5 11)
         let item = head $ filter ((== "flip") . (^. label)) compls
         liftIO $
             item ^. detail @?= Just ":: (a -> b -> c) -> b -> a -> c"

     , testCase "maxCompletions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         compls <- getCompletions doc (Position 5 7)
         liftIO $ length compls @?= maxCompletions def

     , contextTests
     , snippetTests
    ]

snippetTests :: TestTree
snippetTests = testGroup "snippets" [
    testCase "work for argumentless constructors" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
      doc <- openDoc "Completion.hs" "haskell"

      let te = TextEdit (Range (Position 5 7) (Position 5 24)) "Nothing"
      _ <- applyEdit doc te

      compls <- getCompletions doc (Position 5 14)
      let item = head $ filter ((== "Nothing") . (^. label)) compls
      liftIO $ do
          item ^. insertTextFormat @?= Just Snippet
          item ^. insertText @?= Just "Nothing "

    , testCase "work for polymorphic types" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "fold"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 11)
        let item = head $ filter ((== "foldl") . (^. label)) compls
        liftIO $ do
            item ^. label @?= "foldl"
            item ^. kind @?= Just CiFunction
            item ^. insertTextFormat @?= Just Snippet
            item ^. insertText @?= Just "foldl ${1:b -> a -> b} ${2:b} ${3:t a}"

    , testCase "work for complex types" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "mapM"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 11)
        let item = head $ filter ((== "mapM") . (^. label)) compls
        liftIO $ do
            item ^. label @?= "mapM"
            item ^. kind @?= Just CiFunction
            item ^. insertTextFormat @?= Just Snippet
            item ^. insertText @?= Just "mapM ${1:a -> m b} ${2:t a}"

    , testCase "work for infix functions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "even `filte"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 18)
        let item = head $ filter ((== "filter") . (^. label)) compls
        liftIO $ do
            item ^. label @?= "filter"
            item ^. kind @?= Just CiFunction
            item ^. insertTextFormat @?= Just PlainText
            item ^. insertText @?= Nothing

    , testCase "work for infix functions in backticks" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "even `filte`"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 18)
        let item = head $ filter ((== "filter") . (^. label)) compls
        liftIO $ do
            item ^. label @?= "filter"
            item ^. kind @?= Just CiFunction
            item ^. insertTextFormat @?= Just PlainText
            item ^. insertText @?= Nothing

    , testCase "work for qualified infix functions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "\"\" `Data.List.interspe"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 29)
        let item = head $ filter ((== "intersperse") . (^. label)) compls
        liftIO $ do
            item ^. label @?= "intersperse"
            item ^. kind @?= Just CiFunction
            item ^. insertTextFormat @?= Just PlainText
            item ^. insertText @?= Nothing

    , testCase "work for qualified infix functions in backticks" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "\"\" `Data.List.interspe`"
        _ <- applyEdit doc te

        compls <- getCompletions doc (Position 5 29)
        let item = head $ filter ((== "intersperse") . (^. label)) compls
        liftIO $ do
            item ^. label @?= "intersperse"
            item ^. kind @?= Just CiFunction
            item ^. insertTextFormat @?= Just PlainText
            item ^. insertText @?= Nothing

    , testCase "respects lsp configuration" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let config = object [ "haskell" .= object ["completionSnippetsOn" .= False]]

        sendNotification WorkspaceDidChangeConfiguration
                        (DidChangeConfigurationParams config)

        checkNoSnippets doc

    , testCase "respects client capabilities" $ runSession hlsCommand noSnippetsCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        checkNoSnippets doc
    ]
    where
        checkNoSnippets doc = do
            let te = TextEdit (Range (Position 5 7) (Position 5 24)) "fold"
            _      <- applyEdit doc te

            compls <- getCompletions doc (Position 5 11)
            let item = head $ filter ((== "foldl") . (^. label)) compls
            liftIO $ do
                item ^. label @?= "foldl"
                item ^. kind @?= Just CiFunction
                item ^. insertTextFormat @?= Just PlainText
                item ^. insertText @?= Nothing

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
    testCase "only provides type suggestions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
      doc <- openDoc "Context.hs" "haskell"

      compls <- getCompletions doc (Position 2 17)
      liftIO $ do
        compls `shouldContainCompl` "Integer"
        compls `shouldNotContainCompl` "interact"

    , testCase "only provides value suggestions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
      doc <- openDoc "Context.hs" "haskell"

      compls <- getCompletions doc (Position 3 10)
      liftIO $ do
        compls `shouldContainCompl` "abs"
        compls `shouldNotContainCompl` "Applicative"

    , testCase "completes qualified type suggestions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Context.hs" "haskell"

        compls <- getCompletions doc (Position 2 26)
        liftIO $ do
            compls `shouldNotContainCompl` "forkOn"
            compls `shouldContainCompl` "MVar"
            compls `shouldContainCompl` "Chan"
    ]

shouldContainCompl :: [CompletionItem] -> T.Text -> Assertion
compls `shouldContainCompl` x  =
    any ((== x) . (^. label)) compls
    @? "Should contain completion: " ++ show x

shouldNotContainCompl :: [CompletionItem] -> T.Text -> Assertion
compls `shouldNotContainCompl` x =
    all ((/= x) . (^. label)) compls
    @? "Should not contain completion: " ++ show x
