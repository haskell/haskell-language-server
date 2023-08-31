{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Completion(tests) where

import           Control.Lens               hiding ((.=))
import           Data.Aeson                 (toJSON)
import           Data.Foldable              (find)
import           Data.Functor               (void)
import qualified Data.Map                   as Map
import           Data.Row.Records           (focus)
import qualified Data.Text                  as T
import           Ide.Plugin.Config          (maxCompletions, plcConfig, plugins)
import           Language.LSP.Protocol.Lens hiding (applyEdit, length)
import           Test.Hls
import           Test.Hls.Command

tests :: TestTree
tests = testGroup "completions" [
     testCase "works" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         let te = TextEdit (Range (Position 5 7) (Position 5 24)) "put"
         _ <- applyEdit doc te

         compls <- getAndResolveCompletions doc (Position 5 9)
         item <- getCompletionByLabel "putStrLn" compls
         liftIO $ do
             item ^. label @?= "putStrLn"
             item ^. kind @?= Just CompletionItemKind_Function
             item ^. detail @?= Just ":: String -> IO ()\nfrom Prelude"
             item ^. insertTextFormat @?= Just InsertTextFormat_Snippet
             item ^. insertText @?= Just "putStrLn"

     , testCase "itemCompletion/resolve works" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         let te = TextEdit (Range (Position 5 7) (Position 5 24)) "put"
         _ <- applyEdit doc te

         compls <- getAndResolveCompletions doc (Position 5 9)
         item <- getCompletionByLabel "putStrLn" compls
         liftIO $ do
                 item ^. label @?= "putStrLn"
                 item ^. kind @?= Just CompletionItemKind_Function
                 item ^. detail @?= Just ":: String -> IO ()\nfrom Prelude"
                 item ^. insertTextFormat @?= Just InsertTextFormat_Snippet
                 item ^. insertText @?= Just "putStrLn"

     , testCase "completes imports" $ runSession (hlsCommand <> " --test") fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         waitForKickDone

         let te = TextEdit (Range (Position 1 17) (Position 1 26)) "Data.M"
         _ <- applyEdit doc te

         compls <- getAndResolveCompletions doc (Position 1 23)
         item <- getCompletionByLabel "Maybe" compls
         liftIO $ do
             item ^. label @?= "Maybe"
             item ^. detail @?= Just "Data.Maybe"
             item ^. kind @?= Just CompletionItemKind_Module

     , testCase "completes qualified imports" $ runSession (hlsCommand <> " --test") fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         _ <- waitForKickDone

         let te = TextEdit (Range (Position 2 17) (Position 2 25)) "Data.L"
         _ <- applyEdit doc te

         compls <- getAndResolveCompletions doc (Position 2 24)
         item <- getCompletionByLabel "List" compls
         liftIO $ do
             item ^. label @?= "List"
             item ^. detail @?= Just "Data.List"
             item ^. kind @?= Just CompletionItemKind_Module

     , testCase "completes with no prefix" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         compls <- getAndResolveCompletions doc (Position 5 7)
         liftIO $ assertBool "Expected completions" $ not $ null compls

     , expectFailIfBeforeGhc92 "record dot syntax is introduced in GHC 9.2"
       $ testGroup "recorddotsyntax"
        [ testCase "shows field selectors" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
            doc <- openDoc "RecordDotSyntax.hs" "haskell"

            let te = TextEdit (Range (Position 25 0) (Position 25 5)) "z = x.a"
            _ <- applyEdit doc te

            compls <- getAndResolveCompletions doc (Position 25 6)
            item <- getCompletionByLabel "a" compls

            liftIO $ do
                item ^. label @?= "a"
        , testCase "shows field selectors for nested field" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
            doc <- openDoc "RecordDotSyntax.hs" "haskell"

            let te = TextEdit (Range (Position 27 0) (Position 27 8)) "z2 = x.c.z"
            _ <- applyEdit doc te

            compls <- getAndResolveCompletions doc (Position 27 9)
            item <- getCompletionByLabel "z" compls

            liftIO $ do
                item ^. label @?= "z"
        ]

     -- See https://github.com/haskell/haskell-ide-engine/issues/903
     , testCase "strips compiler generated stuff from completions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "DupRecFields.hs" "haskell"

         let te = TextEdit (Range (Position 5 0) (Position 5 2)) "acc"
         _ <- applyEdit doc te

         compls <- getAndResolveCompletions doc (Position 5 4)
         item <- getCompletionByLabel "accessor" compls
         liftIO $ do
             item ^. label @?= "accessor"
             item ^. kind @?= Just CompletionItemKind_Function
     , testCase "have implicit foralls on basic polymorphic types" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         let te = TextEdit (Range (Position 5 7) (Position 5 9)) "id"
         _ <- applyEdit doc te
         compls <- getAndResolveCompletions doc (Position 5 9)
         item <- getCompletionByLabel "id" compls
         liftIO $ do
             item ^. detail @?= Just ":: a -> a\nfrom Prelude"

     , testCase "have implicit foralls with multiple type variables" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         let te = TextEdit (Range (Position 5 7) (Position 5 24)) "flip"
         _ <- applyEdit doc te
         compls <- getAndResolveCompletions doc (Position 5 11)
         item <- getCompletionByLabel "flip" compls
         liftIO $
             item ^. detail @?= Just ":: (a -> b -> c) -> b -> a -> c\nfrom Prelude"

     , testCase "maxCompletions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "Completion.hs" "haskell"

         compls <- getAndResolveCompletions doc (Position 5 7)
         liftIO $ length compls @?= maxCompletions def

     , testCase "import function completions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "FunctionCompletions.hs" "haskell"

         let te = TextEdit (Range (Position 0 30) (Position 0 41)) "A"
         _ <- applyEdit doc te

         compls <- getAndResolveCompletions doc (Position 0 31)
         item <- getCompletionByLabel "Alternative" compls
         liftIO $ do
             item ^. label @?= "Alternative"
             item ^. kind @?= Just CompletionItemKind_Function
             item ^. detail @?= Just "Control.Applicative"

    , testCase "import second function completion" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "FunctionCompletions.hs" "haskell"

         let te = TextEdit (Range (Position 0 39) (Position 0 39)) ", l"
         _ <- applyEdit doc te

         compls <- getAndResolveCompletions doc (Position 0 42)
         item <- getCompletionByLabel "liftA" compls
         liftIO $ do
             item ^. label @?= "liftA"
             item ^. kind @?= Just CompletionItemKind_Function
             item ^. detail @?= Just "Control.Applicative"

     , testCase "completes locally defined associated type family" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "AssociatedTypeFamily.hs" "haskell"

         compls <- getAndResolveCompletions doc (Position 5 20)
         item <- getCompletionByLabel "Fam" compls
         liftIO $ do
             item ^. label @?= "Fam"
             item ^. kind @?= Just CompletionItemKind_Struct

     , contextTests
     , snippetTests
    ]

snippetTests :: TestTree
snippetTests = testGroup "snippets" [
    testCase "work for argumentless constructors" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
      doc <- openDoc "Completion.hs" "haskell"

      let te = TextEdit (Range (Position 5 7) (Position 5 24)) "Nothing"
      _ <- applyEdit doc te

      compls <- getAndResolveCompletions doc (Position 5 14)
      item <- getCompletionByLabel "Nothing" compls
      liftIO $ do
          item ^. insertTextFormat @?= Just InsertTextFormat_Snippet
          item ^. insertText @?= Just "Nothing"

    , testCase "work for polymorphic types" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "fold"
        _ <- applyEdit doc te

        compls <- getAndResolveCompletions doc (Position 5 11)
        item <- getCompletionByLabel "foldl" compls
        liftIO $ do
            item ^. label @?= "foldl"
            item ^. kind @?= Just CompletionItemKind_Function
            item ^. insertTextFormat @?= Just InsertTextFormat_Snippet
            item ^. insertText @?= Just "foldl"

    , testCase "work for complex types" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "mapM"
        _ <- applyEdit doc te

        compls <- getAndResolveCompletions doc (Position 5 11)
        item <- getCompletionByLabel "mapM" compls
        liftIO $ do
            item ^. label @?= "mapM"
            item ^. kind @?= Just CompletionItemKind_Function
            item ^. insertTextFormat @?= Just InsertTextFormat_Snippet
            item ^. insertText @?= Just "mapM"

    , testCase "work for infix functions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "even `filte"
        _ <- applyEdit doc te

        compls <- getAndResolveCompletions doc (Position 5 18)
        item <- getCompletionByLabel "filter" compls
        liftIO $ do
            item ^. label @?= "filter"
            item ^. kind @?= Just CompletionItemKind_Function
            item ^. insertTextFormat @?= Just InsertTextFormat_PlainText
            item ^. insertText @?= Nothing

    , testCase "work for infix functions in backticks" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "even `filte`"
        _ <- applyEdit doc te

        compls <- getAndResolveCompletions doc (Position 5 18)
        item <- getCompletionByLabel "filter" compls
        liftIO $ do
            item ^. label @?= "filter"
            item ^. kind @?= Just CompletionItemKind_Function
            item ^. insertTextFormat @?= Just InsertTextFormat_PlainText
            item ^. insertText @?= Nothing

    , testCase "work for qualified infix functions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "\"\" `Data.List.interspe"
        _ <- applyEdit doc te

        compls <- getAndResolveCompletions doc (Position 5 29)
        item <- getCompletionByLabel "intersperse" compls
        liftIO $ do
            item ^. label @?= "intersperse"
            item ^. kind @?= Just CompletionItemKind_Function
            item ^. insertTextFormat @?= Just InsertTextFormat_PlainText
            item ^. insertText @?= Nothing

    , testCase "work for qualified infix functions in backticks" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        let te = TextEdit (Range (Position 5 7) (Position 5 24)) "\"\" `Data.List.interspe`"
        _ <- applyEdit doc te

        compls <- getAndResolveCompletions doc (Position 5 29)
        item <- getCompletionByLabel "intersperse" compls
        liftIO $ do
            item ^. label @?= "intersperse"
            item ^. kind @?= Just CompletionItemKind_Function
            item ^. insertTextFormat @?= Just InsertTextFormat_PlainText
            item ^. insertText @?= Nothing

    , testCase "respects lsp configuration" $ runSessionWithConfig (def {ignoreConfigurationRequests=False}) hlsCommand fullCaps "test/testdata/completion" $ do
        void configurationRequest
        doc <- openDoc "Completion.hs" "haskell"

        let config = def { plugins = Map.insert "ghcide-completions" (def { plcConfig = [("snippetsOn", (toJSON False))]}) (plugins def) }

        setHlsConfig config

        checkNoSnippets doc

    , testCase "respects client capabilities" $ runSession hlsCommand noSnippetsCaps "test/testdata/completion" $ do
        doc <- openDoc "Completion.hs" "haskell"

        checkNoSnippets doc
    , testCase "works for record fields sharing the single signature" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
         doc <- openDoc "FieldsSharingSignature.hs" "haskell"

         let te = TextEdit (Range (Position 1 0) (Position 1 2)) "MkF"
         _ <- applyEdit doc te

         compls <- getAndResolveCompletions doc (Position 1 6)
         item <- case find (\c -> (c ^. label == "MkFoo") && maybe False ("MkFoo {" `T.isPrefixOf`) (c ^. insertText)) compls of
            Just c -> pure c
            Nothing -> liftIO . assertFailure $ "Completion with label 'MkFoo' and insertText starting with 'MkFoo {' not found among " <> show compls
         liftIO $ do
            item ^. insertTextFormat @?= Just InsertTextFormat_Snippet
            item ^. insertText @?= Just "MkFoo {arg1=${1:_arg1}, arg2=${2:_arg2}, arg3=${3:_arg3}, arg4=${4:_arg4}, arg5=${5:_arg5}}"
    ]
    where
        checkNoSnippets doc = do
            let te = TextEdit (Range (Position 5 7) (Position 5 24)) "fold"
            _      <- applyEdit doc te

            compls <- getAndResolveCompletions doc (Position 5 11)
            item <- getCompletionByLabel "foldl" compls
            liftIO $ do
                item ^. label @?= "foldl"
                item ^. kind @?= Just CompletionItemKind_Function
                item ^. insertTextFormat @?= Just InsertTextFormat_PlainText
                item ^. insertText @?= Nothing

        noSnippetsCaps =
            (  textDocument
            .  _Just
            .  completion
            .  _Just
            .  completionItem
            .  _Just
            .  focus #snippetSupport
            ?~ False
            )
            fullCaps

contextTests :: TestTree
contextTests = testGroup "contexts" [
    testCase "only provides type suggestions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
      doc <- openDoc "Context.hs" "haskell"

      compls <- getAndResolveCompletions doc (Position 2 17)
      liftIO $ do
        compls `shouldContainCompl` "Integer"
        compls `shouldNotContainCompl` "interact"

    , testCase "only provides value suggestions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
      doc <- openDoc "Context.hs" "haskell"

      compls <- getAndResolveCompletions doc (Position 3 10)
      liftIO $ do
        compls `shouldContainCompl` "abs"
        compls `shouldNotContainCompl` "Applicative"

    , testCase "completes qualified type suggestions" $ runSession hlsCommand fullCaps "test/testdata/completion" $ do
        doc <- openDoc "Context.hs" "haskell"

        compls <- getAndResolveCompletions doc (Position 2 26)
        liftIO $ do
            compls `shouldNotContainCompl` "forkOn"
            compls `shouldContainCompl` "MVar"
            compls `shouldContainCompl` "Chan"
    ]

shouldContainCompl :: [CompletionItem] -> T.Text -> Assertion
compls `shouldContainCompl` lbl  =
    any ((== lbl) . (^. label)) compls
    @? "Should contain completion: " ++ show lbl

shouldNotContainCompl :: [CompletionItem] -> T.Text -> Assertion
compls `shouldNotContainCompl` lbl =
    all ((/= lbl) . (^. label)) compls
    @? "Should not contain completion: " ++ show lbl

expectFailIfBeforeGhc92 :: String -> TestTree -> TestTree
expectFailIfBeforeGhc92 = knownBrokenForGhcVersions [GHC90]
