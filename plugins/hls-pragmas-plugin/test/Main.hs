{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main
  ( main
  ) where

import           Control.Lens            ((<&>), (^.))
import qualified Data.Text               as T
import           Ide.Plugin.Pragmas
import qualified Language.LSP.Types.Lens as L
import           System.FilePath
import           Test.Hls
import           Test.Hls.Util           (onlyWorkForGhcVersions)

main :: IO ()
main = defaultTestRunner tests

pragmasPlugin :: PluginTestDescriptor ()
pragmasPlugin = mkPluginTestDescriptor' descriptor "pragmas"

tests :: TestTree
tests =
  testGroup "pragmas"
  [ codeActionTests
  , codeActionTests'
  , completionTests
  , completionSnippetTests
  ]

codeActionTests :: TestTree
codeActionTests =
  testGroup "code actions"
  [ codeActionTest "Block comment then line comment doesn't split line" "BlockCommentThenLineComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Block comment then single-line block comment doesn't split line" "BlockCommentThenSingleLineBlockComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Block comment then multi-line block comment doesn't split line" "BlockCommentThenMultiLineBlockComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Block comment then line haddock splits line" "BlockCommentThenLineHaddock" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Block comment then single-line block haddock splits line" "BlockCommentThenSingleLineBlockHaddock" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Block comment then multi-line block haddock splits line" "BlockCommentThenMultiLineBlockHaddock" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Pragma then line comment doesn't split line" "PragmaThenLineComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Pragma then single-line block comment doesn't split line" "PragmaThenSingleLineBlockComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Pragma then multi-line block comment splits line" "PragmaThenMultiLineBlockComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Pragma then line haddock splits line" "PragmaThenLineHaddock" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Pragma then single-line block haddock splits line" "PragmaThenSingleLineBlockHaddock" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Pragma then multi-line block haddock splits line" "PragmaThenMultiLineBlockHaddock" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Pragma then single-line block haddock single-line block comment splits line" "PragmaThenSingleLineBlockHaddockSingleLineBlockComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Block comment then single-line block haddock single-line block comment splits line" "BlockCommentThenSingleLineBlockHaddockSingleLineBlockComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Pragma then line haddock then newline line comment splits line" "PragmaThenLineHaddockNewlineLineComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "does not add pragma after OPTIONS_GHC pragma located after a declaration" "OptionsGhcAfterDecl" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "adds LANGUAGE with no other pragmas at start ignoring later INLINE pragma" "AddPragmaIgnoreInline" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "adds LANGUAGE before Doc comments after interchanging pragmas" "BeforeDocInterchanging" [("Add \"NamedFieldPuns\"", "Contains NamedFieldPuns code action")]
  , codeActionTest "Add language after altering OPTIONS_GHC and Language" "AddLanguagePragmaAfterInterchaningOptsGhcAndLangs" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "Add language after pragmas with non standard space between prefix and name" "AddPragmaWithNonStandardSpacingInPrecedingPragmas" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "adds LANGUAGE after OptGHC at start ignoring later INLINE pragma" "AddPragmaAfterOptsGhcIgnoreInline" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "adds LANGUAGE ignore later Ann pragma" "AddPragmaIgnoreLaterAnnPragma" [("Add \"BangPatterns\"", "Contains BangPatterns code action")]
  , codeActionTest "adds LANGUAGE after interchanging pragmas ignoring later Ann pragma" "AddLanguageAfterInterchaningIgnoringLaterAnn" [("Add \"BangPatterns\"", "Contains BangPatterns code action")]
  , codeActionTest "adds LANGUAGE after OptGHC preceded by another language pragma" "AddLanguageAfterLanguageThenOptsGhc" [("Add \"NamedFieldPuns\"", "Contains NamedFieldPuns code action")]
  , codeActionTest "adds LANGUAGE pragma after shebang and last language pragma" "AfterShebangAndPragma" [("Add \"NamedFieldPuns\"", "Contains NamedFieldPuns code action")]
  , codeActionTest "adds above module keyword on first line" "ModuleOnFirstLine" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "adds LANGUAGE pragma after GHC_OPTIONS" "AfterGhcOptions" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "adds LANGUAGE pragma after shebang and GHC_OPTIONS" "AfterShebangAndOpts" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "adds LANGUAGE pragma after shebang, GHC_OPTIONS and language pragma" "AfterShebangAndOptionsAndPragma" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "adds LANGUAGE pragma after all others ignoring later INLINE pragma" "AfterShebangAndOptionsAndPragmasIgnoreInline" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "adds LANGUAGE pragma after all others ignoring multiple later INLINE pragma" "AfterAllWithMultipleInlines" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "adds LANGUAGE pragma correctly ignoring later INLINE pragma" "AddLanguagePragma" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTest "adds TypeApplications pragma" "TypeApplications" [("Add \"TypeApplications\"", "Contains TypeApplications code action")]
  , codeActionTest "after shebang" "AfterShebang" [("Add \"NamedFieldPuns\"", "Contains NamedFieldPuns code action")]
  , codeActionTest "append to existing pragmas" "AppendToExisting" [("Add \"NamedFieldPuns\"", "Contains NamedFieldPuns code action")]
  , codeActionTest "before doc comments" "BeforeDocComment" [("Add \"NamedFieldPuns\"", "Contains NamedFieldPuns code action")]
  , codeActionTest "before doc comments" "MissingSignatures" [("Disable \"missing-signatures\" warnings", "Contains missing-signatures code action")]
  , codeActionTest "before doc comments" "UnusedImports" [("Disable \"unused-imports\" warnings", "Contains unused-imports code action")]
  , codeActionTest "adds TypeSynonymInstances pragma" "NeedsPragmas" [("Add \"TypeSynonymInstances\"", "Contains TypeSynonymInstances code action"), ("Add \"FlexibleInstances\"", "Contains FlexibleInstances code action")]
  ]

ghc94regression :: String
ghc94regression = "to be reported"

codeActionTest :: String -> FilePath -> [(T.Text, String)] -> TestTree
codeActionTest testComment fp actions =
  goldenWithPragmas testComment fp $ \doc -> do
    _ <- waitForDiagnosticsFrom doc
    cas <- map fromAction <$> getAllCodeActions doc
    mapM_ (\(action, contains) -> go action contains cas) actions
    action <- case cas of
      (a:_) -> pure a
      []    -> liftIO $ assertFailure "Expected non-empty list of code actions"
    executeCodeAction action
    where
      go action contains cas = liftIO $ action `elem` map (^. L.title) cas @? contains

codeActionTests' :: TestTree
codeActionTests' =
  testGroup "additional code actions"
  [
 goldenWithPragmas "no duplication" "NamedFieldPuns" $ \doc -> do
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getCodeActions doc (Range (Position 8 9) (Position 8 9))
      ca <- liftIO $ case cas of
        [ca] -> pure ca
        _ -> assertFailure $ "Expected one code action, but got: " <> show cas
      liftIO $ (ca ^. L.title == "Add \"NamedFieldPuns\"") @? "NamedFieldPuns code action"
      executeCodeAction ca
  , goldenWithPragmas "doesn't suggest disabling type errors" "DeferredTypeErrors" $ \doc -> do
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getAllCodeActions doc
      liftIO $ "Disable \"deferred-type-errors\" warnings" `notElem` map (^. L.title) cas @? "Doesn't contain deferred-type-errors code action"
      liftIO $ length cas == 0 @? "Expected no code actions, but got: " <> show cas
  ]

completionTests :: TestTree
completionTests =
  testGroup "completions"
  [ completionTest "completes pragmas" "Completion.hs" "" "LANGUAGE" (Just Snippet) (Just "LANGUAGE ${1:extension} #-}") (Just "{-# LANGUAGE #-}") [0, 4, 0, 34, 0, 4]
  , completionTest "completes pragmas with existing closing pragma bracket" "Completion.hs" "" "LANGUAGE" (Just Snippet) (Just "LANGUAGE ${1:extension}") (Just "{-# LANGUAGE #-}") [0, 4, 0, 31, 0, 4]
  , completionTest "completes pragmas with existing closing comment bracket" "Completion.hs" "" "LANGUAGE" (Just Snippet) (Just "LANGUAGE ${1:extension} #") (Just "{-# LANGUAGE #-}") [0, 4, 0, 32, 0, 4]
  , completionTest "completes pragmas with existing closing bracket" "Completion.hs" "" "LANGUAGE" (Just Snippet) (Just "LANGUAGE ${1:extension} #-") (Just "{-# LANGUAGE #-}") [0, 4, 0, 33, 0, 4]
  , completionTest "completes options pragma" "Completion.hs" "OPTIONS" "OPTIONS_GHC" (Just Snippet) (Just "OPTIONS_GHC -${1:option} #-}") (Just "{-# OPTIONS_GHC #-}") [0, 4, 0, 34, 0, 4]
  , completionTest "completes ghc options pragma values" "Completion.hs" "{-# OPTIONS_GHC -Wno-red  #-}\n" "Wno-redundant-constraints" Nothing Nothing Nothing [0, 0, 0, 0, 0, 24]
  , completionTest "completes language extensions" "Completion.hs" "" "OverloadedStrings" Nothing Nothing Nothing [0, 24, 0, 31, 0, 24]
  , completionTest "completes language extensions case insensitive" "Completion.hs" "lAnGuaGe Overloaded" "OverloadedStrings" Nothing Nothing Nothing [0, 4, 0, 34, 0, 24]
  , completionTest "completes the Strict language extension" "Completion.hs" "Str" "Strict" Nothing Nothing Nothing [0, 13, 0, 31, 0, 16]
  , completionTest "completes No- language extensions" "Completion.hs" "NoOverload" "NoOverloadedStrings" Nothing Nothing Nothing [0, 13, 0, 31, 0, 23]
  , onlyWorkForGhcVersions (>=GHC92) "GHC2021 flag introduced since ghc9.2" $
    completionTest "completes GHC2021 extensions" "Completion.hs" "ghc" "GHC2021" Nothing Nothing Nothing [0, 13, 0, 31, 0, 16]
  ]

completionSnippetTests :: TestTree
completionSnippetTests =
  testGroup "expand snippet to pragma" $
    validPragmas <&>
      (\(insertText, label, detail, _) ->
        let input = T.toLower $ T.init label
        in completionTest (T.unpack label)
            "Completion.hs" input label (Just Snippet)
            (Just $ "{-# " <> insertText <> " #-}") (Just detail)
            [0, 0, 0, 34, 0, fromIntegral $ T.length input])

completionTest :: String -> String -> T.Text -> T.Text -> Maybe InsertTextFormat -> Maybe T.Text -> Maybe T.Text -> [UInt] -> TestTree
completionTest testComment fileName te' label textFormat insertText detail [a, b, c, d, x, y] =
  testCase testComment $ runSessionWithServer pragmasPlugin testDataDir $ do
    doc <- openDoc fileName "haskell"
    _ <- waitForDiagnostics
    let te = TextEdit (Range (Position a b) (Position c d)) te'
    _ <- applyEdit doc te
    compls <- getCompletions doc (Position x y)
    item <- getCompletionByLabel label compls
    liftIO $ do
      item ^. L.label @?= label
      item ^. L.kind @?= Just CiKeyword
      item ^. L.insertTextFormat @?= textFormat
      item ^. L.insertText @?= insertText
      item ^. L.detail @?= detail

goldenWithPragmas :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithPragmas title path = goldenWithHaskellDoc pragmasPlugin title testDataDir path "expected" "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
