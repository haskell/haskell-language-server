{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Main
  ( main
  ) where

import           Control.Lens            ((<&>), (^.))
import           Data.Aeson
import           Data.Foldable
import qualified Data.Text               as T
import           Ide.Plugin.Pragmas
import qualified Language.LSP.Types.Lens as L
import           System.FilePath
import           Test.Hls
import           Test.Hls.Util           (onlyWorkForGhcVersions)

main :: IO ()
main = defaultTestRunner tests

pragmasSuggestPlugin :: PluginTestDescriptor ()
pragmasSuggestPlugin = mkPluginTestDescriptor' suggestPragmaDescriptor "pragmas"

pragmasCompletionPlugin :: PluginTestDescriptor ()
pragmasCompletionPlugin = mkPluginTestDescriptor' completionDescriptor "pragmas"

pragmasDisableWarningPlugin :: PluginTestDescriptor ()
pragmasDisableWarningPlugin = mkPluginTestDescriptor' suggestDisableWarningDescriptor "pragmas"

tests :: TestTree
tests =
  testGroup "pragmas"
  [ codeActionTests
  , codeActionTests'
  , completionTests
  , completionSnippetTests
  , dontSuggestCompletionTests
  ]

codeActionTests :: TestTree
codeActionTests =
  testGroup "code actions"
  [ codeActionTestWithPragmasSuggest "Block comment then line comment doesn't split line" "BlockCommentThenLineComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Block comment then single-line block comment doesn't split line" "BlockCommentThenSingleLineBlockComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Block comment then multi-line block comment doesn't split line" "BlockCommentThenMultiLineBlockComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Block comment then line haddock splits line" "BlockCommentThenLineHaddock" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Block comment then single-line block haddock splits line" "BlockCommentThenSingleLineBlockHaddock" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Block comment then multi-line block haddock splits line" "BlockCommentThenMultiLineBlockHaddock" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Pragma then line comment doesn't split line" "PragmaThenLineComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Pragma then single-line block comment doesn't split line" "PragmaThenSingleLineBlockComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Pragma then multi-line block comment splits line" "PragmaThenMultiLineBlockComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Pragma then line haddock splits line" "PragmaThenLineHaddock" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Pragma then single-line block haddock splits line" "PragmaThenSingleLineBlockHaddock" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Pragma then multi-line block haddock splits line" "PragmaThenMultiLineBlockHaddock" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Pragma then single-line block haddock single-line block comment splits line" "PragmaThenSingleLineBlockHaddockSingleLineBlockComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Block comment then single-line block haddock single-line block comment splits line" "BlockCommentThenSingleLineBlockHaddockSingleLineBlockComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Pragma then line haddock then newline line comment splits line" "PragmaThenLineHaddockNewlineLineComment" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "does not add pragma after OPTIONS_GHC pragma located after a declaration" "OptionsGhcAfterDecl" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE with no other pragmas at start ignoring later INLINE pragma" "AddPragmaIgnoreInline" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE before Doc comments after interchanging pragmas" "BeforeDocInterchanging" [("Add \"NamedFieldPuns\"", "Contains NamedFieldPuns code action")]
  , codeActionTestWithPragmasSuggest "Add language after altering OPTIONS_GHC and Language" "AddLanguagePragmaAfterInterchaningOptsGhcAndLangs" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "Add language after pragmas with non standard space between prefix and name" "AddPragmaWithNonStandardSpacingInPrecedingPragmas" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE after OptGHC at start ignoring later INLINE pragma" "AddPragmaAfterOptsGhcIgnoreInline" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE ignore later Ann pragma" "AddPragmaIgnoreLaterAnnPragma" [("Add \"BangPatterns\"", "Contains BangPatterns code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE after interchanging pragmas ignoring later Ann pragma" "AddLanguageAfterInterchaningIgnoringLaterAnn" [("Add \"BangPatterns\"", "Contains BangPatterns code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE after OptGHC preceded by another language pragma" "AddLanguageAfterLanguageThenOptsGhc" [("Add \"NamedFieldPuns\"", "Contains NamedFieldPuns code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE pragma after shebang and last language pragma" "AfterShebangAndPragma" [("Add \"NamedFieldPuns\"", "Contains NamedFieldPuns code action")]
  , codeActionTestWithPragmasSuggest "adds above module keyword on first line" "ModuleOnFirstLine" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE pragma after GHC_OPTIONS" "AfterGhcOptions" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE pragma after shebang and GHC_OPTIONS" "AfterShebangAndOpts" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE pragma after shebang, GHC_OPTIONS and language pragma" "AfterShebangAndOptionsAndPragma" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE pragma after all others ignoring later INLINE pragma" "AfterShebangAndOptionsAndPragmasIgnoreInline" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE pragma after all others ignoring multiple later INLINE pragma" "AfterAllWithMultipleInlines" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "adds LANGUAGE pragma correctly ignoring later INLINE pragma" "AddLanguagePragma" [("Add \"TupleSections\"", "Contains TupleSections code action")]
  , codeActionTestWithPragmasSuggest "adds TypeApplications pragma" "TypeApplications" [("Add \"TypeApplications\"", "Contains TypeApplications code action")]
  , codeActionTestWithPragmasSuggest "after shebang" "AfterShebang" [("Add \"NamedFieldPuns\"", "Contains NamedFieldPuns code action")]
  , codeActionTestWithPragmasSuggest "append to existing pragmas" "AppendToExisting" [("Add \"NamedFieldPuns\"", "Contains NamedFieldPuns code action")]
  , codeActionTestWithPragmasSuggest "before doc comments" "BeforeDocComment" [("Add \"NamedFieldPuns\"", "Contains NamedFieldPuns code action")]
  , codeActionTestWithPragmasSuggest "adds TypeSynonymInstances pragma" "NeedsPragmas" [("Add \"TypeSynonymInstances\"", "Contains TypeSynonymInstances code action"), ("Add \"FlexibleInstances\"", "Contains FlexibleInstances code action")]
  , codeActionTestWithDisableWarning "before doc comments" "MissingSignatures" [("Disable \"missing-signatures\" warnings", "Contains missing-signatures code action")]
  , codeActionTestWithDisableWarning "before doc comments" "UnusedImports" [("Disable \"unused-imports\" warnings", "Contains unused-imports code action")]
  ]

ghc94regression :: String
ghc94regression = "to be reported"

codeActionTestWithPragmasSuggest :: String -> FilePath -> [(T.Text, String)] -> TestTree
codeActionTestWithPragmasSuggest = codeActionTestWith pragmasSuggestPlugin

codeActionTestWithDisableWarning :: String -> FilePath -> [(T.Text, String)] -> TestTree
codeActionTestWithDisableWarning = codeActionTestWith pragmasDisableWarningPlugin

codeActionTestWith :: PluginTestDescriptor () -> String -> FilePath -> [(T.Text, String)] -> TestTree
codeActionTestWith descriptor testComment fp actions =
  goldenWithPragmas descriptor testComment fp $ \doc -> do
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
 goldenWithPragmas pragmasSuggestPlugin "no duplication" "NamedFieldPuns" $ \doc -> do
      _ <- waitForDiagnosticsFrom doc
      cas <- map fromAction <$> getCodeActions doc (Range (Position 8 9) (Position 8 9))
      ca <- liftIO $ case cas of
        [ca] -> pure ca
        _ -> assertFailure $ "Expected one code action, but got: " <> show cas
      liftIO $ (ca ^. L.title == "Add \"NamedFieldPuns\"") @? "NamedFieldPuns code action"
      executeCodeAction ca
  , goldenWithPragmas pragmasSuggestPlugin "doesn't suggest disabling type errors" "DeferredTypeErrors" $ \doc -> do
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
      (\(insertText, label, detail, appearWhere) ->
        let inputPrefix =
              case appearWhere of
                NewLine   -> ""
                CanInline -> "something "
            input = inputPrefix <> (T.toLower $ T.init label)
        in completionTest (T.unpack label)
            "Completion.hs" input label (Just Snippet)
            (Just $ "{-# " <> insertText <> " #-}") (Just detail)
            [0, 0, 0, 34, 0, fromIntegral $ T.length input])

dontSuggestCompletionTests :: TestTree
dontSuggestCompletionTests =
  testGroup "do not suggest pragmas" $
  let replaceFuncBody newBody = Just $ mkEdit (8,6) (8,8) newBody
      writeInEmptyLine txt = Just $ mkEdit (3,0) (3,0) txt
      generalTests = [ provideNoCompletionsTest "in imports" "Completion.hs" (Just $ mkEdit (3,0) (3,0) "import WA") (Position 3 8)
                     , provideNoCompletionsTest "when no word has been typed" "Completion.hs" Nothing (Position 3 0)
                     , provideNoCompletionsTest "when expecting auto complete on modules" "Completion.hs" (Just $ mkEdit (8,6) (8,8) "Data.Maybe.WA") (Position 8 19)
                     ]
      individualPragmaTests = validPragmas <&> \(insertText,label,detail,appearWhere) ->
          let completionPrompt = T.toLower $ T.init label
              promptLen = fromIntegral (T.length completionPrompt)
          in case appearWhere of
              CanInline ->
                  provideNoUndesiredCompletionsTest ("at new line: " <> T.unpack label) "Completion.hs" (Just label) (writeInEmptyLine completionPrompt) (Position 3 0)
              NewLine ->
                  provideNoUndesiredCompletionsTest ("inline: " <> T.unpack label) "Completion.hs" (Just label) (replaceFuncBody completionPrompt) (Position 8 (6 + promptLen))
    in generalTests ++ individualPragmaTests

mkEdit :: (UInt,UInt) -> (UInt,UInt) -> T.Text -> TextEdit
mkEdit (startLine, startCol) (endLine, endCol) newText =
    TextEdit (Range (Position startLine startCol) (Position endLine endCol)) newText

completionTest :: String -> FilePath -> T.Text -> T.Text -> Maybe InsertTextFormat -> Maybe T.Text -> Maybe T.Text -> [UInt] -> TestTree
completionTest testComment fileName replacementText expectedLabel expectedFormat expectedInsertText detail [delFromLine, delFromCol, delToLine, delToCol, completeAtLine, completeAtCol] =
  testCase testComment $ runSessionWithServer pragmasCompletionPlugin testDataDir $ do
    doc <- openDoc fileName "haskell"
    _ <- waitForDiagnostics
    let te = TextEdit (Range (Position delFromLine delFromCol) (Position delToLine delToCol)) replacementText
    _ <- applyEdit doc te
    compls <- getCompletions doc (Position completeAtLine completeAtCol)
    item <- getCompletionByLabel expectedLabel compls
    liftIO $ do
      item ^. L.label @?= expectedLabel
      item ^. L.kind @?= Just CiKeyword
      item ^. L.insertTextFormat @?= expectedFormat
      item ^. L.insertText @?= expectedInsertText
      item ^. L.detail @?= detail

provideNoCompletionsTest :: String -> FilePath -> Maybe TextEdit -> Position -> TestTree
provideNoCompletionsTest testComment fileName mTextEdit pos =
  provideNoUndesiredCompletionsTest testComment fileName Nothing mTextEdit pos

provideNoUndesiredCompletionsTest :: String -> FilePath -> Maybe T.Text -> Maybe TextEdit -> Position -> TestTree
provideNoUndesiredCompletionsTest testComment fileName mUndesiredLabel mTextEdit pos =
  testCase testComment $ runSessionWithServer pragmasCompletionPlugin testDataDir $ do
    doc <- openDoc fileName "haskell"
    _ <- waitForDiagnostics
    _ <- sendConfigurationChanged disableGhcideCompletions
    mapM_ (applyEdit doc) mTextEdit
    compls <- getCompletions doc pos
    liftIO $ case mUndesiredLabel of
        Nothing -> compls @?= []
        Just undesiredLabel -> do
            case find (\c -> c ^. L.label == undesiredLabel) compls of
                Just c -> assertFailure $
                    "Did not expect a completion with label=" <> T.unpack undesiredLabel
                    <> ", got completion: "<> show c
                Nothing -> pure ()

disableGhcideCompletions :: Value
disableGhcideCompletions = object [ "haskell" .= object ["plugin" .= object [ "ghcide-completions" .= object ["globalOn" .= False]]] ]

goldenWithPragmas :: PluginTestDescriptor () -> TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithPragmas descriptor title path = goldenWithHaskellDoc descriptor title testDataDir path "expected" "hs"

testDataDir :: FilePath
testDataDir = "test" </> "testdata"
