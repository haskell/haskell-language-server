{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-unticked-promoted-constructors #-}

module Main
  ( main
  ) where

import           Control.Applicative.Combinators
import           Control.Lens                             ((^.))
import           Control.Monad
import           Data.Default
import           Data.Foldable
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Text                                as T
import           Data.Tuple.Extra
import           Development.IDE.GHC.Util
import           Development.IDE.Plugin.Completions.Types (extendImportCommandId)
import           Development.IDE.Test
import           Development.IDE.Types.Location
import           Development.Shake                        (getDirectoryFilesIO)
import           Ide.Types
import qualified Language.LSP.Protocol.Lens               as L
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types              hiding
                                                          (SemanticTokenAbsolute (length, line),
                                                           SemanticTokenRelative (length),
                                                           SemanticTokensEdit (_start),
                                                           mkRange)
import           Language.LSP.Test
import           System.Directory
import           System.FilePath
import qualified System.IO.Extra
import           System.IO.Extra                          hiding (withTempDir)
import           System.Time.Extra
import           Test.Tasty
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.HUnit
import           Text.Regex.TDFA                          ((=~))


import           Development.IDE.Plugin.CodeAction        (matchRegExMultipleImports)
import           Test.Hls

import           Control.Applicative                      (liftA2)
import qualified Development.IDE.Plugin.CodeAction        as Refactor
import qualified Development.IDE.Plugin.HLS.GhcIde        as GhcIde
import qualified Test.AddArgument

main :: IO ()
main = defaultTestRunner tests

refactorPlugin :: IO (IdePlugins IdeState)
refactorPlugin = do
  exactprintLog <- pluginTestRecorder
  ghcideLog <- pluginTestRecorder
  pure $ IdePlugins $
      [ Refactor.iePluginDescriptor exactprintLog "ghcide-code-actions-imports-exports"
      , Refactor.typeSigsPluginDescriptor exactprintLog "ghcide-code-actions-type-signatures"
      , Refactor.bindingsPluginDescriptor exactprintLog "ghcide-code-actions-bindings"
      , Refactor.fillHolePluginDescriptor exactprintLog "ghcide-code-actions-fill-holes"
      , Refactor.extendImportPluginDescriptor exactprintLog "ghcide-completions-1"
      ] ++ GhcIde.descriptors ghcideLog

tests :: TestTree
tests =
  testGroup "refactor"
  [ initializeTests
  , codeActionTests
  , codeActionHelperFunctionTests
  , completionTests
  ]

initializeTests = withResource acquire release tests
  where
    tests :: IO (TResponseMessage Method_Initialize) -> TestTree
    tests getInitializeResponse = testGroup "initialize response capabilities"
        [ chk "   code action"             _codeActionProvider  (Just (InR (CodeActionOptions {_workDoneProgress = Nothing, _codeActionKinds = Nothing, _resolveProvider = Just False})))
        , che "   execute command"         _executeCommandProvider [extendImportCommandId]
        ]
      where
        chk :: (Eq a, Show a) => TestName -> (ServerCapabilities -> a) -> a -> TestTree
        chk title getActual expected =
          testCase title $ getInitializeResponse >>= \ir -> expected @=? (getActual . innerCaps) ir

        che :: TestName -> (ServerCapabilities -> Maybe ExecuteCommandOptions) -> [T.Text] -> TestTree
        che title getActual expected = testCase title doTest
          where
              doTest = do
                  ir <- getInitializeResponse
                  let Just ExecuteCommandOptions {_commands = commands} = getActual $ innerCaps ir
                  -- Check if expected exists in commands. Note that commands can arrive in different order.
                  mapM_ (\e -> any (\o -> T.isSuffixOf e o) commands @? show (expected, show commands)) expected

    acquire :: IO (TResponseMessage Method_Initialize)
    acquire = run initializeResponse


    release :: TResponseMessage Method_Initialize -> IO ()
    release = const $ pure ()

    innerCaps :: TResponseMessage Method_Initialize -> ServerCapabilities
    innerCaps (TResponseMessage _ _ (Right (InitializeResult c _))) = c
    innerCaps (TResponseMessage _ _ (Left _)) = error "Initialization error"

completionTests :: TestTree
completionTests =
    testGroup "auto import snippets"
      [ completionCommandTest
        "show imports not in list - simple"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad (msum)", "f = joi"]
        (Position 3 6)
        "join"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad (msum, join)", "f = joi"]
      , completionCommandTest
        "show imports not in list - multi-line"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad (\n    msum)", "f = joi"]
        (Position 4 6)
        "join"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad (\n    msum, join)", "f = joi"]
      , completionCommandTest
        "show imports not in list - names with _"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad as M (msum)", "f = M.mapM_"]
        (Position 3 11)
        "mapM_"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad as M (msum, mapM_)", "f = M.mapM_"]
      , completionCommandTest
        "show imports not in list - initial empty list"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad as M ()", "f = M.joi"]
        (Position 3 10)
        "join"
        ["{-# LANGUAGE NoImplicitPrelude #-}",
        "module A where", "import Control.Monad as M (join)", "f = M.joi"]
      , testGroup "qualified imports"
        [ completionCommandTest
            "single"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import Control.Monad ()", "f = Control.Monad.joi"]
            (Position 3 22)
            "join"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import Control.Monad (join)", "f = Control.Monad.joi"]
        , completionCommandTest
            "as"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import Control.Monad as M ()", "f = M.joi"]
            (Position 3 10)
            "join"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import Control.Monad as M (join)", "f = M.joi"]
        , completionCommandTest
            "multiple"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import Control.Monad as M ()", "import Control.Monad as N ()", "f = N.joi"]
            (Position 4 10)
            "join"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import Control.Monad as M ()", "import Control.Monad as N (join)", "f = N.joi"]
        -- Regression test for https://github.com/haskell/haskell-language-server/issues/2824
        , completionNoCommandTest
            "explicit qualified"
            ["{-# LANGUAGE NoImplicitPrelude #-}",
            "module A where", "import qualified Control.Monad as M (j)"]
            (Position 2 38)
            "join"
        , completionNoCommandTest
            "explicit qualified post"
            ["{-# LANGUAGE NoImplicitPrelude, ImportQualifiedPost #-}",
            "module A where", "import Control.Monad qualified as M (j)"]
            (Position 2 38)
            "join"
        , completionNoCommandTest
            "multiline import"
            [ "{-# LANGUAGE NoImplicitPrelude #-}"
            , "module A where", "import Control.Monad", "    (fore)"]
            (Position 3 9)
            "forever"
        ]
      , testGroup "Data constructor"
        [ completionCommandTest
            "not imported"
            ["module A where", "import Text.Printf ()", "ZeroPad"]
            (Position 2 4)
            "ZeroPad"
            ["module A where", "import Text.Printf (FormatAdjustment (ZeroPad))", "ZeroPad"]
        , completionCommandTest
            "parent imported abs"
            ["module A where", "import Text.Printf (FormatAdjustment)", "ZeroPad"]
            (Position 2 4)
            "ZeroPad"
            ["module A where", "import Text.Printf (FormatAdjustment (ZeroPad))", "ZeroPad"]
        , completionNoCommandTest
            "parent imported all"
            ["module A where", "import Text.Printf (FormatAdjustment (..))", "ZeroPad"]
            (Position 2 4)
            "ZeroPad"
        , completionNoCommandTest
            "already imported"
            ["module A where", "import Text.Printf (FormatAdjustment (ZeroPad))", "ZeroPad"]
            (Position 2 4)
            "ZeroPad"
        , completionNoCommandTest
            "function from Prelude"
            ["module A where", "import Data.Maybe ()", "Nothing"]
            (Position 2 4)
            "Nothing"
        , completionCommandTest
            "type operator parent"
            ["module A where", "import Data.Type.Equality ()", "f = Ref"]
            (Position 2 8)
            "Refl"
            ["module A where", "import Data.Type.Equality (type (:~:) (Refl))", "f = Ref"]
        ]
      , testGroup "Record completion"
        [ completionCommandTest
            "not imported"
            ["module A where", "import Text.Printf ()", "FormatParse"]
            (Position 2 10)
            "FormatParse"
            ["module A where", "import Text.Printf (FormatParse)", "FormatParse"]
        , completionCommandTest
            "parent imported"
            ["module A where", "import Text.Printf (FormatParse)", "FormatParse"]
            (Position 2 10)
            "FormatParse"
            ["module A where", "import Text.Printf (FormatParse (FormatParse))", "FormatParse"]
        , completionNoCommandTest
            "already imported"
            ["module A where", "import Text.Printf (FormatParse (FormatParse))", "FormatParse"]
            (Position 2 10)
            "FormatParse"
        ]
        , testGroup "Package completion"
          [ completionCommandTest
                  "import Data.Sequence"
                  ["module A where", "foo :: Seq"]
                  (Position 1 9)
                  "Seq"
                  ["module A where", "import Data.Sequence (Seq)", "foo :: Seq"]

          , completionCommandTest
                  "qualified import"
                  ["module A where", "foo :: Seq.Seq"]
                  (Position 1 13)
                  "Seq"
                  ["module A where", "import qualified Data.Sequence as Seq", "foo :: Seq.Seq"]
          ]
      ]

completionCommandTest ::
  String ->
  [T.Text] ->
  Position ->
  T.Text ->
  [T.Text] ->
  TestTree
completionCommandTest name src pos wanted expected = testSession name $ do
  docId <- createDoc "A.hs" "haskell" (T.unlines src)
  _ <- waitForDiagnostics
  compls <- skipManyTill anyMessage (getCompletions docId pos)
  let wantedC = find ( \case
            CompletionItem {_insertText = Just x
                           ,_command    = Just _} -> wanted `T.isPrefixOf` x
            _                                     -> False
            ) compls
  case wantedC of
    Nothing ->
      liftIO $ assertFailure $ "Cannot find expected completion in: " <> show [_label | CompletionItem {_label} <- compls]
    Just CompletionItem {..} -> do
      c <- assertJust "Expected a command" _command
      executeCommand c
      if src /= expected
          then do
            modifiedCode <- skipManyTill anyMessage (getDocumentEdit docId)
            liftIO $ modifiedCode @?= T.unlines expected
          else do
            expectMessages SMethod_WorkspaceApplyEdit 1 $ \edit ->
              liftIO $ assertFailure $ "Expected no edit but got: " <> show edit

completionNoCommandTest ::
  String ->
  [T.Text] ->
  Position ->
  T.Text ->
  TestTree
completionNoCommandTest name src pos wanted = testSession name $ do
  docId <- createDoc "A.hs" "haskell" (T.unlines src)
  _ <- waitForDiagnostics
  compls <- getCompletions docId pos
  let isPrefixOfInsertOrLabel ci = any (wanted `T.isPrefixOf`) [fromMaybe "" (ci ^. L.insertText), ci ^. L.label]
  case find isPrefixOfInsertOrLabel compls of
    Nothing ->
      liftIO $ assertFailure $ "Cannot find expected completion in: " <> show [_label | CompletionItem {_label} <- compls]
    Just CompletionItem{..} -> liftIO . assertBool ("Expected no command but got: " <> show _command) $ null _command


codeActionTests :: TestTree
codeActionTests = testGroup "code actions"
  [ suggestImportDisambiguationTests
  , insertImportTests
  , extendImportTests
  , renameActionTests
  , typeWildCardActionTests
  , removeImportTests
  , suggestImportClassMethodTests
  , suggestImportTests
  , suggestAddRecordFieldImportTests
  , suggestHideShadowTests
  , fixConstructorImportTests
  , fixModuleImportTypoTests
  , importRenameActionTests
  , fillTypedHoleTests
  , addSigActionTests
  , insertNewDefinitionTests
  , deleteUnusedDefinitionTests
  , addInstanceConstraintTests
  , addFunctionConstraintTests
  , removeRedundantConstraintsTests
  , addTypeAnnotationsToLiteralsTest
  , exportUnusedTests
  , addImplicitParamsConstraintTests
  , removeExportTests
  , Test.AddArgument.tests
  ]

insertImportTests :: TestTree
insertImportTests = testGroup "insert import"
  [ checkImport
        "module where keyword lower in file no exports"
        "WhereKeywordLowerInFileNoExports.hs"
        "WhereKeywordLowerInFileNoExports.expected.hs"
        "import Data.Int"
  , checkImport
        "module where keyword lower in file with exports"
        "WhereDeclLowerInFile.hs"
        "WhereDeclLowerInFile.expected.hs"
        "import Data.Int"
  , checkImport
        "module where keyword lower in file with comments before it"
        "WhereDeclLowerInFileWithCommentsBeforeIt.hs"
        "WhereDeclLowerInFileWithCommentsBeforeIt.expected.hs"
        "import Data.Int"
  , expectFailBecause
      "'findNextPragmaPosition' function doesn't account for case when shebang is not placed at top of file"
      (checkImport
         "Shebang not at top with spaces"
         "ShebangNotAtTopWithSpaces.hs"
         "ShebangNotAtTopWithSpaces.expected.hs"
         "import Data.Monoid")
  , expectFailBecause
      "'findNextPragmaPosition' function doesn't account for case when shebang is not placed at top of file"
      (checkImport
         "Shebang not at top no space"
         "ShebangNotAtTopNoSpace.hs"
         "ShebangNotAtTopNoSpace.expected.hs"
         "import Data.Monoid")
  , expectFailBecause
      ("'findNextPragmaPosition' function doesn't account for case "
      ++ "when OPTIONS_GHC pragma is not placed at top of file")
      (checkImport
         "OPTIONS_GHC pragma not at top with spaces"
         "OptionsNotAtTopWithSpaces.hs"
         "OptionsNotAtTopWithSpaces.expected.hs"
         "import Data.Monoid")
  , expectFailBecause
      ("'findNextPragmaPosition' function doesn't account for "
      ++ "case when shebang is not placed at top of file")
      (checkImport
         "Shebang not at top of file"
         "ShebangNotAtTop.hs"
         "ShebangNotAtTop.expected.hs"
         "import Data.Monoid")
  , expectFailBecause
      ("'findNextPragmaPosition' function doesn't account for case "
      ++ "when OPTIONS_GHC is not placed at top of file")
      (checkImport
         "OPTIONS_GHC pragma not at top of file"
         "OptionsPragmaNotAtTop.hs"
         "OptionsPragmaNotAtTop.expected.hs"
         "import Data.Monoid")
  , expectFailBecause
      ("'findNextPragmaPosition' function doesn't account for case when "
      ++ "OPTIONS_GHC pragma is not placed at top of file")
      (checkImport
         "pragma not at top with comment at top"
         "PragmaNotAtTopWithCommentsAtTop.hs"
         "PragmaNotAtTopWithCommentsAtTop.expected.hs"
         "import Data.Monoid")
  , expectFailBecause
      ("'findNextPragmaPosition' function doesn't account for case when "
      ++ "OPTIONS_GHC pragma is not placed at top of file")
      (checkImport
         "pragma not at top multiple comments"
         "PragmaNotAtTopMultipleComments.hs"
         "PragmaNotAtTopMultipleComments.expected.hs"
         "import Data.Monoid")
  , expectFailBecause
      "'findNextPragmaPosition' function doesn't account for case of multiline pragmas"
      (checkImport
         "after multiline language pragmas"
         "MultiLinePragma.hs"
         "MultiLinePragma.expected.hs"
         "import Data.Monoid")
  , checkImport
      "pragmas not at top with module declaration"
      "PragmaNotAtTopWithModuleDecl.hs"
      "PragmaNotAtTopWithModuleDecl.expected.hs"
      "import Data.Monoid"
  , checkImport
      "pragmas not at top with imports"
      "PragmaNotAtTopWithImports.hs"
      "PragmaNotAtTopWithImports.expected.hs"
      "import Data.Monoid"
  , checkImport
      "above comment at top of module"
      "CommentAtTop.hs"
      "CommentAtTop.expected.hs"
      "import Data.Monoid"
  , checkImport
      "above multiple comments below"
      "CommentAtTopMultipleComments.hs"
      "CommentAtTopMultipleComments.expected.hs"
      "import Data.Monoid"
  , checkImport
      "above curly brace comment"
      "CommentCurlyBraceAtTop.hs"
      "CommentCurlyBraceAtTop.expected.hs"
      "import Data.Monoid"
  , checkImport
      "above multi-line comment"
      "MultiLineCommentAtTop.hs"
      "MultiLineCommentAtTop.expected.hs"
      "import Data.Monoid"
  , checkImport
      "above comment with no module explicit exports"
      "NoExplicitExportCommentAtTop.hs"
      "NoExplicitExportCommentAtTop.expected.hs"
      "import Data.Monoid"
  , checkImport
      "above two-dash comment with no pipe"
      "TwoDashOnlyComment.hs"
      "TwoDashOnlyComment.expected.hs"
      "import Data.Monoid"
  , checkImport
      "above comment with no (module .. where) decl"
      "NoModuleDeclarationCommentAtTop.hs"
      "NoModuleDeclarationCommentAtTop.expected.hs"
      "import Data.Monoid"
  , checkImport
      "comment not at top with no (module .. where) decl"
      "NoModuleDeclaration.hs"
      "NoModuleDeclaration.expected.hs"
      "import Data.Monoid"
  , checkImport
      "comment not at top (data dec is)"
      "DataAtTop.hs"
      "DataAtTop.expected.hs"
      "import Data.Monoid"
  , checkImport
      "comment not at top (newtype is)"
      "NewTypeAtTop.hs"
      "NewTypeAtTop.expected.hs"
      "import Data.Monoid"
  , checkImport
      "with no explicit module exports"
      "NoExplicitExports.hs"
      "NoExplicitExports.expected.hs"
      "import Data.Monoid"
  , checkImport
      "add to correctly placed existing import"
      "ImportAtTop.hs"
      "ImportAtTop.expected.hs"
      "import Data.Monoid"
  , checkImport
      "add to multiple correctly placed existing imports"
      "MultipleImportsAtTop.hs"
      "MultipleImportsAtTop.expected.hs"
      "import Data.Monoid"
  , checkImport
      "with language pragma at top of module"
      "LangPragmaModuleAtTop.hs"
      "LangPragmaModuleAtTop.expected.hs"
      "import Data.Monoid"
  , checkImport
      "with language pragma and explicit module exports"
      "LangPragmaModuleWithComment.hs"
      "LangPragmaModuleWithComment.expected.hs"
      "import Data.Monoid"
  , checkImport
      "with language pragma at top and no module declaration"
      "LanguagePragmaAtTop.hs"
      "LanguagePragmaAtTop.expected.hs"
      "import Data.Monoid"
  , checkImport
      "with multiple lang pragmas and no module declaration"
      "MultipleLanguagePragmasNoModuleDeclaration.hs"
      "MultipleLanguagePragmasNoModuleDeclaration.expected.hs"
      "import Data.Monoid"
  , checkImport
      "with pragmas and shebangs"
      "LanguagePragmasThenShebangs.hs"
      "LanguagePragmasThenShebangs.expected.hs"
      "import Data.Monoid"
  , checkImport
      "with pragmas and shebangs but no comment at top"
      "PragmasAndShebangsNoComment.hs"
      "PragmasAndShebangsNoComment.expected.hs"
      "import Data.Monoid"
  , checkImport
      "module decl no exports under pragmas and shebangs"
      "PragmasShebangsAndModuleDecl.hs"
      "PragmasShebangsAndModuleDecl.expected.hs"
      "import Data.Monoid"
  , checkImport
      "module decl with explicit import under pragmas and shebangs"
      "PragmasShebangsModuleExplicitExports.hs"
      "PragmasShebangsModuleExplicitExports.expected.hs"
      "import Data.Monoid"
  , checkImport
      "module decl and multiple imports"
      "ModuleDeclAndImports.hs"
      "ModuleDeclAndImports.expected.hs"
      "import Data.Monoid"
  , importQualifiedTests
  ]

importQualifiedTests :: TestTree
importQualifiedTests = testGroup "import qualified prefix suggestions"
  [ checkImport'
      "qualified import works with 3.8 code action kinds"
      "ImportQualified.hs"
      "ImportQualified.expected.hs"
      "import qualified Control.Monad as Control"
      ["import Control.Monad (when)"]
  , checkImport'
      "qualified import in postfix position works with 3.8 code action kinds"
      "ImportPostQualified.hs"
      "ImportPostQualified.expected.hs"
      "import Control.Monad qualified as Control"
      ["import qualified Control.Monad as Control", "import Control.Monad (when)"]
  ]

checkImport :: String -> FilePath -> FilePath -> T.Text -> TestTree
checkImport testComment originalPath expectedPath action =
  checkImport' testComment originalPath expectedPath action []

checkImport' :: String -> FilePath -> FilePath -> T.Text -> [T.Text] -> TestTree
checkImport' testComment originalPath expectedPath action excludedActions =
  testSessionWithExtraFiles "import-placement" testComment $ \dir ->
    check (dir </> originalPath) (dir </> expectedPath) action
  where
    check :: FilePath -> FilePath -> T.Text -> Session ()
    check originalPath expectedPath action = do
      oSrc <- liftIO $ readFileUtf8 originalPath
      eSrc <- liftIO $ readFileUtf8 expectedPath
      originalDoc <- createDoc originalPath "haskell" oSrc
      _ <- waitForDiagnostics
      shouldBeDoc <- createDoc expectedPath "haskell" eSrc
      actionsOrCommands <- getAllCodeActions originalDoc
      for_ excludedActions (\a -> liftIO $ assertNoActionWithTitle a actionsOrCommands)
      chosenAction <- liftIO $ pickActionWithTitle action actionsOrCommands
      executeCodeAction chosenAction
      originalDocAfterAction <- documentContents originalDoc
      shouldBeDocContents <- documentContents shouldBeDoc
      liftIO $ T.replace "\r\n" "\n" shouldBeDocContents @=? T.replace "\r\n" "\n" originalDocAfterAction

renameActionTests :: TestTree
renameActionTests = testGroup "rename actions"
  [ testSession "change to local variable name" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int"
            , "foo argName = argNme"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      action <- findCodeAction doc (Range (Position 2 14) (Position 2 20)) "Replace with ‘argName’"
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "foo :: Int -> Int"
            , "foo argName = argName"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "change to name of imported function" $ do
      let content = T.unlines
            [ "module Testing where"
            , "import Data.Maybe (maybeToList)"
            , "foo :: Maybe a -> [a]"
            , "foo = maybToList"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      action <- findCodeAction doc (Range (Position 3 6) (Position 3 16))  "Replace with ‘maybeToList’"
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "import Data.Maybe (maybeToList)"
            , "foo :: Maybe a -> [a]"
            , "foo = maybeToList"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "suggest multiple local variable names" $ do
      let content = T.unlines
            [ "module Testing where"
            , "foo :: Char -> Char -> Char -> Char"
            , "foo argument1 argument2 argument3 = argumentX"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      _ <- findCodeActions doc (Range (Position 2 36) (Position 2 45))
                           ["Replace with ‘argument1’", "Replace with ‘argument2’", "Replace with ‘argument3’"]
      return()
  , testSession "change infix function" $ do
      let content = T.unlines
            [ "module Testing where"
            , "monus :: Int -> Int"
            , "monus x y = max 0 (x - y)"
            , "foo x y = x `monnus` y"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 3 12) (Position 3 20))
      [fixTypo] <- pure [action | InR action@CodeAction{ _title = actionTitle } <- actionsOrCommands, "monus" `T.isInfixOf` actionTitle , "Replace" `T.isInfixOf` actionTitle]
      executeCodeAction fixTypo
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "monus :: Int -> Int"
            , "monus x y = max 0 (x - y)"
            , "foo x y = x `monus` y"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "change template function" $ do
      let content = T.unlines
            [ "{-# LANGUAGE TemplateHaskellQuotes #-}"
            , "module Testing where"
            , "import Language.Haskell.TH (Name)"
            , "foo :: Name"
            , "foo = 'bread"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      diags <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 4 6) (Position 4 12))
      [fixTypo] <- pure [action | InR action@CodeAction{ _title = actionTitle } <- actionsOrCommands, "break" `T.isInfixOf` actionTitle ]
      executeCodeAction fixTypo
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "{-# LANGUAGE TemplateHaskellQuotes #-}"
            , "module Testing where"
            , "import Language.Haskell.TH (Name)"
            , "foo :: Name"
            , "foo = 'break"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

typeWildCardActionTests :: TestTree
typeWildCardActionTests = testGroup "type wildcard actions"
  [ testUseTypeSignature "global signature"
        [ "func :: _"
        , "func x = x"
        ]
        [ "func :: p -> p"
        , "func x = x"
        ]
  , testUseTypeSignature "local signature"
        [ "func :: Int -> Int"
        , "func x ="
        , "  let y :: _"
        , "      y = x * 2"
        , "  in y"
        ]
        [ "func :: Int -> Int"
        , "func x ="
        , "  let y :: Int"
        , "      y = x * 2"
        , "  in y"
        ]
  , testUseTypeSignature "multi-line message 1"
        [ "func :: _"
        , "func x y = x + y"
        ]
        [ "func :: Integer -> Integer -> Integer"
        , "func x y = x + y"
        ]
  , testUseTypeSignature "type in parentheses"
        [ "func :: a -> _"
        , "func x = (x, const x)"
        ]
        [ "func :: a -> (a, b -> a)"
        , "func x = (x, const x)"
        ]
  , testUseTypeSignature "type in brackets"
        [ "func :: _ -> Maybe a"
        , "func xs = head xs"
        ]
        [ "func :: [Maybe a] -> Maybe a"
        , "func xs = head xs"
        ]
  , testUseTypeSignature "unit type"
        [ "func :: IO _"
        , "func = putChar 'H'"
        ]
        [ "func :: IO ()"
        , "func = putChar 'H'"
        ]
  , testUseTypeSignature "no spaces around '::'"
        [ "func::_"
        , "func x y = x + y"
        ]
        [ "func::Integer -> Integer -> Integer"
        , "func x y = x + y"
        ]
  , testGroup "add parens if hole is part of bigger type"
    [ testUseTypeSignature "subtype 1"
          [ "func :: _ -> Integer -> Integer"
          , "func x y = x + y"
          ]
          [ "func :: Integer -> Integer -> Integer"
          , "func x y = x + y"
          ]
    , testUseTypeSignature "subtype 2"
          [ "func :: Integer -> _ -> Integer"
          , "func x y = x + y"
          ]
          [ "func :: Integer -> Integer -> Integer"
          , "func x y = x + y"
          ]
    , testUseTypeSignature "subtype 3"
          [ "func :: Integer -> Integer -> _"
          , "func x y = x + y"
          ]
          [ "func :: Integer -> Integer -> Integer"
          , "func x y = x + y"
          ]
    , testUseTypeSignature "subtype 4"
          [ "func :: Integer -> _"
          , "func x y = x + y"
          ]
          [ "func :: Integer -> (Integer -> Integer)"
          , "func x y = x + y"
          ]
    ]
  ]
  where
    -- | Test session of given name, checking action "Use type signature..."
    --   on a test file with given content and comparing to expected result.
    testUseTypeSignature name textIn textOut = testSession name $ do
        let fileStart = "module Testing where"
            content = T.unlines $ fileStart : textIn
            expectedContentAfterAction = T.unlines $ fileStart : textOut
        doc <- createDoc "Testing.hs" "haskell" content
        _ <- waitForDiagnostics
        actionsOrCommands <- getAllCodeActions doc
        let [addSignature] = [action | InR action@CodeAction { _title = actionTitle } <- actionsOrCommands
                                    , "Use type signature" `T.isInfixOf` actionTitle
                            ]
        executeCodeAction addSignature
        contentAfterAction <- documentContents doc
        liftIO $ expectedContentAfterAction @=? contentAfterAction


{-# HLINT ignore "Use nubOrd" #-}
removeImportTests :: TestTree
removeImportTests = testGroup "remove import actions"
  [ testSession "redundant" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "qualified redundant" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant binding" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "stuffA = False"
            , "stuffB :: Integer"
            , "stuffB = 123"
            , "stuffC = ()"
            , "_stuffD = '_'"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (stuffA, stuffB, _stuffD, stuffC, stuffA)"
            , "main = print stuffB"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove _stuffD, stuffA, stuffC from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (stuffB)"
            , "main = print stuffB"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant binding - unicode regression " $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "data A = A"
            , "ε :: Double"
            , "ε = 0.5"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (A(..), ε)"
            , "a = A"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove ε from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (A(..))"
            , "a = A"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant operator" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "a !! _b = a"
            , "a <?> _b = a"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA as A ((<?>), stuffB, (!!))"
            , "main = print A.stuffB"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove !!, <?> from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import qualified ModuleA as A (stuffB)"
            , "main = print A.stuffB"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant all import" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "data A = A"
            , "stuffB :: Integer"
            , "stuffB = 123"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (A(..), stuffB)"
            , "main = print stuffB"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove A from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (stuffB)"
            , "main = print stuffB"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "redundant constructor import" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "data D = A | B"
            , "data E = F"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (D(A,B), E(F))"
            , "main = B"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove A, E, F from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (D(B))"
            , "main = B"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "import containing the identifier Strict" $ do
      let contentA = T.unlines
            [ "module Strict where"
            ]
      _docA <- createDoc "Strict.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import Strict"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "remove all" $ do
      let content = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleA where"
            , "import Data.Function (fix, (&))"
            , "import qualified Data.Functor.Const"
            , "import Data.Functor.Identity"
            , "import Data.Functor.Sum (Sum (InL, InR))"
            , "import qualified Data.Kind as K (Constraint, Type)"
            , "x = InL (Identity 123)"
            , "y = fix id"
            , "type T = K.Type"
            ]
      doc <- createDoc "ModuleC.hs" "haskell" content
      _ <- waitForDiagnostics
      [_, _, _, _, InR action@CodeAction { _title = actionTitle }]
          <- nub <$> getAllCodeActions doc
      liftIO $ "Remove all redundant imports" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleA where"
            , "import Data.Function (fix)"
            , "import Data.Functor.Identity"
            , "import Data.Functor.Sum (Sum (InL))"
            , "import qualified Data.Kind as K (Type)"
            , "x = InL (Identity 123)"
            , "y = fix id"
            , "type T = K.Type"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  , testSession "remove unused operators whose name ends with '.'" $ do
      let contentA = T.unlines
            [ "module ModuleA where"
            , "(@.) = 0 -- Must have an operator whose name ends with '.'"
            , "a = 1 -- .. but also something else"
            ]
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      let contentB = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (a, (@.))"
            , "x = a -- Must use something from module A, but not (@.)"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" contentB
      _ <- waitForDiagnostics
      [InR action@CodeAction { _title = actionTitle }, _]
          <- getCodeActions docB (Range (Position 2 0) (Position 2 5))
      liftIO $ "Remove @. from import" @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      let expectedContentAfterAction = T.unlines
            [ "{-# OPTIONS_GHC -Wunused-imports #-}"
            , "module ModuleB where"
            , "import ModuleA (a)"
            , "x = a -- Must use something from module A, but not (@.)"
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction
  ]

extendImportTests :: TestTree
extendImportTests = testGroup "extend import actions"
  [ testGroup "with checkAll" $ tests True
  , testGroup "without checkAll" $ tests False
  ]
  where
    tests overrideCheckProject =
        [ testSession "extend all constructors for record field" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "data A = B { a :: Int }"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A(B))"
                    , "f = a"
                    ])
            (Range (Position 2 4) (Position 2 5))
            [ "Add A(..) to the import list of ModuleA"
            , "Add A(a) to the import list of ModuleA"
            , "Add a to the import list of ModuleA"
            ]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A(..))"
                    , "f = a"
                    ])
        , testSession "extend all constructors with sibling" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "data Foo"
                    , "data Bar"
                    , "data A = B | C"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA ( Foo,  A (C) , Bar ) "
                    , "f = B"
                    ])
            (Range (Position 2 4) (Position 2 5))
            [ "Add A(..) to the import list of ModuleA"
            , "Add A(B) to the import list of ModuleA"
            ]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA ( Foo,  A (..) , Bar ) "
                    , "f = B"
                    ])
        , testSession "extend all constructors with comment" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "data Foo"
                    , "data Bar"
                    , "data A = B | C"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA ( Foo,  A (C{-comment--}) , Bar ) "
                    , "f = B"
                    ])
            (Range (Position 2 4) (Position 2 5))
            [ "Add A(..) to the import list of ModuleA"
            , "Add A(B) to the import list of ModuleA"
            ]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA ( Foo,  A (..{-comment--}) , Bar ) "
                    , "f = B"
                    ])
        , testSession "extend all constructors for type operator" $ template
            []
            ("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "import Data.Type.Equality ((:~:))"
                    , "x :: (:~:) [] []"
                    , "x = Refl"
                    ])
            (Range (Position 3 17) (Position 3 18))
            [ "Add (:~:)(..) to the import list of Data.Type.Equality"
            , "Add type (:~:)(Refl) to the import list of Data.Type.Equality"]
            (T.unlines
                    [ "module ModuleA where"
                    , "import Data.Type.Equality ((:~:) (..))"
                    , "x :: (:~:) [] []"
                    , "x = Refl"
                    ])
        , testSession "extend all constructors for class" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "class C a where"
                    , "  m1 :: a -> a"
                    , "  m2 :: a -> a"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (C(m1))"
                    , "b = m2"
                    ])
            (Range (Position 2 5) (Position 2 5))
            [ "Add C(..) to the import list of ModuleA"
            , "Add C(m2) to the import list of ModuleA"
            , "Add m2 to the import list of ModuleA"
            ]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (C(..))"
                    , "b = m2"
                    ])
        , testSession "extend single line import with value" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "stuffA :: Double"
                    , "stuffA = 0.00750"
                    , "stuffB :: Integer"
                    , "stuffB = 123"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA as A (stuffB)"
                    , "main = print (stuffA, stuffB)"
                    ])
            (Range (Position 2 17) (Position 2 18))
            ["Add stuffA to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA as A (stuffB, stuffA)"
                    , "main = print (stuffA, stuffB)"
                    ])
        , testSession "extend single line import with operator" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "(.*) :: Integer -> Integer -> Integer"
                    , "x .* y = x * y"
                    , "stuffB :: Integer"
                    , "stuffB = 123"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA as A (stuffB)"
                    , "main = print (stuffB .* stuffB)"
                    ])
            (Range (Position 2 17) (Position 2 18))
            ["Add (.*) to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA as A (stuffB, (.*))"
                    , "main = print (stuffB .* stuffB)"
                    ])
        , testSession "extend single line import with infix constructor" $ template
            []
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import Data.List.NonEmpty (fromList)"
                    , "main = case (fromList []) of _ :| _ -> pure ()"
                    ])
            (Range (Position 2 5) (Position 2 6))
            [ "Add NonEmpty((:|)) to the import list of Data.List.NonEmpty"
            , "Add NonEmpty(..) to the import list of Data.List.NonEmpty"
            ]
            (T.unlines
                    [ "module ModuleB where"
                    , "import Data.List.NonEmpty (fromList, NonEmpty ((:|)))"
                    , "main = case (fromList []) of _ :| _ -> pure ()"
                    ])
        , testSession "extend single line import with prefix constructor" $ template
            []
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import Prelude hiding (Maybe(..))"
                    , "import Data.Maybe (catMaybes)"
                    , "x = Just 10"
                    ])
            (Range (Position 3 5) (Position 2 6))
            [ "Add Maybe(Just) to the import list of Data.Maybe"
            , "Add Maybe(..) to the import list of Data.Maybe"
            ]
            (T.unlines
                    [ "module ModuleB where"
                    , "import Prelude hiding (Maybe(..))"
                    , "import Data.Maybe (catMaybes, Maybe (Just))"
                    , "x = Just 10"
                    ])
        , testSession "extend single line import with type" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "type A = Double"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA ()"
                    , "b :: A"
                    , "b = 0"
                    ])
            (Range (Position 2 5) (Position 2 5))
            ["Add A to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A)"
                    , "b :: A"
                    , "b = 0"
                    ])
        , testSession "extend single line import with constructor" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "data A = Constructor"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A)"
                    , "b :: A"
                    , "b = Constructor"
                    ])
            (Range (Position 3 5) (Position 3 5))
            [ "Add A(Constructor) to the import list of ModuleA"
            , "Add A(..) to the import list of ModuleA"
            ]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A (Constructor))"
                    , "b :: A"
                    , "b = Constructor"
                    ])
        , testSession "extend single line import with constructor (with comments)" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "data A = Constructor"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A ({-Constructor-}))"
                    , "b :: A"
                    , "b = Constructor"
                    ])
            (Range (Position 3 5) (Position 3 5))
            [ "Add A(Constructor) to the import list of ModuleA"
            , "Add A(..) to the import list of ModuleA"
            ]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A (Constructor{-Constructor-}))"
                    , "b :: A"
                    , "b = Constructor"
                    ])
        , testSession "extend single line import with mixed constructors" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "data A = ConstructorFoo | ConstructorBar"
                    , "a = 1"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A (ConstructorBar), a)"
                    , "b :: A"
                    , "b = ConstructorFoo"
                    ])
            (Range (Position 3 5) (Position 3 5))
            [ "Add A(ConstructorFoo) to the import list of ModuleA"
            , "Add A(..) to the import list of ModuleA"
            ]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (A (ConstructorBar, ConstructorFoo), a)"
                    , "b :: A"
                    , "b = ConstructorFoo"
                    ])
        , brokenForGHC94 "On GHC 9.4, the error messages with -fdefer-type-errors don't have necessary imported target srcspan info." $
          testSession "extend single line qualified import with value" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "stuffA :: Double"
                    , "stuffA = 0.00750"
                    , "stuffB :: Integer"
                    , "stuffB = 123"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import qualified ModuleA as A (stuffB)"
                    , "main = print (A.stuffA, A.stuffB)"
                    ])
            (Range (Position 2 17) (Position 2 18))
            ["Add stuffA to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import qualified ModuleA as A (stuffB, stuffA)"
                    , "main = print (A.stuffA, A.stuffB)"
                    ])
        , testSession "extend multi line import with value" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "stuffA :: Double"
                    , "stuffA = 0.00750"
                    , "stuffB :: Integer"
                    , "stuffB = 123"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (stuffB"
                    , "               )"
                    , "main = print (stuffA, stuffB)"
                    ])
            (Range (Position 3 17) (Position 3 18))
            ["Add stuffA to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (stuffB, stuffA"
                    , "               )"
                    , "main = print (stuffA, stuffB)"
                    ])
        , testSession "extend multi line import with trailing comma" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "stuffA :: Double"
                    , "stuffA = 0.00750"
                    , "stuffB :: Integer"
                    , "stuffB = 123"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (stuffB,"
                    , "               )"
                    , "main = print (stuffA, stuffB)"
                    ])
            (Range (Position 3 17) (Position 3 18))
            ["Add stuffA to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (stuffB, stuffA,"
                    , "               )"
                    , "main = print (stuffA, stuffB)"
                    ])
        , testSession "extend single line import with method within class" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "class C a where"
                    , "  m1 :: a -> a"
                    , "  m2 :: a -> a"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (C(m1))"
                    , "b = m2"
                    ])
            (Range (Position 2 5) (Position 2 5))
            [ "Add C(m2) to the import list of ModuleA"
            , "Add m2 to the import list of ModuleA"
            , "Add C(..) to the import list of ModuleA"
            ]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (C(m1, m2))"
                    , "b = m2"
                    ])
        , testSession "extend single line import with method without class" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "class C a where"
                    , "  m1 :: a -> a"
                    , "  m2 :: a -> a"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (C(m1))"
                    , "b = m2"
                    ])
            (Range (Position 2 5) (Position 2 5))
            [ "Add m2 to the import list of ModuleA"
            , "Add C(m2) to the import list of ModuleA"
            , "Add C(..) to the import list of ModuleA"
            ]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA (C(m1), m2)"
                    , "b = m2"
                    ])
        , testSession "extend import list with multiple choices" $ template
            [("ModuleA.hs", T.unlines
                    --  this is just a dummy module to help the arguments needed for this test
                    [  "module ModuleA (bar) where"
                    , "bar = 10"
                    ]),
            ("ModuleB.hs", T.unlines
                    --  this is just a dummy module to help the arguments needed for this test
                    [  "module ModuleB (bar) where"
                    , "bar = 10"
                    ])]
            ("ModuleC.hs", T.unlines
                    [ "module ModuleC where"
                    , "import ModuleB ()"
                    , "import ModuleA ()"
                    , "foo = bar"
                    ])
            (Range (Position 3 17) (Position 3 18))
            ["Add bar to the import list of ModuleA",
            "Add bar to the import list of ModuleB"]
            (T.unlines
                    [ "module ModuleC where"
                    , "import ModuleB ()"
                    , "import ModuleA (bar)"
                    , "foo = bar"
                    ])
        , testSession "extend import list with constructor of type operator" $ template
            []
            ("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "import Data.Type.Equality ((:~:))"
                    , "x :: (:~:) [] []"
                    , "x = Refl"
                    ])
            (Range (Position 3 17) (Position 3 18))
            [ "Add type (:~:)(Refl) to the import list of Data.Type.Equality"
            , "Add (:~:)(..) to the import list of Data.Type.Equality"]
            (T.unlines
                    [ "module ModuleA where"
                    , "import Data.Type.Equality ((:~:) (Refl))"
                    , "x :: (:~:) [] []"
                    , "x = Refl"
                    ])
        , expectFailBecause "importing pattern synonyms is unsupported"
          $ testSession "extend import list with pattern synonym" $ template
            [("ModuleA.hs", T.unlines
                    [ "{-# LANGUAGE PatternSynonyms #-}"
                      , "module ModuleA where"
                      , "pattern Some x = Just x"
                    ])
            ]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import A ()"
                    , "k (Some x) = x"
                    ])
            (Range (Position 2 3) (Position 2 7))
            ["Add pattern Some to the import list of A"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import A (pattern Some)"
                    , "k (Some x) = x"
                    ])
        , ignoreForGhcVersions [GHC92, GHC94] "Diagnostic message has no suggestions" $
          testSession "type constructor name same as data constructor name" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "newtype Foo = Foo Int"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA(Foo)"
                    , "f :: Foo"
                    , "f = Foo 1"
                    ])
            (Range (Position 3 4) (Position 3 6))
            ["Add Foo(Foo) to the import list of ModuleA", "Add Foo(..) to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA(Foo (Foo))"
                    , "f :: Foo"
                    , "f = Foo 1"
                    ])
        , testSession "type constructor name same as data constructor name, data constructor extraneous" $ template
            [("ModuleA.hs", T.unlines
                    [ "module ModuleA where"
                    , "data Foo = Foo"
                    ])]
            ("ModuleB.hs", T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA()"
                    , "f :: Foo"
                    , "f = undefined"
                    ])
            (Range (Position 2 4) (Position 2 6))
            ["Add Foo to the import list of ModuleA"]
            (T.unlines
                    [ "module ModuleB where"
                    , "import ModuleA(Foo)"
                    , "f :: Foo"
                    , "f = undefined"
                    ])
        ]
      where
        codeActionTitle CodeAction{_title=x} = x

        template setUpModules moduleUnderTest range expectedTitles expectedContentB = do
            configureCheckProject overrideCheckProject

            mapM_ (\x -> createDoc (fst x) "haskell" (snd x)) setUpModules
            docB <- createDoc (fst moduleUnderTest) "haskell" (snd moduleUnderTest)
            _  <- waitForDiagnostics
            waitForProgressDone
            actionsOrCommands <- getCodeActions docB range
            let codeActions =
                  filter
                    (liftA2 (&&) (T.isPrefixOf "Add") (not . T.isPrefixOf "Add argument") . codeActionTitle)
                    [ca | InR ca <- actionsOrCommands]
                actualTitles = codeActionTitle <$> codeActions
            -- Note that we are not testing the order of the actions, as the
            -- order of the expected actions indicates which one we'll execute
            -- in this test, i.e., the first one.
            liftIO $ sort expectedTitles @=? sort actualTitles

            -- Execute the action with the same title as the first expected one.
            -- Since we tested that both lists have the same elements (possibly
            -- in a different order), this search cannot fail.
            let firstTitle:_ = expectedTitles
                action = fromJust $
                  find ((firstTitle ==) . codeActionTitle) codeActions
            executeCodeAction action
            contentAfterAction <- documentContents docB
            liftIO $ expectedContentB @=? contentAfterAction

fixModuleImportTypoTests :: TestTree
fixModuleImportTypoTests = testGroup "fix module import typo"
    [ testSession "works when single module suggested" $ do
        doc <- createDoc "A.hs" "haskell" "import Data.Cha"
        _ <- waitForDiagnostics
        InR action@CodeAction { _title = actionTitle } : _  <- getCodeActions doc (R 0 0 0 10)
        liftIO $ actionTitle @?= "replace with Data.Char"
        executeCodeAction action
        contentAfterAction <- documentContents doc
        liftIO $ contentAfterAction @?= "import Data.Char"
    , testSession "works when multiple modules suggested" $ do
        doc <- createDoc "A.hs" "haskell" "import Data.I"
        _ <- waitForDiagnostics
        actions <- sortOn (\(InR CodeAction{_title=x}) -> x) <$> getCodeActions doc (R 0 0 0 10)
        let actionTitles = [ title | InR CodeAction{_title=title} <- actions ]
        liftIO $ actionTitles @?= [ "replace with Data.Eq"
                                  , "replace with Data.Int"
                                  , "replace with Data.Ix"
                                  ]
        let InR replaceWithDataEq : _ = actions
        executeCodeAction replaceWithDataEq
        contentAfterAction <- documentContents doc
        liftIO $ contentAfterAction @?= "import Data.Eq"
    ]

suggestImportClassMethodTests :: TestTree
suggestImportClassMethodTests =
  testGroup
    "suggest import class methods"
    [ testGroup
        "new"
        [ testSession "via parent" $
            template'
            "import Data.Semigroup (Semigroup(stimes))"
            (Range (Position 4 2) (Position 4 8)),
          testSession "top level" $
            template'
              "import Data.Semigroup (stimes)"
              (Range (Position 4 2) (Position 4 8)),
          testSession "all" $
            template'
              "import Data.Semigroup"
              (Range (Position 4 2) (Position 4 8))
        ],
      testGroup
        "extend"
        [ testSession "via parent" $
            template
              [ "module A where",
                "",
                "import Data.Semigroup ()"
              ]
              (Range (Position 6 2) (Position 6 8))
              "Add Semigroup(stimes) to the import list of Data.Semigroup"
              [ "module A where",
                "",
                "import Data.Semigroup (Semigroup (stimes))"
              ],
          testSession "top level" $
            template
              [ "module A where",
                "",
                "import Data.Semigroup ()"
              ]
              (Range (Position 6 2) (Position 6 8))
              "Add stimes to the import list of Data.Semigroup"
              [ "module A where",
                "",
                "import Data.Semigroup (stimes)"
              ]
        ]
    ]
  where
    decls =
      [ "data X = X",
        "instance Semigroup X where",
        "  (<>) _ _ = X",
        "  stimes _ _ = X"
      ]
    template beforeContent range executeTitle expectedContent = do
      doc <- createDoc "A.hs" "haskell" $ T.unlines (beforeContent <> decls)
      _ <- waitForDiagnostics
      waitForProgressDone
      actions <- getCodeActions doc range
      let actions' = [x | InR x <- actions]
          titles = [_title | CodeAction {_title} <- actions']
      liftIO $ executeTitle `elem` titles @? T.unpack executeTitle <> " does not in " <> show titles
      executeCodeAction $ fromJust $ find (\CodeAction {_title} -> _title == executeTitle) actions'
      content <- documentContents doc
      liftIO $ T.unlines (expectedContent <> decls) @=? content
    template' executeTitle range = let c = ["module A where"] in template c range executeTitle $ c <> [executeTitle]

suggestImportTests :: TestTree
suggestImportTests = testGroup "suggest import actions"
  [ testGroup "Dont want suggestion"
    [ -- extend import
      test False ["Data.List.NonEmpty ()"] "f = nonEmpty" []                "import Data.List.NonEmpty (nonEmpty)"
      -- data constructor
    , test False []                        "f = First"    []                "import Data.Monoid (First)"
      -- internal module
    , test False []         "f :: Typeable a => a"        ["f = undefined"] "import Data.Typeable.Internal (Typeable)"
      -- package not in scope
    , test False []         "f = quickCheck"              []                "import Test.QuickCheck (quickCheck)"
      -- don't omit the parent data type of a constructor
    , test False []         "f ExitSuccess = ()"          []                "import System.Exit (ExitSuccess)"
      -- don't suggest data constructor when we only need the type
    , test False []         "f :: Bar"                    []                "import Bar (Bar(Bar))"
      -- don't suggest all data constructors for the data type
    , test False []         "f :: Bar"                    []                "import Bar (Bar(..))"
    ]
  , testGroup "want suggestion"
    [ wantWait  []          "f = foo"                     []                "import Foo (foo)"
    , wantWait  []          "f = Bar"                     []                "import Bar (Bar(Bar))"
    , wantWait  []          "f :: Bar"                    []                "import Bar (Bar)"
    , wantWait  []          "f = Bar"                     []                "import Bar (Bar(..))"
    , test True []          "f = nonEmpty"                []                "import Data.List.NonEmpty (nonEmpty)"
    , test True []          "f = (:|)"                    []                "import Data.List.NonEmpty (NonEmpty((:|)))"
    , test True []          "f :: Natural"                ["f = undefined"] "import Numeric.Natural (Natural)"
    , test True []          "f :: Natural"                ["f = undefined"] "import Numeric.Natural"
    , test True []          "f :: NonEmpty ()"            ["f = () :| []"]  "import Data.List.NonEmpty (NonEmpty)"
    , test True []          "f :: NonEmpty ()"            ["f = () :| []"]  "import Data.List.NonEmpty"
    , test True []          "f = First"                   []                "import Data.Monoid (First(First))"
    , test True []          "f = Endo"                    []                "import Data.Monoid (Endo(Endo))"
    , test True []          "f = Version"                 []                "import Data.Version (Version(Version))"
    , test True []          "f ExitSuccess = ()"          []                "import System.Exit (ExitCode(ExitSuccess))"
    , test True []          "f = AssertionFailed"         []                "import Control.Exception (AssertionFailed(AssertionFailed))"
    , test True ["Prelude"] "f = nonEmpty"                []                "import Data.List.NonEmpty (nonEmpty)"
    , test True []          "f :: Alternative f => f ()"  ["f = undefined"] "import Control.Applicative (Alternative)"
    , test True []          "f :: Alternative f => f ()"  ["f = undefined"] "import Control.Applicative"
    , test True []          "f = empty"                   []                "import Control.Applicative (Alternative(empty))"
    , test True []          "f = empty"                   []                "import Control.Applicative (empty)"
    , test True []          "f = empty"                   []                "import Control.Applicative"
    , test True []          "f = (&)"                     []                "import Data.Function ((&))"
    , test True []          "f = NE.nonEmpty"             []                "import qualified Data.List.NonEmpty as NE"
    , test True []          "f = (NE.:|)"                 []                "import qualified Data.List.NonEmpty as NE"
    , test True []          "f = (Data.List.NonEmpty.:|)" []                "import qualified Data.List.NonEmpty"
    , test True []          "f = (B..|.)"                 []                "import qualified Data.Bits as B"
    , test True []          "f = (Data.Bits..|.)"         []                "import qualified Data.Bits"
    , test True []          "f :: Typeable a => a"        ["f = undefined"] "import Data.Typeable (Typeable)"
    , test True []          "f = pack"                    []                "import Data.Text (pack)"
    , test True []          "f :: Text"                   ["f = undefined"] "import Data.Text (Text)"
    , test True []          "f = [] & id"                 []                "import Data.Function ((&))"
    , test True []          "f = (&) [] id"               []                "import Data.Function ((&))"
    , test True []          "f = (.|.)"                   []                "import Data.Bits (Bits((.|.)))"
    , test True []          "f = (.|.)"                   []                "import Data.Bits ((.|.))"
    , test True []          "f :: a ~~ b"                 []                "import Data.Type.Equality ((~~))"
    , test True
      ["qualified Data.Text as T"
      ]                     "f = T.putStrLn"              []                "import qualified Data.Text.IO as T"
    , test True
      [ "qualified Data.Text as T"
      , "qualified Data.Function as T"
      ]                     "f = T.putStrLn"              []                "import qualified Data.Text.IO as T"
    , test True
      [ "qualified Data.Text as T"
      , "qualified Data.Function as T"
      , "qualified Data.Functor as T"
      , "qualified Data.Data as T"
      ]                     "f = T.putStrLn"              []                "import qualified Data.Text.IO as T"
    , test True []          "f = (.|.)"                   []                "import Data.Bits (Bits(..))"
    , test True []          "f = empty"                   []                "import Control.Applicative (Alternative(..))"
    ]
  , expectFailBecause "importing pattern synonyms is unsupported" $ test True [] "k (Some x) = x" [] "import B (pattern Some)"
  ]
  where
    test = test' False
    wantWait = test' True True

    test' waitForCheckProject wanted imps def other newImp = testSessionWithExtraFiles "hover" (T.unpack def) $ \dir -> do
      configureCheckProject waitForCheckProject
      let before = T.unlines $ "module A where" : ["import " <> x | x <- imps] ++ def : other
          after  = T.unlines $ "module A where" : ["import " <> x | x <- imps] ++ [newImp] ++ def : other
          cradle = "cradle: {direct: {arguments: [-hide-all-packages, -package, base, -package, text, -package-env, -, A, Bar, Foo, B]}}"
      liftIO $ writeFileUTF8 (dir </> "hie.yaml") cradle
      liftIO $ writeFileUTF8 (dir </> "B.hs") $ unlines ["{-# LANGUAGE PatternSynonyms #-}", "module B where", "pattern Some x = Just x"]
      doc <- createDoc "Test.hs" "haskell" before
      waitForProgressDone
      _ <- waitForDiagnostics
      -- there isn't a good way to wait until the whole project is checked atm
      when waitForCheckProject $ liftIO $ sleep 0.5
      let defLine = fromIntegral $ length imps + 1
          range = Range (Position defLine 0) (Position defLine maxBound)
      actions <- getCodeActions doc range
      if wanted
         then do
             action <- liftIO $ pickActionWithTitle newImp actions
             executeCodeAction action
             contentAfterAction <- documentContents doc
             liftIO $ after @=? contentAfterAction
          else
              liftIO $ [_title | InR CodeAction{_title} <- actions, _title == newImp ] @?= []

suggestAddRecordFieldImportTests :: TestTree
suggestAddRecordFieldImportTests = testGroup "suggest imports of record fields when using OverloadedRecordDot"
  [ testGroup "The field is suggested when an instance resolution failure occurs"
    [ ignoreForGhcVersions [GHC90, GHC94, GHC96] "Extension not present <9.2, and the assist is derived from the help message in >=9.4" theTest
    ]
  ]
  where
    theTest = testSessionWithExtraFiles "hover" def $ \dir -> do
      configureCheckProject False
      let before = T.unlines $ "module A where" : ["import B (Foo)", "getFoo :: Foo -> Int", "getFoo x = x.foo"]
          after  = T.unlines $ "module A where" : ["import B (Foo, foo)", "getFoo :: Foo -> Int", "getFoo x = x.foo"]
          cradle = "cradle: {direct: {arguments: [-hide-all-packages, -package, base, -package, text, -package-env, -, A, B]}}"
      liftIO $ writeFileUTF8 (dir </> "hie.yaml") cradle
      liftIO $ writeFileUTF8 (dir </> "B.hs") $ unlines ["module B where", "data Foo = Foo { foo :: Int }"]
      doc <- createDoc "Test.hs" "haskell" before
      waitForProgressDone
      _ <- waitForDiagnostics
      let defLine = fromIntegral $ 1 + 2
          range = Range (Position defLine 0) (Position defLine maxBound)
      actions <- getCodeActions doc range
      action <- liftIO $ pickActionWithTitle "Add foo to the import list of B" actions
      executeCodeAction action
      contentAfterAction <- documentContents doc
      liftIO $ after @=? contentAfterAction


suggestImportDisambiguationTests :: TestTree
suggestImportDisambiguationTests = testGroup "suggest import disambiguation actions"
  [ testGroup "Hiding strategy works"
    [ testGroup "fromList"
        [ testCase "AVec" $
            compareHideFunctionTo [(8,9),(10,8)]
                "Use AVec for fromList, hiding other imports"
                "HideFunction.expected.fromList.A.hs"
        , testCase "BVec" $
            compareHideFunctionTo [(8,9),(10,8)]
                "Use BVec for fromList, hiding other imports"
                "HideFunction.expected.fromList.B.hs"
        ]
    , testGroup "(++)"
        [ testCase "EVec" $
            compareHideFunctionTo [(8,9),(10,8)]
                "Use EVec for ++, hiding other imports"
                "HideFunction.expected.append.E.hs"
        , testCase "Hide functions without local" $
            compareTwo
                "HideFunctionWithoutLocal.hs" [(8,8)]
                "Use local definition for ++, hiding other imports"
                "HideFunctionWithoutLocal.expected.hs"
        , testCase "Prelude" $
            compareHideFunctionTo [(8,9),(10,8)]
                "Use Prelude for ++, hiding other imports"
                "HideFunction.expected.append.Prelude.hs"
        , testCase "Prelude and local definition, infix" $
            compareTwo
                "HidePreludeLocalInfix.hs" [(2,19)]
                "Use local definition for ++, hiding other imports"
                "HidePreludeLocalInfix.expected.hs"
        , testCase "AVec, indented" $
            compareTwo "HidePreludeIndented.hs" [(3,8)]
            "Use AVec for ++, hiding other imports"
            "HidePreludeIndented.expected.hs"

        ]
    , testGroup "Vec (type)"
        [ testCase "AVec" $
            compareTwo
                "HideType.hs" [(8,15)]
                "Use AVec for Vec, hiding other imports"
                "HideType.expected.A.hs"
        , testCase "EVec" $
            compareTwo
                "HideType.hs" [(8,15)]
                "Use EVec for Vec, hiding other imports"
                "HideType.expected.E.hs"
        ]
    ]
  , testGroup "Qualify strategy"
    [ testCase "won't suggest full name for qualified module" $
      withHideFunction [(8,9),(10,8)] $ \_ _ actions -> do
        liftIO $
            assertBool "EVec.fromList must not be suggested" $
                "Replace with qualified: EVec.fromList" `notElem`
                [ actionTitle
                | InR CodeAction { _title = actionTitle } <- actions
                ]
        liftIO $
            assertBool "EVec.++ must not be suggested" $
                "Replace with qualified: EVec.++" `notElem`
                [ actionTitle
                | InR CodeAction { _title = actionTitle } <- actions
                ]
    , testGroup "fromList"
        [ testCase "EVec" $
            compareHideFunctionTo [(8,9),(10,8)]
                "Replace with qualified: E.fromList"
                "HideFunction.expected.qualified.fromList.E.hs"
        , testCase "Hide DuplicateRecordFields" $
            compareTwo
                "HideQualifyDuplicateRecordFields.hs" [(9, 9)]
                "Replace with qualified: AVec.fromList"
                "HideQualifyDuplicateRecordFields.expected.hs"
        , testCase "Duplicate record fields should not be imported" $ do
          withTarget ("HideQualifyDuplicateRecordFields" <.> ".hs") [(9, 9)] $
            \_ _ actions -> do
              liftIO $
                assertBool "Hidings should not be presented while DuplicateRecordFields exists" $
                  all not [ actionTitle =~ T.pack "Use ([A-Za-z][A-Za-z0-9]*) for fromList, hiding other imports"
                      | InR CodeAction { _title = actionTitle } <- actions]
          withTarget ("HideQualifyDuplicateRecordFieldsSelf" <.> ".hs") [(4, 4)] $
            \_ _ actions -> do
              liftIO $
                assertBool "ambiguity from DuplicateRecordFields should not be imported" $
                  null actions
        ]
    , testGroup "(++)"
        [ testCase "Prelude, parensed" $
            compareHideFunctionTo [(8,9),(10,8)]
                "Replace with qualified: Prelude.++"
                "HideFunction.expected.qualified.append.Prelude.hs"
        , testCase "Prelude, infix" $
            compareTwo
                "HideQualifyInfix.hs" [(4,19)]
                "Replace with qualified: Prelude.++"
                "HideQualifyInfix.expected.hs"
        , testCase "Prelude, left section" $
            compareTwo
                "HideQualifySectionLeft.hs" [(4,15)]
                "Replace with qualified: Prelude.++"
                "HideQualifySectionLeft.expected.hs"
        , testCase "Prelude, right section" $
            compareTwo
                "HideQualifySectionRight.hs" [(4,18)]
                "Replace with qualified: Prelude.++"
                "HideQualifySectionRight.expected.hs"
        ]
    ]
  ]
  where
    compareTwo original locs cmd expected =
        withTarget original locs $ \dir doc actions -> do
            expected <- liftIO $
                readFileUtf8 (dir </> expected)
            action <- liftIO $ pickActionWithTitle cmd actions
            executeCodeAction action
            contentAfterAction <- documentContents doc
            liftIO $ T.replace "\r\n" "\n" expected @=? contentAfterAction
    compareHideFunctionTo = compareTwo "HideFunction.hs"
    auxFiles = ["AVec.hs", "BVec.hs", "CVec.hs", "DVec.hs", "EVec.hs", "FVec.hs"]
    withTarget file locs k = runWithExtraFiles "hiding" $ \dir -> do
        doc <- openDoc file "haskell"
        void $ expectDiagnostics [(file, [(DiagnosticSeverity_Error, loc, "Ambiguous occurrence") | loc <- locs])]
        actions <- getAllCodeActions doc
        k dir doc actions
    withHideFunction = withTarget ("HideFunction" <.> "hs")

suggestHideShadowTests :: TestTree
suggestHideShadowTests =
  testGroup
    "suggest hide shadow"
    [ testGroup
        "single"
        [ testOneCodeAction
            "hide unused"
            "Hide on from Data.Function"
            (1, 2)
            (1, 4)
            [ "import Data.Function"
            , "f on = on"
            , "g on = on"
            ]
            [ "import Data.Function hiding (on)"
            , "f on = on"
            , "g on = on"
            ]
        , testOneCodeAction
            "extend hiding unused"
            "Hide on from Data.Function"
            (1, 2)
            (1, 4)
            [ "import Data.Function hiding ((&))"
            , "f on = on"
            ]
            [ "import Data.Function hiding (on, (&))"
            , "f on = on"
            ]
        , testOneCodeAction
            "delete unused"
            "Hide on from Data.Function"
            (1, 2)
            (1, 4)
            [ "import Data.Function ((&), on)"
            , "f on = on"
            ]
            [ "import Data.Function ((&))"
            , "f on = on"
            ]
        , testOneCodeAction
            "hide operator"
            "Hide & from Data.Function"
            (1, 2)
            (1, 5)
            [ "import Data.Function"
            , "f (&) = (&)"
            ]
            [ "import Data.Function hiding ((&))"
            , "f (&) = (&)"
            ]
        , testOneCodeAction
            "remove operator"
            "Hide & from Data.Function"
            (1, 2)
            (1, 5)
            [ "import Data.Function ((&), on)"
            , "f (&) = (&)"
            ]
            [ "import Data.Function ( on)"
            , "f (&) = (&)"
            ]
        , noCodeAction
            "don't remove already used"
            (2, 2)
            (2, 4)
            [ "import Data.Function"
            , "g = on"
            , "f on = on"
            ]
        ]
    , testGroup
        "multi"
        [ testOneCodeAction
            "hide from B"
            "Hide ++ from B"
            (2, 2)
            (2, 6)
            [ "import B"
            , "import C"
            , "f (++) = (++)"
            ]
            [ "import B hiding ((++))"
            , "import C"
            , "f (++) = (++)"
            ]
        , testOneCodeAction
            "hide from C"
            "Hide ++ from C"
            (2, 2)
            (2, 6)
            [ "import B"
            , "import C"
            , "f (++) = (++)"
            ]
            [ "import B"
            , "import C hiding ((++))"
            , "f (++) = (++)"
            ]
        , testOneCodeAction
            "hide from Prelude"
            "Hide ++ from Prelude"
            (2, 2)
            (2, 6)
            [ "import B"
            , "import C"
            , "f (++) = (++)"
            ]
            [ "import B"
            , "import C"
            , "import Prelude hiding ((++))"
            , "f (++) = (++)"
            ]
        , testMultiCodeActions
            "manual hide all"
            [ "Hide ++ from Prelude"
            , "Hide ++ from C"
            , "Hide ++ from B"
            ]
            (2, 2)
            (2, 6)
            [ "import B"
            , "import C"
            , "f (++) = (++)"
            ]
            [ "import B hiding ((++))"
            , "import C hiding ((++))"
            , "import Prelude hiding ((++))"
            , "f (++) = (++)"
            ]
        , testOneCodeAction
            "auto hide all"
            "Hide ++ from all occurrence imports"
            (2, 2)
            (2, 6)
            [ "import B"
            , "import C"
            , "f (++) = (++)"
            ]
            [ "import B hiding ((++))"
            , "import C hiding ((++))"
            , "import Prelude hiding ((++))"
            , "f (++) = (++)"
            ]
        ]
    ]
 where
  testOneCodeAction testName actionName start end origin expected =
    helper testName start end origin expected $ \cas -> do
      action <- liftIO $ pickActionWithTitle actionName cas
      executeCodeAction action
  noCodeAction testName start end origin =
    helper testName start end origin origin $ \cas -> do
      liftIO $ cas @?= []
  testMultiCodeActions testName actionNames start end origin expected =
    helper testName start end origin expected $ \cas -> do
      let r = [ca | (InR ca) <- cas, ca ^. L.title `elem` actionNames]
      liftIO $
        (length r == length actionNames)
          @? "Expected " <> show actionNames <> ", but got " <> show cas <> " which is not its superset"
      forM_ r executeCodeAction
  helper testName (line1, col1) (line2, col2) origin expected k = testSession testName $ do
    void $ createDoc "B.hs" "haskell" $ T.unlines docB
    void $ createDoc "C.hs" "haskell" $ T.unlines docC
    doc <- createDoc "A.hs" "haskell" $ T.unlines (header <> origin)
    void waitForDiagnostics
    waitForProgressDone
    cas <- getCodeActions doc (Range (Position (fromIntegral $ line1 + length header) col1) (Position (fromIntegral $ line2 + length header) col2))
    void $ k [x | x@(InR ca) <- cas, "Hide" `T.isPrefixOf` (ca ^. L.title)]
    contentAfter <- documentContents doc
    liftIO $ contentAfter @?= T.unlines (header <> expected)
  header =
    [ "{-# OPTIONS_GHC -Wname-shadowing #-}"
    , "module A where"
    , ""
    ]
  -- for multi group
  docB =
    [ "module B where"
    , "(++) = id"
    ]
  docC =
    [ "module C where"
    , "(++) = id"
    ]

insertNewDefinitionTests :: TestTree
insertNewDefinitionTests = testGroup "insert new definition actions"
  [ testSession "insert new function definition" $ do
      let txtB =
            ["foo True = select [True]"
            , ""
            ,"foo False = False"
            ]
          txtB' =
            [""
            ,"someOtherCode = ()"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" (T.unlines $ txtB ++ txtB')
      _ <- waitForDiagnostics
      InR action@CodeAction { _title = actionTitle } : _
                  <- filter (\(InR CodeAction{_title=x}) -> "Define" `T.isPrefixOf` x) <$>
                     getCodeActions docB (R 0 0 0 50)
      liftIO $ actionTitle @?= "Define select :: [Bool] -> Bool"
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ contentAfterAction @?= T.unlines (txtB ++
        [ ""
        , "select :: [Bool] -> Bool"
        , "select = _"
        ]
        ++ txtB')
  , testSession "define a hole" $ do
      let txtB =
            ["foo True = _select [True]"
            , ""
            ,"foo False = False"
            ]
          txtB' =
            [""
            ,"someOtherCode = ()"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" (T.unlines $ txtB ++ txtB')
      _ <- waitForDiagnostics
      InR action@CodeAction { _title = actionTitle } : _
                  <- filter (\(InR CodeAction{_title=x}) -> "Define" `T.isPrefixOf` x) <$>
                     getCodeActions docB (R 0 0 0 50)
      liftIO $ actionTitle @?= "Define select :: [Bool] -> Bool"
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ contentAfterAction @?= T.unlines (
        ["foo True = select [True]"
        , ""
        ,"foo False = False"
        , ""
        , "select :: [Bool] -> Bool"
        , "select = _"
        ]
        ++ txtB')
  , testSession "insert new function definition - Haddock comments" $ do
    let start =  ["foo :: Int -> Bool"
                 , "foo x = select (x + 1)"
                 , ""
                 , "-- | This is a haddock comment"
                 , "haddock :: Int -> Int"
                 , "haddock = undefined"
                 ]
    let expected =  ["foo :: Int -> Bool"
             , "foo x = select (x + 1)"
             , ""
             , "select :: Int -> Bool"
             , "select = _"
             , ""
             , "-- | This is a haddock comment"
             , "haddock :: Int -> Int"
             , "haddock = undefined"]
    docB <- createDoc "ModuleB.hs" "haskell" (T.unlines start)
    _ <- waitForDiagnostics
    InR action@CodeAction { _title = actionTitle } : _
                <- filter (\(InR CodeAction{_title=x}) -> "Define" `T.isPrefixOf` x) <$>
                    getCodeActions docB (R 1 0 0 50)
    liftIO $ actionTitle @?= "Define select :: Int -> Bool"
    executeCodeAction action
    contentAfterAction <- documentContents docB
    liftIO $ contentAfterAction @?= T.unlines expected
  , testSession "insert new function definition - normal comments" $ do
    let start =  ["foo :: Int -> Bool"
                 , "foo x = select (x + 1)"
                 , ""
                 , "-- This is a normal comment"
                 , "normal :: Int -> Int"
                 , "normal = undefined"
                 ]
    let expected =  ["foo :: Int -> Bool"
             , "foo x = select (x + 1)"
             , ""
             , "select :: Int -> Bool"
             , "select = _"
             , ""
             , "-- This is a normal comment"
             , "normal :: Int -> Int"
             , "normal = undefined"]
    docB <- createDoc "ModuleB.hs" "haskell" (T.unlines start)
    _ <- waitForDiagnostics
    InR action@CodeAction { _title = actionTitle } : _
                <- filter (\(InR CodeAction{_title=x}) -> "Define" `T.isPrefixOf` x) <$>
                    getCodeActions docB (R 1 0 0 50)
    liftIO $ actionTitle @?= "Define select :: Int -> Bool"
    executeCodeAction action
    contentAfterAction <- documentContents docB
    liftIO $ contentAfterAction @?= T.unlines expected
    , testSession "insert new function definition - untyped error" $ do
      let txtB =
            ["foo = select"
            ]
          txtB' =
            [""
            ,"someOtherCode = ()"
            ]
      docB <- createDoc "ModuleB.hs" "haskell" (T.unlines $ txtB ++ txtB')
      _ <- waitForDiagnostics
      InR action@CodeAction { _title = actionTitle } : _
                  <- filter (\(InR CodeAction{_title=x}) -> "Define" `T.isPrefixOf` x) <$>
                     getCodeActions docB (R 0 0 0 50)
      liftIO $ actionTitle @?= "Define select :: _"
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ contentAfterAction @?= T.unlines (txtB ++
        [ ""
        , "select :: _"
        , "select = _"
        ]
        ++ txtB')
  ]

deleteUnusedDefinitionTests :: TestTree
deleteUnusedDefinitionTests = testGroup "delete unused definition action"
  [ testSession "delete unused top level binding" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
               , "module A (some) where"
               , ""
               , "f :: Int -> Int"
               , "f 1 = let a = 1"
               , "      in a"
               , "f 2 = 2"
               , ""
               , "some = ()"
               ])
    (4, 0)
    "Delete ‘f’"
    (T.unlines [
        "{-# OPTIONS_GHC -Wunused-top-binds #-}"
        , "module A (some) where"
        , ""
        , "some = ()"
        ])

  , testSession "delete unused top level binding defined in infix form" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
               , "module A (some) where"
               , ""
               , "myPlus :: Int -> Int -> Int"
               , "a `myPlus` b = a + b"
               , ""
               , "some = ()"
               ])
    (4, 2)
    "Delete ‘myPlus’"
    (T.unlines [
        "{-# OPTIONS_GHC -Wunused-top-binds #-}"
        , "module A (some) where"
        , ""
        , "some = ()"
      ])
  , testSession "delete unused binding in where clause" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (h, g) where"
               , ""
               , "h :: Int"
               , "h = 3"
               , ""
               , "g :: Int"
               , "g = 6"
               , "  where"
               , "    h :: Int"
               , "    h = 4"
               , ""
               ])
    (10, 4)
    "Delete ‘h’"
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (h, g) where"
               , ""
               , "h :: Int"
               , "h = 3"
               , ""
               , "g :: Int"
               , "g = 6"
               , "  where"
               , ""
               ])
  , testSession "delete unused binding with multi-oneline signatures front" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (b, c) where"
               , ""
               , "a, b, c :: Int"
               , "a = 3"
               , "b = 4"
               , "c = 5"
               ])
    (4, 0)
    "Delete ‘a’"
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (b, c) where"
               , ""
               , "b, c :: Int"
               , "b = 4"
               , "c = 5"
               ])
  , testSession "delete unused binding with multi-oneline signatures mid" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (a, c) where"
               , ""
               , "a, b, c :: Int"
               , "a = 3"
               , "b = 4"
               , "c = 5"
               ])
    (5, 0)
    "Delete ‘b’"
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (a, c) where"
               , ""
               , "a, c :: Int"
               , "a = 3"
               , "c = 5"
               ])
  , testSession "delete unused binding with multi-oneline signatures end" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (a, b) where"
               , ""
               , "a, b, c :: Int"
               , "a = 3"
               , "b = 4"
               , "c = 5"
               ])
    (6, 0)
    "Delete ‘c’"
    (T.unlines [ "{-# OPTIONS_GHC -Wunused-binds #-}"
               , "module A (a, b) where"
               , ""
               , "a, b :: Int"
               , "a = 3"
               , "b = 4"
               ])
  ]
  where
    testFor source pos expectedTitle expectedResult = do
      docId <- createDoc "A.hs" "haskell" source
      expectDiagnostics [ ("A.hs", [(DiagnosticSeverity_Warning, pos, "not used")]) ]

      (action, title) <- extractCodeAction docId "Delete" pos

      liftIO $ title @?= expectedTitle
      executeCodeAction action
      contentAfterAction <- documentContents docId
      liftIO $ contentAfterAction @?= expectedResult

    extractCodeAction docId actionPrefix (l, c) = do
      [action@CodeAction { _title = actionTitle }]  <- findCodeActionsByPrefix docId (R l c l c) [actionPrefix]
      return (action, actionTitle)

addTypeAnnotationsToLiteralsTest :: TestTree
addTypeAnnotationsToLiteralsTest = testGroup "add type annotations to literals to satisfy constraints"
  [
    testSession "add default type to satisfy one constraint" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A (f) where"
               , ""
               , "f = 1"
               ])
    (if ghcVersion >= GHC94
      then [ (DiagnosticSeverity_Warning, (3, 4), "Defaulting the type variable") ]
      else [ (DiagnosticSeverity_Warning, (3, 4), "Defaulting the following constraint") ])
    "Add type annotation ‘Integer’ to ‘1’"
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A (f) where"
               , ""
               , "f = (1 :: Integer)"
               ])

  , testSession "add default type to satisfy one constraint in nested expressions" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A where"
               , ""
               , "f ="
               , "    let x = 3"
               , "    in x"
               ])
    (if ghcVersion >= GHC94
      then [ (DiagnosticSeverity_Warning, (4, 12), "Defaulting the type variable") ]
      else [ (DiagnosticSeverity_Warning, (4, 12), "Defaulting the following constraint") ])
    "Add type annotation ‘Integer’ to ‘3’"
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A where"
               , ""
               , "f ="
               , "    let x = (3 :: Integer)"
               , "    in x"
               ])
  , testSession "add default type to satisfy one constraint in more nested expressions" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A where"
               , ""
               , "f ="
               , "    let x = let y = 5 in y"
               , "    in x"
               ])
    (if ghcVersion >= GHC94
      then [ (DiagnosticSeverity_Warning, (4, 20), "Defaulting the type variable") ]
      else [ (DiagnosticSeverity_Warning, (4, 20), "Defaulting the following constraint") ])
    "Add type annotation ‘Integer’ to ‘5’"
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "module A where"
               , ""
               , "f ="
               , "    let x = let y = (5 :: Integer) in y"
               , "    in x"
               ])
  , testSession "add default type to satisfy one constraint with duplicate literals" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f = seq \"debug\" traceShow \"debug\""
               ])
    (if ghcVersion >= GHC94
      then
        [ (DiagnosticSeverity_Warning, (6, 8), "Defaulting the type variable")
        , (DiagnosticSeverity_Warning, (6, 16), "Defaulting the type variable")
        ]
      else
        [ (DiagnosticSeverity_Warning, (6, 8), "Defaulting the following constraint")
        , (DiagnosticSeverity_Warning, (6, 16), "Defaulting the following constraint")
        ])
    ("Add type annotation ‘" <> listOfChar <> "’ to ‘\"debug\"’")
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f = seq (\"debug\" :: " <> listOfChar <> ") traceShow \"debug\""
               ])
  , testSession "add default type to satisfy two constraints" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f a = traceShow \"debug\" a"
               ])
    (if ghcVersion >= GHC94
      then [ (DiagnosticSeverity_Warning, (6, 6), "Defaulting the type variable") ]
      else [ (DiagnosticSeverity_Warning, (6, 6), "Defaulting the following constraint") ])
    ("Add type annotation ‘" <> listOfChar <> "’ to ‘\"debug\"’")
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f a = traceShow (\"debug\" :: " <> listOfChar <> ") a"
               ])
  , testSession "add default type to satisfy two constraints with duplicate literals" $
    testFor
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f = seq (\"debug\" :: [Char]) (seq (\"debug\" :: [Char]) (traceShow \"debug\"))"
               ])
    (if ghcVersion >= GHC94
      then [ (DiagnosticSeverity_Warning, (6, 54), "Defaulting the type variable") ]
      else [ (DiagnosticSeverity_Warning, (6, 54), "Defaulting the following constraint") ])
    ("Add type annotation ‘" <> listOfChar <> "’ to ‘\"debug\"’")
    (T.unlines [ "{-# OPTIONS_GHC -Wtype-defaults #-}"
               , "{-# LANGUAGE OverloadedStrings #-}"
               , "module A (f) where"
               , ""
               , "import Debug.Trace"
               , ""
               , "f = seq (\"debug\" :: [Char]) (seq (\"debug\" :: [Char]) (traceShow (\"debug\" :: " <> listOfChar <> ")))"
               ])
  ]
  where
    testFor source diag expectedTitle expectedResult = do
      docId <- createDoc "A.hs" "haskell" source
      expectDiagnostics [ ("A.hs", diag) ]

      let cursors = map snd3 diag
      (action, title) <- extractCodeAction docId "Add type annotation" (minimum cursors) (maximum cursors)

      liftIO $ title @?= expectedTitle
      executeCodeAction action
      contentAfterAction <- documentContents docId
      liftIO $ contentAfterAction @?= expectedResult

    extractCodeAction docId actionPrefix (l,c) (l', c')= do
      [action@CodeAction { _title = actionTitle }]  <- findCodeActionsByPrefix docId (R l c l' c') [actionPrefix]
      return (action, actionTitle)


fixConstructorImportTests :: TestTree
fixConstructorImportTests = testGroup "fix import actions"
  [ testSession "fix constructor import" $ template
      (T.unlines
            [ "module ModuleA where"
            , "data A = Constructor"
            ])
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA(Constructor)"
            ])
      (Range (Position 1 10) (Position 1 11))
      "Fix import of A(Constructor)"
      (T.unlines
            [ "module ModuleB where"
            , "import ModuleA(A(Constructor))"
            ])
  ]
  where
    template contentA contentB range expectedAction expectedContentB = do
      _docA <- createDoc "ModuleA.hs" "haskell" contentA
      docB  <- createDoc "ModuleB.hs" "haskell" contentB
      _diags <- waitForDiagnostics
      InR action@CodeAction { _title = actionTitle } : _
                  <- sortOn (\(InR CodeAction{_title=x}) -> x) <$>
                     getCodeActions docB range
      liftIO $ expectedAction @=? actionTitle
      executeCodeAction action
      contentAfterAction <- documentContents docB
      liftIO $ expectedContentB @=? contentAfterAction

importRenameActionTests :: TestTree
importRenameActionTests = testGroup "import rename actions"
  [ testSession "Data.Mape -> Data.Map"   $ check "Map"
  , testSession "Data.Mape -> Data.Maybe" $ check "Maybe" ] where
  check modname = do
      let content = T.unlines
            [ "module Testing where"
            , "import Data.Mape"
            ]
      doc <- createDoc "Testing.hs" "haskell" content
      _ <- waitForDiagnostics
      actionsOrCommands <- getCodeActions doc (Range (Position 1 8) (Position 1 16))
      let [changeToMap] = [action | InR action@CodeAction{ _title = actionTitle } <- actionsOrCommands, ("Data." <> modname) `T.isInfixOf` actionTitle ]
      executeCodeAction changeToMap
      contentAfterAction <- documentContents doc
      let expectedContentAfterAction = T.unlines
            [ "module Testing where"
            , "import Data." <> modname
            ]
      liftIO $ expectedContentAfterAction @=? contentAfterAction

fillTypedHoleTests :: TestTree
fillTypedHoleTests = let

  sourceCode :: T.Text -> T.Text -> T.Text -> T.Text
  sourceCode a b c = T.unlines
    [ "module Testing where"
      , ""
      , "globalConvert :: Int -> String"
      , "globalConvert = undefined"
      , ""
      , "globalInt :: Int"
      , "globalInt = 3"
      , ""
      , "bar :: Int -> Int -> String"
      , "bar n parameterInt = " <> a <> " (n + " <> b <> " + " <> c <> ")  where"
      , "  localConvert = (flip replicate) 'x'"
      , ""
      , "foo :: () -> Int -> String"
      , "foo = undefined"

    ]

  check :: T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle
        oldA oldB oldC
        newA newB newC = testSession (T.unpack actionTitle) $ do
    let originalCode = sourceCode oldA oldB oldC
    let expectedCode = sourceCode newA newB newC
    doc <- createDoc "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 9 0) (Position 9 maxBound))
    chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "fill typed holes"
  [ check "replace _ with show"
          "_"    "n" "n"
          "show" "n" "n"

  , check "replace _ with globalConvert"
          "_"             "n" "n"
          "globalConvert" "n" "n"

  , check "replace _convertme with localConvert"
          "_convertme"   "n" "n"
          "localConvert" "n" "n"

  , check "replace _b with globalInt"
          "_a" "_b"        "_c"
          "_a" "globalInt" "_c"

  , check "replace _c with globalInt"
          "_a" "_b"        "_c"
          "_a" "_b" "globalInt"

  , check "replace _c with parameterInt"
          "_a" "_b" "_c"
          "_a" "_b"  "parameterInt"
  , check "replace _ with foo _"
          "_" "n" "n"
          "(foo _)" "n" "n"
  , testSession "replace _toException with E.toException" $ do
      let mkDoc x = T.unlines
            [ "module Testing where"
            , "import qualified Control.Exception as E"
            , "ioToSome :: E.IOException -> E.SomeException"
            , "ioToSome = " <> x ]
      doc <- createDoc "Test.hs" "haskell" $ mkDoc "_toException"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc (Range (Position 3 0) (Position 3 maxBound))
      chosen <- liftIO $ pickActionWithTitle "replace _toException with E.toException" actions
      executeCodeAction chosen
      modifiedCode <- documentContents doc
      liftIO $ mkDoc "E.toException" @=? modifiedCode
  , testSession "filling infix type hole uses prefix notation" $ do
      let mkDoc x = T.unlines
              [ "module Testing where"
              , "data A = A"
              , "foo :: A -> A -> A"
              , "foo A A = A"
              , "test :: A -> A -> A"
              , "test a1 a2 = a1 " <> x <> " a2"
              ]
      doc <- createDoc "Test.hs" "haskell" $ mkDoc "`_`"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc (Range (Position 5 16) (Position 5 19))
      chosen <- liftIO $ pickActionWithTitle "replace _ with foo" actions
      executeCodeAction chosen
      modifiedCode <- documentContents doc
      liftIO $ mkDoc "`foo`" @=? modifiedCode
  , testSession "postfix hole uses postfix notation of infix operator" $ do
      let mkDoc x = T.unlines
              [ "module Testing where"
              , "test :: Int -> Int -> Int"
              , "test a1 a2 = " <> x <> " a1 a2"
              ]
      doc <- createDoc "Test.hs" "haskell" $ mkDoc "_"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc (Range (Position 2 13) (Position 2 14))
      chosen <- liftIO $ pickActionWithTitle "replace _ with (+)" actions
      executeCodeAction chosen
      modifiedCode <- documentContents doc
      liftIO $ mkDoc "(+)" @=? modifiedCode
  , testSession "filling infix type hole uses infix operator" $ do
      let mkDoc x = T.unlines
              [ "module Testing where"
              , "test :: Int -> Int -> Int"
              , "test a1 a2 = a1 " <> x <> " a2"
              ]
      doc <- createDoc "Test.hs" "haskell" $ mkDoc "`_`"
      _ <- waitForDiagnostics
      actions <- getCodeActions doc (Range (Position 2 16) (Position 2 19))
      chosen <- liftIO $ pickActionWithTitle "replace _ with (+)" actions
      executeCodeAction chosen
      modifiedCode <- documentContents doc
      liftIO $ mkDoc "+" @=? modifiedCode
  ]

addInstanceConstraintTests :: TestTree
addInstanceConstraintTests = let
  missingConstraintSourceCode :: Maybe T.Text -> T.Text
  missingConstraintSourceCode mConstraint =
    let constraint = maybe "" (<> " => ") mConstraint
     in T.unlines
    [ "module Testing where"
    , ""
    , "data Wrap a = Wrap a"
    , ""
    , "instance " <> constraint <> "Eq (Wrap a) where"
    , "  (Wrap x) == (Wrap y) = x == y"
    ]

  incompleteConstraintSourceCode :: Maybe T.Text -> T.Text
  incompleteConstraintSourceCode mConstraint =
    let constraint = maybe "Eq a" (\c -> "(Eq a, " <> c <> ")")  mConstraint
     in T.unlines
    [ "module Testing where"
    , ""
    , "data Pair a b = Pair a b"
    , ""
    , "instance " <> constraint <> " => Eq (Pair a b) where"
    , "  (Pair x y) == (Pair x' y') = x == x' && y == y'"
    ]

  incompleteConstraintSourceCode2 :: Maybe T.Text -> T.Text
  incompleteConstraintSourceCode2 mConstraint =
    let constraint = maybe "(Eq a, Eq b)" (\c -> "(Eq a, Eq b, " <> c <> ")")  mConstraint
     in T.unlines
    [ "module Testing where"
    , ""
    , "data Three a b c = Three a b c"
    , ""
    , "instance " <> constraint <> " => Eq (Three a b c) where"
    , "  (Three x y z) == (Three x' y' z') = x == x' && y == y' && z == z'"
    ]

  check :: T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle originalCode expectedCode = testSession (T.unpack actionTitle) $ do
    doc <- createDoc "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getAllCodeActions doc
    chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode

  in testGroup "add instance constraint"
  [ check
    "Add `Eq a` to the context of the instance declaration"
    (missingConstraintSourceCode Nothing)
    (missingConstraintSourceCode $ Just "Eq a")
  , check
    "Add `Eq b` to the context of the instance declaration"
    (incompleteConstraintSourceCode Nothing)
    (incompleteConstraintSourceCode $ Just "Eq b")
  , check
    "Add `Eq c` to the context of the instance declaration"
    (incompleteConstraintSourceCode2 Nothing)
    (incompleteConstraintSourceCode2 $ Just "Eq c")
  ]

addFunctionConstraintTests :: TestTree
addFunctionConstraintTests = let
  missingConstraintSourceCode :: T.Text -> T.Text
  missingConstraintSourceCode constraint =
    T.unlines
    [ "module Testing where"
    , ""
    , "eq :: " <> constraint <> "a -> a -> Bool"
    , "eq x y = x == y"
    ]

  missingConstraintWithForAllSourceCode :: T.Text -> T.Text
  missingConstraintWithForAllSourceCode constraint =
    T.unlines
    [ "{-# LANGUAGE ExplicitForAll #-}"
    , "module Testing where"
    , ""
    , "eq :: forall a. " <> constraint <> "a -> a -> Bool"
    , "eq x y = x == y"
    ]

  incompleteConstraintWithForAllSourceCode :: T.Text -> T.Text
  incompleteConstraintWithForAllSourceCode constraint =
    T.unlines
    [ "{-# LANGUAGE ExplicitForAll #-}"
    , "module Testing where"
    , ""
    , "data Pair a b = Pair a b"
    , ""
    , "eq :: " <> constraint <> " => Pair a b -> Pair a b -> Bool"
    , "eq (Pair x y) (Pair x' y') = x == x' && y == y'"
    ]

  incompleteConstraintSourceCode :: T.Text -> T.Text
  incompleteConstraintSourceCode constraint =
    T.unlines
    [ "module Testing where"
    , ""
    , "data Pair a b = Pair a b"
    , ""
    , "eq :: " <> constraint <> " => Pair a b -> Pair a b -> Bool"
    , "eq (Pair x y) (Pair x' y') = x == x' && y == y'"
    ]

  incompleteConstraintSourceCode2 :: T.Text -> T.Text
  incompleteConstraintSourceCode2 constraint =
    T.unlines
    [ "module Testing where"
    , ""
    , "data Three a b c = Three a b c"
    , ""
    , "eq :: " <> constraint <> " => Three a b c -> Three a b c -> Bool"
    , "eq (Three x y z) (Three x' y' z') = x == x' && y == y' && z == z'"
    ]

  incompleteConstraintSourceCodeWithExtraCharsInContext :: T.Text -> T.Text
  incompleteConstraintSourceCodeWithExtraCharsInContext constraint =
    T.unlines
    [ "module Testing where"
    , ""
    , "data Pair a b = Pair a b"
    , ""
    , "eq :: ( " <> constraint <> " ) => Pair a b -> Pair a b -> Bool"
    , "eq (Pair x y) (Pair x' y') = x == x' && y == y'"
    ]

  incompleteConstraintSourceCodeWithNewlinesInTypeSignature :: T.Text -> T.Text
  incompleteConstraintSourceCodeWithNewlinesInTypeSignature constraint =
    T.unlines
    [ "module Testing where"
    , "data Pair a b = Pair a b"
    , "eq "
    , "    :: (" <> constraint <> ")"
    , "    => Pair a b -> Pair a b -> Bool"
    , "eq (Pair x y) (Pair x' y') = x == x' && y == y'"
    ]

  missingMonadConstraint constraint = T.unlines
    [ "module Testing where"
    , "f :: " <> constraint <> "m ()"
    , "f = do "
    , "  return ()"
    ]

  in testGroup "add function constraint"
  [ checkCodeAction
    "no preexisting constraint"
    "Add `Eq a` to the context of the type signature for `eq`"
    (missingConstraintSourceCode "")
    (missingConstraintSourceCode "Eq a => ")
  , checkCodeAction
    "no preexisting constraint, with forall"
    "Add `Eq a` to the context of the type signature for `eq`"
    (missingConstraintWithForAllSourceCode "")
    (missingConstraintWithForAllSourceCode "Eq a => ")
  , checkCodeAction
    "preexisting constraint, no parenthesis"
    "Add `Eq b` to the context of the type signature for `eq`"
    (incompleteConstraintSourceCode "Eq a")
    (incompleteConstraintSourceCode "(Eq a, Eq b)")
  , checkCodeAction
    "preexisting constraints in parenthesis"
    "Add `Eq c` to the context of the type signature for `eq`"
    (incompleteConstraintSourceCode2 "(Eq a, Eq b)")
    (incompleteConstraintSourceCode2 "(Eq a, Eq b, Eq c)")
  , checkCodeAction
    "preexisting constraints with forall"
    "Add `Eq b` to the context of the type signature for `eq`"
    (incompleteConstraintWithForAllSourceCode "Eq a")
    (incompleteConstraintWithForAllSourceCode "(Eq a, Eq b)")
  , checkCodeAction
    "preexisting constraint, with extra spaces in context"
    "Add `Eq b` to the context of the type signature for `eq`"
    (incompleteConstraintSourceCodeWithExtraCharsInContext "Eq a")
    (incompleteConstraintSourceCodeWithExtraCharsInContext "Eq a, Eq b")
  , checkCodeAction
    "preexisting constraint, with newlines in type signature"
    "Add `Eq b` to the context of the type signature for `eq`"
    (incompleteConstraintSourceCodeWithNewlinesInTypeSignature "Eq a")
    (incompleteConstraintSourceCodeWithNewlinesInTypeSignature "Eq a, Eq b")
  , checkCodeAction
    "missing Monad constraint"
    "Add `Monad m` to the context of the type signature for `f`"
    (missingMonadConstraint "")
    (missingMonadConstraint "Monad m => ")
  ]

checkCodeAction :: String -> T.Text -> T.Text -> T.Text -> TestTree
checkCodeAction testName actionTitle originalCode expectedCode = testSession testName $ do
  doc <- createDoc "Testing.hs" "haskell" originalCode
  _ <- waitForDiagnostics
  actionsOrCommands <- getAllCodeActions doc
  chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
  executeCodeAction chosenAction
  modifiedCode <- documentContents doc
  liftIO $ expectedCode @=? modifiedCode

addImplicitParamsConstraintTests :: TestTree
addImplicitParamsConstraintTests =
  testGroup
    "add missing implicit params constraints"
    [ testGroup
        "introduced"
        [ let ex ctxtA = exampleCode "?a" ctxtA ""
           in checkCodeAction "at top level" "Add ?a::() to the context of fBase" (ex "") (ex "?a::()"),
          let ex ctxA = exampleCode "x where x = ?a" ctxA ""
           in checkCodeAction "in nested def" "Add ?a::() to the context of fBase" (ex "") (ex "?a::()")
        ],
      testGroup
        "inherited"
        [ let ex = exampleCode "()" "?a::()"
           in checkCodeAction
                "with preexisting context"
                "Add `?a::()` to the context of the type signature for `fCaller`"
                (ex "Eq ()")
                (ex "Eq (), ?a::()"),
          let ex = exampleCode "()" "?a::()"
           in checkCodeAction "without preexisting context" "Add ?a::() to the context of fCaller" (ex "") (ex "?a::()")
        ]
    ]
  where
    mkContext ""       = ""
    mkContext contents = "(" <> contents <> ") => "

    exampleCode bodyBase contextBase contextCaller =
      T.unlines
        [ "{-# LANGUAGE FlexibleContexts, ImplicitParams #-}",
          "module Testing where",
          "fBase :: " <> mkContext contextBase <> "()",
          "fBase = " <> bodyBase,
          "fCaller :: " <> mkContext contextCaller <> "()",
          "fCaller = fBase"
        ]

removeRedundantConstraintsTests :: TestTree
removeRedundantConstraintsTests = let
  header =
    [ "{-# OPTIONS_GHC -Wredundant-constraints #-}"
    , "module Testing where"
    , ""
    ]

  headerExt :: [T.Text] -> [T.Text]
  headerExt exts =
    redunt : extTxt ++ ["module Testing where"]
    where
      redunt = "{-# OPTIONS_GHC -Wredundant-constraints #-}"
      extTxt = map (\ext -> "{-# LANGUAGE " <> ext <> " #-}") exts

  redundantConstraintsCode :: Maybe T.Text -> T.Text
  redundantConstraintsCode mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ header <>
        [ "foo :: " <> constraint <> "a -> a"
        , "foo = id"
        ]

  redundantMixedConstraintsCode :: Maybe T.Text -> T.Text
  redundantMixedConstraintsCode mConstraint =
    let constraint = maybe "(Num a, Eq a)" (\c -> "(Num a, Eq a, " <> c <> ")") mConstraint
      in T.unlines $ header <>
        [ "foo :: " <> constraint <> " => a -> Bool"
        , "foo x = x == 1"
        ]

  typeSignatureSpaces :: Maybe T.Text -> T.Text
  typeSignatureSpaces mConstraint =
    let constraint = maybe "(Num a, Eq a)" (\c -> "(Num a, Eq a, " <> c <> ")") mConstraint
      in T.unlines $ header <>
        [ "foo ::  " <> constraint <> " => a -> Bool"
        , "foo x = x == 1"
        ]

  redundantConstraintsForall :: Maybe T.Text -> T.Text
  redundantConstraintsForall mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ headerExt ["RankNTypes"] <>
        [ "foo :: forall a. " <> constraint <> "a -> a"
        , "foo = id"
        ]

  typeSignatureDo :: Maybe T.Text -> T.Text
  typeSignatureDo mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ header <>
        [ "f :: Int -> IO ()"
        , "f n = do"
        , "  let foo :: " <> constraint <> "a -> IO ()"
        , "      foo _ = return ()"
        , "  r n"
        ]

  typeSignatureNested :: Maybe T.Text -> T.Text
  typeSignatureNested mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ header <>
        [ "f :: Int -> ()"
        , "f = g"
        , "  where"
        , "    g :: " <> constraint <> "a -> ()"
        , "    g _ = ()"
        ]

  typeSignatureNested' :: Maybe T.Text -> T.Text
  typeSignatureNested' mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ header <>
        [ "f :: Int -> ()"
        , "f ="
        , "  let"
        , "    g :: Int -> ()"
        , "    g = h"
        , "      where"
        , "        h :: " <> constraint <> "a -> ()"
        , "        h _ = ()"
        , "  in g"
        ]

  typeSignatureNested'' :: Maybe T.Text -> T.Text
  typeSignatureNested'' mConstraint =
    let constraint = maybe "" (\c -> "" <> c <> " => ") mConstraint
      in T.unlines $ header <>
        [ "f :: Int -> ()"
        , "f = g"
        , "  where"
        , "    g :: Int -> ()"
        , "    g = "
        , "      let"
        , "        h :: " <> constraint <> "a -> ()"
        , "        h _ = ()"
        , "      in h"
        ]

  typeSignatureLined1 = T.unlines $ header <>
    [ "foo :: Eq a =>"
    , "  a -> Bool"
    , "foo _ = True"
    ]

  typeSignatureLined2 = T.unlines $ header <>
    [ "foo :: (Eq a, Show a)"
    , "  => a -> Bool"
    , "foo _ = True"
    ]

  typeSignatureOneLine = T.unlines $ header <>
    [ "foo :: a -> Bool"
    , "foo _ = True"
    ]

  typeSignatureLined3 = T.unlines $ header <>
    [ "foo :: ( Eq a"
    , "       , Show a"
    , "       )"
    , "    => a -> Bool"
    , "foo x = x == x"
    ]

  typeSignatureLined3' = T.unlines $ header <>
    [ "foo :: ( Eq a"
    , "       )"
    , "    => a -> Bool"
    , "foo x = x == x"
    ]


  check :: T.Text -> T.Text -> T.Text -> TestTree
  check actionTitle originalCode expectedCode = testSession (T.unpack actionTitle) $ do
    doc <- createDoc "Testing.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getAllCodeActions doc
    chosenAction <- liftIO $ pickActionWithTitle actionTitle actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode

  in testGroup "remove redundant function constraints"
  [ check
    "Remove redundant constraint `Eq a` from the context of the type signature for `foo`"
    (redundantConstraintsCode $ Just "Eq a")
    (redundantConstraintsCode Nothing)
  , check
    "Remove redundant constraints `(Eq a, Monoid a)` from the context of the type signature for `foo`"
    (redundantConstraintsCode $ Just "(Eq a, Monoid a)")
    (redundantConstraintsCode Nothing)
  , check
    "Remove redundant constraints `(Monoid a, Show a)` from the context of the type signature for `foo`"
    (redundantMixedConstraintsCode $ Just "Monoid a, Show a")
    (redundantMixedConstraintsCode Nothing)
  , check
    "Remove redundant constraint `Eq a` from the context of the type signature for `g`"
    (typeSignatureNested $ Just "Eq a")
    (typeSignatureNested Nothing)
  , check
    "Remove redundant constraint `Eq a` from the context of the type signature for `h`"
    (typeSignatureNested' $ Just "Eq a")
    (typeSignatureNested' Nothing)
  , check
    "Remove redundant constraint `Eq a` from the context of the type signature for `h`"
    (typeSignatureNested'' $ Just "Eq a")
    (typeSignatureNested'' Nothing)
  , check
    "Remove redundant constraint `Eq a` from the context of the type signature for `foo`"
    (redundantConstraintsForall $ Just "Eq a")
    (redundantConstraintsForall Nothing)
  , check
    "Remove redundant constraint `Eq a` from the context of the type signature for `foo`"
    (typeSignatureDo $ Just "Eq a")
    (typeSignatureDo Nothing)
  , check
    "Remove redundant constraints `(Monoid a, Show a)` from the context of the type signature for `foo`"
    (typeSignatureSpaces $ Just "Monoid a, Show a")
    (typeSignatureSpaces Nothing)
  , check
    "Remove redundant constraint `Eq a` from the context of the type signature for `foo`"
    typeSignatureLined1
    typeSignatureOneLine
  , check
    "Remove redundant constraints `(Eq a, Show a)` from the context of the type signature for `foo`"
    typeSignatureLined2
    typeSignatureOneLine
  , check
    "Remove redundant constraint `Show a` from the context of the type signature for `foo`"
    typeSignatureLined3
    typeSignatureLined3'
  ]

addSigActionTests :: TestTree
addSigActionTests = let
  header = [ "{-# OPTIONS_GHC -Wmissing-signatures -Wmissing-pattern-synonym-signatures #-}"
           , "{-# LANGUAGE PatternSynonyms,BangPatterns,GADTs #-}"
           , "module Sigs where"
           , "data T1 a where"
           , "  MkT1 :: (Show b) => a -> b -> T1 a"
           ]
  before def     = T.unlines $ header ++ [def]
  after' def sig = T.unlines $ header ++ [sig, def]

  def >:: sig = testSession (T.unpack $ T.replace "\n" "\\n" def) $ do
    let originalCode = before def
    let expectedCode = after' def sig
    doc <- createDoc "Sigs.hs" "haskell" originalCode
    _ <- waitForDiagnostics
    actionsOrCommands <- getCodeActions doc (Range (Position 5 1) (Position 5 maxBound))
    chosenAction <- liftIO $ pickActionWithTitle ("add signature: " <> sig) actionsOrCommands
    executeCodeAction chosenAction
    modifiedCode <- documentContents doc
    liftIO $ expectedCode @=? modifiedCode
  in
  testGroup "add signature"
    [ "abc = True"              >:: "abc :: Bool"
    , "foo a b = a + b"         >:: "foo :: Num a => a -> a -> a"
    , "bar a b = show $ a + b"  >:: "bar :: (Show a, Num a) => a -> a -> String"
    , "(!!!) a b = a > b"       >:: "(!!!) :: Ord a => a -> a -> Bool"
    , "a >>>> b = a + b"        >:: "(>>>>) :: Num a => a -> a -> a"
    , "a `haha` b = a b"        >:: "haha :: (t1 -> t2) -> t1 -> t2"
    , "pattern Some a = Just a" >:: "pattern Some :: a -> Maybe a"
    , "pattern Some a <- Just a" >:: "pattern Some :: a -> Maybe a"
    , "pattern Some a <- Just a\n  where Some a = Just a" >:: "pattern Some :: a -> Maybe a"
    , "pattern Some a <- Just !a\n  where Some !a = Just a" >:: "pattern Some :: a -> Maybe a"
    , "pattern Point{x, y} = (x, y)" >:: "pattern Point :: a -> b -> (a, b)"
    , "pattern Point{x, y} <- (x, y)" >:: "pattern Point :: a -> b -> (a, b)"
    , "pattern Point{x, y} <- (x, y)\n  where Point x y = (x, y)" >:: "pattern Point :: a -> b -> (a, b)"
    , "pattern MkT1' b = MkT1 42 b" >:: "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a"
    , "pattern MkT1' b <- MkT1 42 b" >:: "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a"
    , "pattern MkT1' b <- MkT1 42 b\n  where MkT1' b = MkT1 42 b" >:: "pattern MkT1' :: (Eq a, Num a) => Show b => b -> T1 a"
    ]

exportUnusedTests :: TestTree
exportUnusedTests = testGroup "export unused actions"
  [ testGroup "don't want suggestion"
    [ testSession "implicit exports" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# OPTIONS_GHC -Wmissing-signatures #-}"
              , "module A where"
              , "foo = id"])
        (R 3 0 3 3)
        "Export ‘foo’"
        Nothing -- codeaction should not be available
    , testSession "not top-level" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# OPTIONS_GHC -Wunused-binds #-}"
              , "module A (foo,bar) where"
              , "foo = ()"
              , "  where bar = ()"
              , "bar = ()"])
        (R 2 0 2 11)
        "Export ‘bar’"
        Nothing
    , ignoreForGhcVersions [GHC92, GHC94] "Diagnostic message has no suggestions" $
      testSession "type is exported but not the constructor of same name" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo) where"
              , "data Foo = Foo"])
        (R 2 0 2 8)
        "Export ‘Foo’"
        Nothing -- codeaction should not be available
    , testSession "unused data field" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo(Foo)) where"
              , "data Foo = Foo {foo :: ()}"])
        (R 2 0 2 20)
        "Export ‘foo’"
        Nothing -- codeaction should not be available
    ]
  , testGroup "want suggestion"
    [ testSession "empty exports" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A ("
              , ") where"
              , "foo = id"])
        (R 3 0 3 3)
        "Export ‘foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A ("
              , "foo) where"
              , "foo = id"])
    , testSession "single line explicit exports" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (foo) where"
              , "foo = id"
              , "bar = foo"])
        (R 3 0 3 3)
        "Export ‘bar’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (foo, bar) where"
              , "foo = id"
              , "bar = foo"])
    , testSession "multi line explicit exports" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  ("
              , "    foo) where"
              , "foo = id"
              , "bar = foo"])
        (R 5 0 5 3)
        "Export ‘bar’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  ("
              , "    foo, bar) where"
              , "foo = id"
              , "bar = foo"])
    , testSession "export list ends in comma" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  (foo,"
              , "  ) where"
              , "foo = id"
              , "bar = foo"])
        (R 5 0 5 3)
        "Export ‘bar’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  (foo,"
              , "  bar) where"
              , "foo = id"
              , "bar = foo"])
    , testSession "style of multiple exports is preserved 1" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  ( foo"
              , "  , bar"
              , "  ) where"
              , "foo = id"
              , "bar = foo"
              , "baz = bar"
              ])
        (R 7 0 7 3)
        "Export ‘baz’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  ( foo"
              , "  , bar"
              , "  , baz"
              , "  ) where"
              , "foo = id"
              , "bar = foo"
              , "baz = bar"
              ])
    , testSession "style of multiple exports is preserved 2" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  ( foo,"
              , "    bar"
              , "  ) where"
              , "foo = id"
              , "bar = foo"
              , "baz = bar"
              ])
        (R 7 0 7 3)
        "Export ‘baz’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  ( foo,"
              , "    bar,"
              , "    baz"
              , "  ) where"
              , "foo = id"
              , "bar = foo"
              , "baz = bar"
              ])
    , testSession "style of multiple exports is preserved and selects smallest export separator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  ( foo"
              , "  , bar"
              , "  -- * For testing"
              , "  , baz"
              , "  ) where"
              , "foo = id"
              , "bar = foo"
              , "baz = bar"
              , "quux = bar"
              ])
        (R 10 0 10 4)
        "Export ‘quux’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A"
              , "  ( foo"
              , "  , bar"
              , "  -- * For testing"
              , "  , baz"
              , "  , quux"
              , "  ) where"
              , "foo = id"
              , "bar = foo"
              , "baz = bar"
              , "quux = bar"
              ])
    , testSession "unused pattern synonym" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE PatternSynonyms #-}"
              , "module A () where"
              , "pattern Foo a <- (a, _)"])
        (R 3 0 3 10)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE PatternSynonyms #-}"
              , "module A (pattern Foo) where"
              , "pattern Foo a <- (a, _)"])
    , testSession "unused data type" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "data Foo = Foo"])
        (R 2 0 2 7)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo(..)) where"
              , "data Foo = Foo"])
    , testSession "unused newtype" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "newtype Foo = Foo ()"])
        (R 2 0 2 10)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo(..)) where"
              , "newtype Foo = Foo ()"])
    , testSession "unused type synonym" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "type Foo = ()"])
        (R 2 0 2 7)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo) where"
              , "type Foo = ()"])
    , testSession "unused type family" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "module A () where"
              , "type family Foo p"])
        (R 3 0 3 15)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "module A (Foo) where"
              , "type family Foo p"])
    , testSession "unused typeclass" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "class Foo a"])
        (R 2 0 2 8)
        "Export ‘Foo’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (Foo(..)) where"
              , "class Foo a"])
    , testSession "infix" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "a `f` b = ()"])
        (R 2 0 2 11)
        "Export ‘f’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A (f) where"
              , "a `f` b = ()"])
    , testSession "function operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A () where"
              , "(<|) = ($)"])
        (R 2 0 2 9)
        "Export ‘<|’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "module A ((<|)) where"
              , "(<|) = ($)"])
    , testSession "type synonym operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "type (:<) = ()"])
        (R 3 0 3 13)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A ((:<)) where"
              , "type (:<) = ()"])
    , testSession "type family operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "type family (:<)"])
        (R 4 0 4 15)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeFamilies #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A (type (:<)) where"
              , "type family (:<)"])
    , testSession "typeclass operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "class (:<) a"])
        (R 3 0 3 11)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A (type (:<)(..)) where"
              , "class (:<) a"])
    , testSession "newtype operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "newtype (:<) = Foo ()"])
        (R 3 0 3 20)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A (type (:<)(..)) where"
              , "newtype (:<) = Foo ()"])
    , testSession "data type operator" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A () where"
              , "data (:<) = Foo ()"])
        (R 3 0 3 17)
        "Export ‘:<’"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wunused-top-binds #-}"
              , "{-# LANGUAGE TypeOperators #-}"
              , "module A (type (:<)(..)) where"
              , "data (:<) = Foo ()"])
    ]
  ]
  where
    template doc range = exportTemplate (Just range) doc

exportTemplate :: Maybe Range -> T.Text -> T.Text -> Maybe T.Text -> Session ()
exportTemplate mRange initialContent expectedAction expectedContents = do
  doc <- createDoc "A.hs" "haskell" initialContent
  _ <- waitForDiagnostics
  actions <- case mRange of
    Nothing    -> getAllCodeActions doc
    Just range -> getCodeActions doc range
  case expectedContents of
    Just content -> do
      action <- liftIO $ pickActionWithTitle expectedAction actions
      executeCodeAction action
      contentAfterAction <- documentContents doc
      liftIO $ content @=? contentAfterAction
    Nothing ->
      liftIO $ [_title | InR CodeAction{_title} <- actions, _title == expectedAction ] @?= []

removeExportTests :: TestTree
removeExportTests = testGroup "remove export actions"
    [ testSession "single export" $ template
        (T.unlines
              [ "module A (  a   ) where"
              , "b :: ()"
              , "b = ()"])
        "Remove ‘a’ from export"
        (Just $ T.unlines
              [ "module A (     ) where"
              , "b :: ()"
              , "b = ()"])
    , testSession "ending comma" $ template
        (T.unlines
              [ "module A (  a,   ) where"
              , "b :: ()"
              , "b = ()"])
        "Remove ‘a’ from export"
        (Just $ T.unlines
              [ "module A (  ) where"
              , "b :: ()"
              , "b = ()"])
    , testSession "multiple exports" $ template
        (T.unlines
              [ "module A (a  ,   c,    b ) where"
              , "a, c :: ()"
              , "a = ()"
              , "c = ()"])
        "Remove ‘b’ from export"
        (Just $ T.unlines
              [ "module A (a  ,   c ) where"
              , "a, c :: ()"
              , "a = ()"
              , "c = ()"])
    , testSession "not in scope constructor" $ template
        (T.unlines
              [ "module A (A (X,Y,Z,(:<)), ab) where"
              , "data A = X Int | Y | (:<) Int"
              , "ab :: ()"
              , "ab = ()"
              ])
        "Remove ‘Z’ from export"
        (Just $ T.unlines
              [ "module A (A (X,Y,(:<)), ab) where"
              , "data A = X Int | Y | (:<) Int"
              , "ab :: ()"
              , "ab = ()"])
    , testSession "multiline export" $ template
        (T.unlines
              [ "module A (a"
              , " ,  b"
              , " , (:*:)"
              , " , ) where"
              , "a,b :: ()"
              , "a = ()"
              , "b = ()"])
        "Remove ‘:*:’ from export"
        (Just $ T.unlines
              [ "module A (a"
              , " ,  b"
              , " "
              , " , ) where"
              , "a,b :: ()"
              , "a = ()"
              , "b = ()"])
    , testSession "qualified re-export" $ template
        (T.unlines
              [ "module A (M.x,a) where"
              , "import qualified Data.List as M"
              , "a :: ()"
              , "a = ()"])
        "Remove ‘M.x’ from export"
        (Just $ T.unlines
              [ "module A (a) where"
              , "import qualified Data.List as M"
              , "a :: ()"
              , "a = ()"])
    , testSession "qualified re-export ending in '.'" $ template
        (T.unlines
              [ "module A ((M.@.),a) where"
              , "import qualified Data.List as M"
              , "a :: ()"
              , "a = ()"])
        "Remove ‘M.@.’ from export"
        (Just $ T.unlines
              [ "module A (a) where"
              , "import qualified Data.List as M"
              , "a :: ()"
              , "a = ()"])
    , testSession "export module" $ template
        (T.unlines
              [ "module A (module B) where"
              , "a :: ()"
              , "a = ()"])
        "Remove ‘module B’ from export"
        (Just $ T.unlines
              [ "module A () where"
              , "a :: ()"
              , "a = ()"])
    , testSession "dodgy export" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wall #-}"
              , "module A (A (..)) where"
              , "data X = X"
              , "type A = X"])
        "Remove ‘A(..)’ from export"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wall #-}"
              , "module A () where"
              , "data X = X"
              , "type A = X"])
    , testSession "dodgy export" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wall #-}"
              , "module A (A (..)) where"
              , "data X = X"
              , "type A = X"])
        "Remove ‘A(..)’ from export"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wall #-}"
              , "module A () where"
              , "data X = X"
              , "type A = X"])
    , testSession "duplicate module export" $ template
        (T.unlines
              [ "{-# OPTIONS_GHC -Wall #-}"
              , "module A (module L,module L) where"
              , "import Data.List as L"
              , "a :: ()"
              , "a = ()"])
        "Remove ‘Module L’ from export"
        (Just $ T.unlines
              [ "{-# OPTIONS_GHC -Wall #-}"
              , "module A (module L) where"
              , "import Data.List as L"
              , "a :: ()"
              , "a = ()"])
    , testSession "remove all exports single" $ template
        (T.unlines
              [ "module A (x) where"
              , "a :: ()"
              , "a = ()"])
        "Remove all redundant exports"
        (Just $ T.unlines
              [ "module A () where"
              , "a :: ()"
              , "a = ()"])
    , testSession "remove all exports two" $ template
        (T.unlines
              [ "module A (x,y) where"
              , "a :: ()"
              , "a = ()"])
        "Remove all redundant exports"
        (Just $ T.unlines
              [ "module A () where"
              , "a :: ()"
              , "a = ()"])
    , testSession "remove all exports three" $ template
        (T.unlines
              [ "module A (a,x,y) where"
              , "a :: ()"
              , "a = ()"])
        "Remove all redundant exports"
        (Just $ T.unlines
              [ "module A (a) where"
              , "a :: ()"
              , "a = ()"])
    , testSession "remove all exports composite" $ template
        (T.unlines
              [ "module A (x,y,b, module Ls, a, A(X,getW, Y, Z,(:-),getV), (-+), B(B)) where"
              , "data A = X {getV :: Int} | Y {getV :: Int}"
              , "data B = B"
              , "a,b :: ()"
              , "a = ()"
              , "b = ()"])
        "Remove all redundant exports"
        (Just $ T.unlines
              [ "module A (b, a, A(X, Y,getV), B(B)) where"
              , "data A = X {getV :: Int} | Y {getV :: Int}"
              , "data B = B"
              , "a,b :: ()"
              , "a = ()"
              , "b = ()"])
    ]
  where
    template = exportTemplate Nothing


codeActionHelperFunctionTests :: TestTree
codeActionHelperFunctionTests = testGroup "code action helpers"
    [
    extendImportTestsRegEx
    ]

extendImportTestsRegEx :: TestTree
extendImportTestsRegEx = testGroup "regex parsing"
    [
      testCase "parse invalid multiple imports" $ template "foo bar foo" Nothing
    , testCase "parse malformed import list" $ template
                  "\n\8226 Perhaps you want to add \8216fromList\8217 to one of these import lists:\n    \8216Data.Map\8217)"
                  Nothing
    , testCase "parse multiple imports" $ template
                 "\n\8226 Perhaps you want to add \8216fromList\8217 to one of these import lists:\n    \8216Data.Map\8217 (app/testlsp.hs:7:1-18)\n    \8216Data.HashMap.Strict\8217 (app/testlsp.hs:8:1-29)"
                 $ Just ("fromList",[("Data.Map","app/testlsp.hs:7:1-18"),("Data.HashMap.Strict","app/testlsp.hs:8:1-29")])
    ]
    where
        template message expected = do
            liftIO $ matchRegExMultipleImports message @=? expected

pickActionWithTitle :: T.Text -> [Command |? CodeAction] -> IO CodeAction
pickActionWithTitle title actions = do
  assertBool ("Found no matching actions for " <> show title <> " in " <> show titles) (not $ null matches)
  return $ head matches
  where
    titles =
        [ actionTitle
        | InR CodeAction { _title = actionTitle } <- actions
        ]
    matches =
        [ action
        | InR action@CodeAction { _title = actionTitle } <- actions
        , title == actionTitle
        ]

assertNoActionWithTitle :: T.Text -> [Command |? CodeAction] -> IO ()
assertNoActionWithTitle title actions = do
  assertBool ("Unexpected code action " <> show title <> " in " <> show titles) (null matches)
  pure ()
  where
    titles =
        [ actionTitle
        | InR CodeAction { _title = actionTitle } <- actions
        ]
    matches =
        [ action
        | InR action@CodeAction { _title = actionTitle } <- actions
        , title == actionTitle
        ]

findCodeActions :: TextDocumentIdentifier -> Range -> [T.Text] -> Session [CodeAction]
findCodeActions = findCodeActions' (==) "is not a superset of"

findCodeActionsByPrefix :: TextDocumentIdentifier -> Range -> [T.Text] -> Session [CodeAction]
findCodeActionsByPrefix = findCodeActions' T.isPrefixOf "is not prefix of"

findCodeActions' :: (T.Text -> T.Text -> Bool) -> String -> TextDocumentIdentifier -> Range -> [T.Text] -> Session [CodeAction]
findCodeActions' op errMsg doc range expectedTitles = do
  actions <- getCodeActions doc range
  let matches = sequence
        [ listToMaybe
          [ action
          | InR action@CodeAction { _title = actionTitle } <- actions
          , expectedTitle `op` actionTitle]
        | expectedTitle <- expectedTitles]
  let msg = show
            [ actionTitle
            | InR CodeAction { _title = actionTitle } <- actions
            ]
            ++ " " <> errMsg <> " "
            ++ show expectedTitles
  liftIO $ case matches of
    Nothing -> assertFailure msg
    Just _  -> pure ()
  return (fromJust matches)

findCodeAction :: TextDocumentIdentifier -> Range -> T.Text -> Session CodeAction
findCodeAction doc range t = head <$> findCodeActions doc range [t]

testSession :: String -> Session () -> TestTree
testSession name = testCase name . run

testSessionWithExtraFiles :: HasCallStack => FilePath -> String -> (FilePath -> Session ()) -> TestTree
testSessionWithExtraFiles prefix name = testCase name . runWithExtraFiles prefix

runWithExtraFiles :: HasCallStack => FilePath -> (FilePath -> Session a) -> IO a
runWithExtraFiles prefix s = withTempDir $ \dir -> do
  copyTestDataFiles dir prefix
  runInDir dir (s dir)

copyTestDataFiles :: HasCallStack => FilePath -> FilePath -> IO ()
copyTestDataFiles dir prefix = do
  -- Copy all the test data files to the temporary workspace
  testDataFiles <- getDirectoryFilesIO ("test/data" </> prefix) ["//*"]
  for_ testDataFiles $ \f -> do
    createDirectoryIfMissing True $ dir </> takeDirectory f
    copyFile ("test/data" </> prefix </> f) (dir </> f)

run :: Session a -> IO a
run s = run' (const s)

run' :: (FilePath -> Session a) -> IO a
run' s = withTempDir $ \dir -> runInDir dir (s dir)

runInDir :: FilePath -> Session a -> IO a
runInDir dir act = do
  plugin <- refactorPlugin
  runSessionWithServer' plugin def def lspTestCaps dir act

lspTestCaps :: ClientCapabilities
lspTestCaps = fullCaps { _window = Just $ WindowClientCapabilities (Just True) Nothing Nothing }

pattern R :: UInt -> UInt -> UInt -> UInt -> Range
pattern R x y x' y' = Range (Position x y) (Position x' y')

-- | Version of 'System.IO.Extra.withTempDir' that canonicalizes the path
-- Which we need to do on macOS since the $TMPDIR can be in @/private/var@ or
-- @/var@
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir f = System.IO.Extra.withTempDir $ \dir -> do
  dir' <- canonicalizePath dir
  f dir'

ignoreForGHC92 :: String -> TestTree -> TestTree
ignoreForGHC92 = ignoreForGhcVersions [GHC92]

brokenForGHC94 :: String -> TestTree -> TestTree
brokenForGHC94 = knownBrokenForGhcVersions [GHC94]

-- | Assert that a value is not 'Nothing', and extract the value.
assertJust :: MonadIO m => String -> Maybe a -> m a
assertJust s = \case
  Nothing -> liftIO $ assertFailure s
  Just x  -> pure x

-- | Before ghc9, lists of Char is displayed as [Char], but with ghc9 and up, it's displayed as String
listOfChar :: T.Text
listOfChar | ghcVersion >= GHC90 = "String"
           | otherwise = "[Char]"
