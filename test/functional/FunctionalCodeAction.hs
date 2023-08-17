{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module FunctionalCodeAction (tests) where

import           Control.Lens                 hiding (List)
import           Control.Monad
import           Data.Aeson.Lens              (_Object)
import           Data.List
import qualified Data.Map                     as M
import           Data.Maybe
import qualified Data.Text                    as T
import           Development.IDE.Core.Compile (sourceTypecheck)
import           Development.IDE.Test         (configureCheckProject)
import           Ide.Plugin.Config
import qualified Language.LSP.Protocol.Lens   as L
import           Language.LSP.Test            as Test
import           Test.Hls
import           Test.Hls.Command
import           Test.Hspec.Expectations

{-# ANN module ("HLint: ignore Reduce duplication"::String) #-}

tests :: TestTree
tests = testGroup "code actions" [
#if hls_refactor
      importTests
    , ignoreInEnv [HostOS Windows, GhcVer GHC94] "Diagnostic failure for Windows-ghc9.4.2" importQualifiedTests
    , ignoreInEnv [HostOS Windows, GhcVer GHC94] "Diagnostic failure for Windows-ghc9.4.2" importQualifiedPostTests
    , packageTests
    , redundantImportTests
    , renameTests
    , signatureTests
    , typedHoleTests
    , unusedTermTests
#endif
    ]

renameTests :: TestTree
renameTests = testGroup "rename suggestions" [
    testCase "works" $ runSession hlsCommand noLiteralCaps "test/testdata" $ do
        doc <- openDoc "CodeActionRename.hs" "haskell"

        _ <- waitForDiagnosticsFromSource doc (T.unpack sourceTypecheck)

        cars <- getAllCodeActions doc
        replaceButStrLn <- liftIO $ inspectCommand cars ["Replace with", "putStrLn"]
        executeCommand replaceButStrLn
        _ <- anyRequest

        x:_ <- T.lines <$> documentContents doc
        liftIO $ x @?= "main = putStrLn \"hello\""

    , testCase "doesn't give both documentChanges and changes"
        $ runSession hlsCommand noLiteralCaps "test/testdata" $ do
            configureCheckProject False
            doc <- openDoc "CodeActionRename.hs" "haskell"

            _ <- waitForDiagnosticsFromSource doc (T.unpack sourceTypecheck)

            cars <- getAllCodeActions doc
            cmd <- liftIO $ inspectCommand cars ["Replace with", "putStrLn"]
            let mbArgs = cmd ^. L.arguments
            case mbArgs of
                Just [args] -> liftIO $ do
                    let editParams = args ^. ix "fallbackWorkspaceEdit" . _Object
                    (editParams & has (ix "changes"))  @? "Contains changes"
                    not (editParams & has (ix "documentChanges")) @? "Doesn't contain documentChanges"
                _ -> error $ "Unexpected arguments: " ++ show mbArgs

            executeCommand cmd
            _ <- anyRequest

            x1:x2:_ <- T.lines <$> documentContents doc
            liftIO $
                x1 == "main = putStrLn \"hello\""
                || x2 == "foo = putStrLn \"world\""
                @? "One of the typos got fixed"
    ]

importTests :: TestTree
importTests = testGroup "import suggestions" [
    testCase "import works with 3.8 code action kinds" $ runSessionWithConfig (def {lspConfig = hlsConfigToClientConfig testConfig}) hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "CodeActionImport.hs" "haskell"

        (diag:_) <- waitForDiagnosticsFrom doc
        liftIO $ diag ^. L.message @?= "Variable not in scope: when :: Bool -> IO () -> IO ()"

        actionsOrCommands <- getAllCodeActions doc
        let actns = map fromAction actionsOrCommands

        importControlMonad <- liftIO $ inspectCodeAction actionsOrCommands ["import Control.Monad"]
        liftIO $ do
            expectCodeAction actionsOrCommands ["import Control.Monad (when)"]
            length actns >= 10 @? "There are some actions"

        executeCodeAction importControlMonad

        contents <- documentContents doc
        liftIO $ contents @?= "import Control.Monad\nmain :: IO ()\nmain = when True $ putStrLn \"hello\""
    ]

importQualifiedTests :: TestTree
importQualifiedTests = testGroup "import qualified prefix suggestions" [
    testCase "qualified import works with 3.8 code action kinds" $ runSessionWithConfig (def {lspConfig = hlsConfigToClientConfig testConfig}) hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "CodeActionImportQualified.hs" "haskell"
        (diag:_) <- waitForDiagnosticsFrom doc
        liftIO $ diag ^. L.message @?=
           if ghcVersion >= GHC96
           then "Variable not in scope: Control.when :: Bool -> IO () -> IO ()\nNB: no module named ‘Control’ is imported."
           else "Not in scope: ‘Control.when’\nNo module named ‘Control’ is imported."

        actionsOrCommands <- getAllCodeActions doc
        let actns = map fromAction actionsOrCommands

        let importQualifiedSuggestion = "import qualified Control.Monad as Control"
        importControlMonadQualified <- liftIO $ inspectCodeAction actionsOrCommands [importQualifiedSuggestion]
        liftIO $ do
            dontExpectCodeAction actionsOrCommands ["import Control.Monad (when)"]
            length actns >= 5 @? "There are some actions"

        executeCodeAction importControlMonadQualified

        contents <- documentContents doc
        liftIO $ contents @?= "import qualified Control.Monad as Control\nmain :: IO ()\nmain = Control.when True $ putStrLn \"hello\"\n"
    ]

importQualifiedPostTests :: TestTree
importQualifiedPostTests = testGroup "import qualified postfix suggestions" [
    testCase "qualified import in postfix position works with 3.8 code action kinds" $ runSessionWithConfig (def {lspConfig = hlsConfigToClientConfig testConfig}) hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "CodeActionImportPostQualified.hs" "haskell"
        (diag:_) <- waitForDiagnosticsFrom doc
        liftIO $ diag ^. L.message @?=
           if ghcVersion >= GHC96
           then "Variable not in scope: Control.when :: Bool -> IO () -> IO ()\nNB: no module named ‘Control’ is imported."
           else "Not in scope: ‘Control.when’\nNo module named ‘Control’ is imported."

        actionsOrCommands <- getAllCodeActions doc
        let actns = map fromAction actionsOrCommands

        let importQualifiedPostSuggestion = "import Control.Monad qualified as Control"
        importControlMonadQualified <- liftIO $ inspectCodeAction actionsOrCommands [importQualifiedPostSuggestion]
        liftIO $ do
            dontExpectCodeAction actionsOrCommands ["import qualified Control.Monad as Control", "import Control.Monad (when)"]
            length actns >= 5 @? "There are some actions"

        executeCodeAction importControlMonadQualified

        contents <- documentContents doc
        liftIO $ T.lines contents !! 2 @?= "import Control.Monad qualified as Control"
    ]

packageTests :: TestTree
packageTests = testGroup "add package suggestions" [
    ignoreTestBecause "no support for adding dependent packages via code action" $ testCase "adds to .cabal files" $ do
        runSession hlsCommand fullCaps "test/testdata/addPackageTest/cabal-exe" $ do
            doc <- openDoc "AddPackage.hs" "haskell"

            -- ignore the first empty hlint diagnostic publish
            [_,diag:_] <- count 2 $ waitForDiagnosticsFrom doc

            let prefixes = [ "Could not load module `Data.Text'" -- Windows && GHC >= 8.6
                        , "Could not find module `Data.Text'" -- Windows
                        , "Could not load module ‘Data.Text’" -- GHC >= 8.6
                        , "Could not find module ‘Data.Text’"
                        ]
                in liftIO $ any (`T.isPrefixOf` (diag ^. L.message)) prefixes @? "Contains prefix"

            acts <- getAllCodeActions doc
            case acts of
                (InR action:_) -> do
                    liftIO $ do
                        action ^. L.title @?= "Add text as a dependency"
                        action ^. L.kind @?= Just CodeActionKind_QuickFix
                        "package:add" `T.isSuffixOf` (action ^. L.command . _Just . L.command) @? "Command contains package:add"

                    executeCodeAction action

                _ -> error $ "Unexpected code actions: " ++ show acts

            contents <- skipManyTill anyMessage $ getDocumentEdit . TextDocumentIdentifier =<< getDocUri "add-package-test.cabal"
            liftIO $
                any (\l -> "text -any" `T.isSuffixOf` l || "text : {} -any" `T.isSuffixOf` l) (T.lines contents) @? "Contains text package"

    , ignoreTestBecause "no support for adding dependent packages via code action" $ testCase "adds to hpack package.yaml files" $
        runSession hlsCommand fullCaps "test/testdata/addPackageTest/hpack-exe" $ do
            doc <- openDoc "app/Asdf.hs" "haskell"

            -- ignore the first empty hlint diagnostic publish
            [_,_:diag:_] <- count 2 $ waitForDiagnosticsFrom doc

            let prefixes =
                        [ "Could not load module `Codec.Compression.GZip'" -- Windows && GHC >= 8.6
                        , "Could not find module `Codec.Compression.GZip'" -- Windows
                        , "Could not load module ‘Codec.Compression.GZip’" -- GHC >= 8.6
                        , "Could not find module ‘Codec.Compression.GZip’"
                        ]
                in liftIO $ any (`T.isPrefixOf` (diag ^. L.message)) prefixes @? "Diagnostic contains message"

            mActions <- getAllCodeActions doc
            let allActions = map fromAction mActions
            action <- case allActions of
                (a:_) -> pure a
                _ -> liftIO $ assertFailure "Expected non-empty list of actions"

            liftIO $ do
                action ^. L.title @?= "Add zlib as a dependency"
                forM_ allActions $ \a -> a ^. L.kind @?= Just CodeActionKind_QuickFix
                forM_ allActions $ \a -> "package:add" `T.isSuffixOf` (a ^. L.command . _Just . L.command) @? "Command contains package:add"

            executeCodeAction action

            contents <- skipManyTill anyMessage $ getDocumentEdit . TextDocumentIdentifier =<< getDocUri "package.yaml"
            liftIO $ do
                "zlib" `T.isSuffixOf` (T.lines contents !! 3) @? "Contains zlib"
                "zlib" `T.isSuffixOf` (T.lines contents !! 21) @? "Does not contain zlib in unrelated component"
    ]

redundantImportTests :: TestTree
redundantImportTests = testGroup "redundant import code actions" [
    testCase "remove solitary redundant imports" $
        runSession hlsCommand fullCaps "test/testdata/redundantImportTest/" $ do
            doc <- openDoc "src/CodeActionRedundant.hs" "haskell"

            diags <- waitForDiagnosticsFromSource doc (T.unpack sourceTypecheck)
            liftIO $ expectDiagnostic diags [ "The import of", "Data.List", "is redundant" ]
            liftIO $ expectDiagnostic diags [ "Empty", "from module", "Data.Sequence" ]

            mActions <- getAllCodeActions doc

            let allActions = map fromAction mActions
                actionTitles = map (view L.title) allActions

            liftIO $ actionTitles `shouldContain`
              [ "Remove import"
              , "Remove Empty from import"
              , "Remove all redundant imports"
              ]

            let mbRemoveAction = find (\x -> x ^. L.title == "Remove all redundant imports") allActions

            case mbRemoveAction of
                Just removeAction -> do
                    liftIO $ do
                        forM_ allActions $ \a -> a ^. L.kind @?= Just CodeActionKind_QuickFix
                        forM_ allActions $ \a -> a ^. L.command @?= Nothing
                        forM_ allActions $ \a -> isJust (a ^. L.edit) @? "Has edit"

                    executeCodeAction removeAction

                Nothing -> error $ "Unexpected code actions: " ++ show allActions

            -- No command/applyworkspaceedit should be here, since action
            -- provides workspace edit property which skips round trip to
            -- the server
            contents <- documentContents doc
            liftIO $ contents @?= T.unlines
              [ "{-# OPTIONS_GHC -Wunused-imports #-}"
              , "{-# LANGUAGE PatternSynonyms #-}"
              , "module CodeActionRedundant where"
              , "-- We need a non-reduntant import in the import list"
              , "-- to properly test the removal of the singular redundant item"
              , "import Data.Sequence (singleton)"
              , "main :: IO ()"
              , "main = putStrLn \"hello\""
              , "  where unused = Data.Sequence.singleton 42"
              ]

    , testCase "doesn't touch other imports" $ runSession hlsCommand noLiteralCaps "test/testdata/redundantImportTest/" $ do
        doc <- openDoc "src/MultipleImports.hs" "haskell"
        _   <- waitForDiagnosticsFromSource doc (T.unpack sourceTypecheck)
        cas <- getAllCodeActions doc
        cmd <- liftIO $ inspectCommand cas ["redundant import"]
        executeCommand cmd
        _ <- anyRequest
        contents <- documentContents doc
        liftIO $ T.lines contents @?=
                [ "{-# OPTIONS_GHC -Wunused-imports #-}"
                , "module MultipleImports where"
                , "import Data.Maybe"
                , "foo :: Int"
                , "foo = fromJust (Just 3)"
                ]
    ]


typedHoleTests :: TestTree
typedHoleTests = testGroup "typed hole code actions" [
    testCase "works" $
        runSessionWithConfig (def {lspConfig = hlsConfigToClientConfig testConfig}) hlsCommand fullCaps "test/testdata" $ do
            doc <- openDoc "TypedHoles.hs" "haskell"
            _ <- waitForDiagnosticsFromSource doc (T.unpack sourceTypecheck)
            cas <- getAllCodeActions doc
            liftIO $ do
                expectCodeAction cas ["replace _ with minBound"]
                expectCodeAction cas ["replace _ with foo _"]
            replaceWithMaxBound <- liftIO $ inspectCodeAction cas ["replace _ with maxBound"]

            executeCodeAction replaceWithMaxBound

            contents <- documentContents doc

            liftIO $ contents @?= T.concat
                    [ "module TypedHoles where\n"
                    , "foo :: [Int] -> Int\n"
                    , "foo x = maxBound"
                    ]

      , knownBrokenForGhcVersions [GHC92, GHC94, GHC96] "The wingman plugin doesn't yet compile in GHC92/GHC94" $
          testCase "doesn't work when wingman is active" $
          runSession hlsCommand fullCaps "test/testdata" $ do
              doc <- openDoc "TypedHoles.hs" "haskell"
              _ <- waitForDiagnosticsFromSource doc (T.unpack sourceTypecheck)
              cas <- getAllCodeActions doc
              liftIO $ do
                  dontExpectCodeAction cas ["replace _ with minBound"]
                  dontExpectCodeAction cas ["replace _ with foo _"]

      , testCase "shows more suggestions" $
            runSessionWithConfig (def {lspConfig = hlsConfigToClientConfig testConfig}) hlsCommand fullCaps "test/testdata" $ do
                doc <- openDoc "TypedHoles2.hs" "haskell"
                _ <- waitForDiagnosticsFromSource doc (T.unpack sourceTypecheck)
                cas <- getAllCodeActions doc

                liftIO $ do
                    expectCodeAction cas ["replace _ with foo2 _"]
                    expectCodeAction cas ["replace _ with A _"]
                replaceWithStuff <- liftIO $ inspectCodeAction cas ["replace _ with stuff _"]

                executeCodeAction replaceWithStuff

                contents <- documentContents doc

                liftIO $ T.lines contents @?=
                        [ "module TypedHoles2 (foo2) where"
                        , "newtype A = A Int"
                        , "foo2 :: [A] -> A"
                        , "foo2 x = (stuff _)"
                        , "  where"
                        , "    stuff (A a) = A (a + 1)"
                        ]

      , knownBrokenForGhcVersions [GHC92, GHC94, GHC96] "The wingman plugin doesn't yet compile in GHC92/GHC94" $
          testCase "doesnt show more suggestions when wingman is active" $
            runSession hlsCommand fullCaps "test/testdata" $ do
                doc <- openDoc "TypedHoles2.hs" "haskell"
                _ <- waitForDiagnosticsFromSource doc (T.unpack sourceTypecheck)
                cas <- getAllCodeActions doc

                liftIO $ do
                    dontExpectCodeAction cas ["replace _ with foo2 _"]
                    dontExpectCodeAction cas ["replace _ with A _"]
    ]

signatureTests :: TestTree
signatureTests = testGroup "missing top level signature code actions" [
    testCase "Adds top level signature" $
      runSession hlsCommand fullCaps "test/testdata/" $ do
        doc <- openDoc "TopLevelSignature.hs" "haskell"

        _ <- waitForDiagnosticsFromSource doc (T.unpack sourceTypecheck)
        cas <- getAllCodeActions doc

        liftIO $ expectCodeAction cas ["add signature: main :: IO ()"]

        replaceWithStuff <- liftIO $ inspectCodeAction cas ["add signature"]
        executeCodeAction replaceWithStuff

        contents <- documentContents doc

        let expected = [ "{-# OPTIONS_GHC -Wall #-}"
                       , "module TopLevelSignature where"
                       , "main :: IO ()"
                       , "main = do"
                       , "  putStrLn \"Hello\""
                       , "  return ()"
                       ]

        liftIO $ T.lines contents @?= expected
    ]

unusedTermTests :: TestTree
unusedTermTests = testGroup "unused term code actions" [
    ignoreTestBecause "no support for prefixing unused names with _" $ testCase "Prefixes with '_'" $
        runSession hlsCommand fullCaps "test/testdata/" $ do
          doc <- openDoc "UnusedTerm.hs" "haskell"

          _ <- waitForDiagnosticsFromSource doc (T.unpack sourceTypecheck)
          cars <- getAllCodeActions doc
          prefixImUnused <- liftIO $ inspectCodeAction cars ["Prefix imUnused with _"]

          executeCodeAction prefixImUnused

          edit <- skipManyTill anyMessage $ getDocumentEdit doc

          let expected = [ "{-# OPTIONS_GHC -Wall #-}"
                         , "module UnusedTerm () where"
                         , "_imUnused :: Int -> Int"
                         , "_imUnused 1 = 1"
                         , "_imUnused 2 = 2"
                         , "_imUnused _ = 3"
                         ]

          liftIO $ edit @?= T.unlines expected

    -- See https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_codeAction
    -- `CodeActionContext`
    , testCase "respect 'only' parameter" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc   <- openDoc "CodeActionOnly.hs" "haskell"
        _     <- waitForDiagnosticsFrom doc
        diags <- getCurrentDiagnostics doc
        let params = CodeActionParams Nothing Nothing doc (Range (Position 1 0) (Position 4 0)) caContext
            caContext = CodeActionContext diags (Just [CodeActionKind_Refactor]) Nothing
            caContextAllActions = CodeActionContext diags Nothing Nothing
        -- Verify that we get code actions of at least two different kinds.
        TResponseMessage _ _ (Right res)
          <- request SMethod_TextDocumentCodeAction (params & L.context .~ caContextAllActions)
        liftIO $ do
            let cas = map fromAction $ absorbNull res
                kinds = map (^. L.kind) cas
            assertBool "Test precondition failed" $ Just CodeActionKind_QuickFix `elem` kinds
        -- Verify that that when we set the only parameter, we only get actions
        -- of the right kind.
        TResponseMessage _ _ (Right res) <- request SMethod_TextDocumentCodeAction params
        liftIO $ do
            let cas = map fromAction $ absorbNull res
                kinds = map (^. L.kind) cas
            assertBool "Quick fixes should have been filtered out"
              $ Just CodeActionKind_QuickFix `notElem` kinds
    ]

testConfig :: Config
testConfig = def {
  formattingProvider = "none"
  , plugins = M.insert "tactics" (def { plcGlobalOn = False }) (plugins def)
  }


