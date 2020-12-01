{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module FunctionalCodeAction (tests) where

import           Control.Applicative.Combinators
import           Control.Lens hiding (List)
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Maybe
import qualified Data.Text as T
import           Ide.Plugin.Config
import           Language.Haskell.LSP.Test as Test
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as L
import qualified Language.Haskell.LSP.Types.Capabilities as C
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.ExpectedFailure (ignoreTestBecause)
import           Test.Tasty.HUnit

{-# ANN module ("HLint: ignore Reduce duplication"::String) #-}

tests :: TestTree
tests = testGroup "code actions" [
      hlintTests
    , importTests
    , missingPragmaTests
    , packageTests
    , redundantImportTests
    , renameTests
    , signatureTests
    , typedHoleTests
    , unusedTermTests
    ]


hlintTests :: TestTree
hlintTests = testGroup "hlint suggestions" [
    testCase "provides 3.8 code actions including apply all" $ runSession hlsCommand fullCaps "test/testdata/hlint" $ do
        doc <- openDoc "ApplyRefact2.hs" "haskell"
        diags@(reduceDiag:_) <- waitForDiagnosticsSource "hlint"

        liftIO $ do
            length diags @?= 2 -- "Eta Reduce" and "Redundant Id"
            reduceDiag ^. L.range @?= Range (Position 1 0) (Position 1 12)
            reduceDiag ^. L.severity @?= Just DsInfo
            reduceDiag ^. L.code @?= Just (StringValue "refact:Eta reduce")
            reduceDiag ^. L.source @?= Just "hlint"

        cas <- map fromAction <$> getAllCodeActions doc

        let applyAll = find (\ca -> "Apply all hints" `T.isSuffixOf` (ca ^. L.title)) cas
        let redId = find (\ca -> "Redundant id" `T.isSuffixOf` (ca ^. L.title)) cas
        let redEta = find (\ca -> "Eta reduce" `T.isSuffixOf` (ca ^. L.title)) cas

        liftIO $ isJust applyAll @? "There is 'Apply all hints' code action"
        liftIO $ isJust redId @? "There is 'Redundant id' code action"
        liftIO $ isJust redEta @? "There is 'Eta reduce' code action"

        executeCodeAction (fromJust redId)

        contents <- skipManyTill anyMessage $ getDocumentEdit doc
        liftIO $ contents @?= "main = undefined\nfoo x = x\n"

    , testCase "falls back to pre 3.8 code actions" $ runSession hlsCommand noLiteralCaps "test/testdata/hlint" $ do
        doc <- openDoc "ApplyRefact2.hs" "haskell"

        _ <- waitForDiagnosticsSource "hlint"

        cars <- getAllCodeActions doc
        etaReduce <- liftIO $ inspectCommand cars ["Apply hint: Eta reduce"]

        executeCommand etaReduce

        contents <- skipManyTill anyMessage $ getDocumentEdit doc
        liftIO $ contents @?= "main = undefined\nfoo = id\n"

    , testCase "changing configuration enables or disables hlint diagnostics" $ runSession hlsCommand fullCaps "test/testdata/hlint" $ do
        let config = def { hlintOn = True }
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

        _ <- openDoc "ApplyRefact2.hs" "haskell"
        diags <- waitForDiagnosticsSource "hlint"

        liftIO $ length diags > 0 @? "There are hlint diagnostics"

        let config' = def { hlintOn = False }
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config'))

        diags' <- waitForDiagnostics

        liftIO $ Just "hlint" `notElem` map (^. L.source) diags' @? "There are no hlint diagnostics"

    , testCase "changing document contents updates hlint diagnostics" $ runSession hlsCommand fullCaps "test/testdata/hlint" $ do
        doc <- openDoc "ApplyRefact2.hs" "haskell"
        diags <- waitForDiagnosticsSource "hlint"

        liftIO $ length diags @?= 2 -- "Eta Reduce" and "Redundant Id"

        let change = TextDocumentContentChangeEvent
                        (Just (Range (Position 1 8) (Position 1 12)))
                         Nothing "x"

        changeDoc doc [change]

        diags' <- waitForDiagnostics

        liftIO $ (not $ Just "hlint" `elem` map (^. L.source) diags') @? "There are no hlint diagnostics"

        let change' = TextDocumentContentChangeEvent
                        (Just (Range (Position 1 8) (Position 1 12)))
                         Nothing "id x"

        changeDoc doc [change']

        diags'' <- waitForDiagnosticsSource "hlint"

        liftIO $ length diags'' @?= 2
    ]

renameTests :: TestTree
renameTests = testGroup "rename suggestions" [
    testCase "works" $ runSession hlsCommand noLiteralCaps "test/testdata" $ do
        doc <- openDoc "CodeActionRename.hs" "haskell"

        _ <- waitForDiagnosticsSource "typecheck"

        cars <- getAllCodeActions doc
        replaceButStrLn <- liftIO $ inspectCommand cars ["Replace with", "putStrLn"]
        executeCommand replaceButStrLn

        x:_ <- T.lines <$> documentContents doc
        liftIO $ x @?= "main = putStrLn \"hello\""

    , testCase "doesn't give both documentChanges and changes"
        $ runSession hlsCommand noLiteralCaps "test/testdata" $ do
            doc <- openDoc "CodeActionRename.hs" "haskell"

            _ <- waitForDiagnosticsSource "typecheck"

            cars <- getAllCodeActions doc
            cmd <- liftIO $ inspectCommand cars ["Replace with", "putStrLn"]
            let Just (List [Object args]) = cmd ^. L.arguments
                Object editParams = args HM.! "fallbackWorkspaceEdit"
            liftIO $ do
                "changes" `HM.member` editParams @? "Contains changes"
                not ("documentChanges" `HM.member` editParams) @? "Doesn't contain documentChanges"

            executeCommand cmd

            x1:x2:_ <- T.lines <$> documentContents doc
            liftIO $
                x1 == "main = putStrLn \"hello\""
                || x2 == "foo = putStrLn \"world\""
                @? "One of the typos got fixed"
    ]

importTests :: TestTree
importTests = testGroup "import suggestions" [
    testCase "works with 3.8 code action kinds" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "CodeActionImport.hs" "haskell"
        -- No Formatting:
        let config = def { formattingProvider = "none" }
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

        diag:_ <- waitForDiagnostics
        liftIO $ diag ^. L.message @?= "Variable not in scope: when :: Bool -> IO () -> IO ()"

        actionsOrCommands <- getAllCodeActions doc
        let actns = map fromAction actionsOrCommands

        importControlMonad <- liftIO $ inspectCodeAction actionsOrCommands ["import Control.Monad"]
        liftIO $ do
            expectCodeAction actionsOrCommands ["import Control.Monad (when)"]
            forM_ actns $ \a -> do
                a ^. L.kind @?= Just CodeActionQuickFix
            length actns >= 10 @? "There are some actions"

        executeCodeAction importControlMonad

        contents <- documentContents doc
        liftIO $ contents @?= "import Control.Monad\nmain :: IO ()\nmain = when True $ putStrLn \"hello\""
    ]

packageTests :: TestTree
packageTests = testGroup "add package suggestions" [
    ignoreTestBecause "no support for adding dependent packages via code action" $ testCase "adds to .cabal files" $ do
        flushStackEnvironment
        runSession hlsCommand fullCaps "test/testdata/addPackageTest/cabal-exe" $ do
            doc <- openDoc "AddPackage.hs" "haskell"

            -- ignore the first empty hlint diagnostic publish
            [_,diag:_] <- count 2 waitForDiagnostics

            let prefixes = [ "Could not load module `Data.Text'" -- Windows && GHC >= 8.6
                        , "Could not find module `Data.Text'" -- Windows
                        , "Could not load module ‘Data.Text’" -- GHC >= 8.6
                        , "Could not find module ‘Data.Text’"
                        ]
                in liftIO $ any (`T.isPrefixOf` (diag ^. L.message)) prefixes @? "Contains prefix"

            acts <- getAllCodeActions doc
            let (CACodeAction action:_) = acts

            liftIO $ do
                action ^. L.title @?= "Add text as a dependency"
                action ^. L.kind @?= Just CodeActionQuickFix
                "package:add" `T.isSuffixOf` (action ^. L.command . _Just . L.command) @? "Command contains package:add"

            executeCodeAction action

            contents <- skipManyTill anyMessage $ getDocumentEdit . TextDocumentIdentifier =<< getDocUri "add-package-test.cabal"
            liftIO $
                any (\l -> "text -any" `T.isSuffixOf` l || "text : {} -any" `T.isSuffixOf` l) (T.lines contents) @? "Contains text package"

    , ignoreTestBecause "no support for adding dependent packages via code action" $ testCase "adds to hpack package.yaml files" $
        runSession hlsCommand fullCaps "test/testdata/addPackageTest/hpack-exe" $ do
            doc <- openDoc "app/Asdf.hs" "haskell"

            -- ignore the first empty hlint diagnostic publish
            [_,_:diag:_] <- count 2 waitForDiagnostics

            let prefixes = [ "Could not load module `Codec.Compression.GZip'" -- Windows && GHC >= 8.6
                        , "Could not find module `Codec.Compression.GZip'" -- Windows
                        , "Could not load module ‘Codec.Compression.GZip’" -- GHC >= 8.6
                        , "Could not find module ‘Codec.Compression.GZip’"
                        ]
                in liftIO $ any (`T.isPrefixOf` (diag ^. L.message)) prefixes @? "Diagnostic contains message"

            mActions <- getAllCodeActions doc
            let allActions = map fromAction mActions
                action = head allActions

            liftIO $ do
                action ^. L.title @?= "Add zlib as a dependency"
                forM_ allActions $ \a -> a ^. L.kind @?= Just CodeActionQuickFix
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

            diags <- waitForDiagnostics
            liftIO $ expectDiagnostic diags ["The import of", "Data.List", "is redundant"]

            mActions <- getAllCodeActions doc

            let allActions@[removeAction, removeAllAction, makeAllExplicitAction] = map fromAction mActions

            liftIO $ do
                removeAction ^. L.title @?= "Remove import"
                removeAllAction ^. L.title @?= "Remove all redundant imports"
                makeAllExplicitAction ^. L.title @?= "Make all imports explicit"
                forM_ allActions $ \a -> a ^. L.kind @?= Just CodeActionQuickFix
                forM_ allActions $ \a -> a ^. L.command @?= Nothing
                forM_ allActions $ \a -> isJust (a ^. L.edit) @? "Has edit"

            executeCodeAction removeAction

            -- No command/applyworkspaceedit should be here, since action
            -- provides workspace edit property which skips round trip to
            -- the server
            contents <- documentContents doc
            liftIO $ contents @?= "module CodeActionRedundant where\nmain :: IO ()\nmain = putStrLn \"hello\""

    , testCase "doesn't touch other imports" $ runSession hlsCommand noLiteralCaps "test/testdata/redundantImportTest/" $ do
        doc <- openDoc "src/MultipleImports.hs" "haskell"
        _   <- waitForDiagnostics
        CACommand cmd : _ <- getAllCodeActions doc
        executeCommand cmd
        contents <- documentContents doc
        liftIO $ (T.lines contents) @?=
                [ "module MultipleImports where"
                , "import Data.Maybe"
                , "foo :: Int"
                , "foo = fromJust (Just 3)"
                ]
    ]

typedHoleTests :: TestTree
typedHoleTests = testGroup "typed hole code actions" [
    testCase "works" $
        runSession hlsCommand fullCaps "test/testdata" $ do
            doc <- openDoc "TypedHoles.hs" "haskell"
            _ <- waitForDiagnosticsSource "typecheck"
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

      , testCase "shows more suggestions" $
            runSession hlsCommand fullCaps "test/testdata" $ do
                doc <- openDoc "TypedHoles2.hs" "haskell"
                _ <- waitForDiagnosticsSource "typecheck"
                cas <- getAllCodeActions doc

                liftIO $ do
                    expectCodeAction cas ["replace _ with foo2 _"]
                    expectCodeAction cas ["replace _ with A _"]
                replaceWithStuff <- liftIO $ inspectCodeAction cas ["replace _ with stuff _"]

                executeCodeAction replaceWithStuff

                contents <- documentContents doc

                liftIO $ (T.lines contents) @?=
                        [ "module TypedHoles2 (foo2) where"
                        , "newtype A = A Int"
                        , "foo2 :: [A] -> A"
                        , "foo2 x = (stuff _)"
                        , "  where"
                        , "    stuff (A a) = A (a + 1)"
                        ]
    ]

signatureTests :: TestTree
signatureTests = testGroup "missing top level signature code actions" [
    testCase "Adds top level signature" $
      runSession hlsCommand fullCaps "test/testdata/" $ do
        doc <- openDoc "TopLevelSignature.hs" "haskell"

        _ <- waitForDiagnosticsSource "typecheck"
        cas <- map fromAction <$> getAllCodeActions doc

        liftIO $ "add signature: main :: IO ()" `elem` (map (^. L.title) cas) @? "Contains code action"

        executeCodeAction $ head cas

        contents <- documentContents doc

        let expected = [ "{-# OPTIONS_GHC -Wall #-}"
                       , "module TopLevelSignature where"
                       , "main :: IO ()"
                       , "main = do"
                       , "  putStrLn \"Hello\""
                       , "  return ()"
                       ]

        liftIO $ (T.lines contents) @?= expected
    ]

missingPragmaTests :: TestTree
missingPragmaTests = testGroup "missing pragma warning code actions" [
    testCase "Adds TypeSynonymInstances pragma" $ do
        runSession hlsCommand fullCaps "test/testdata/addPragmas" $ do
            doc <- openDoc "NeedsPragmas.hs" "haskell"

            _ <- waitForDiagnosticsSource "typecheck"
            cas <- map fromAction <$> getAllCodeActions doc

            liftIO $ "Add \"TypeSynonymInstances\"" `elem` map (^. L.title) cas @? "Contains TypeSynonymInstances code action"
            liftIO $ "Add \"FlexibleInstances\"" `elem` map (^. L.title) cas @? "Contains FlexibleInstances code action"

            executeCodeAction $ head cas

            contents <- documentContents doc

            let expected = [ "{-# LANGUAGE TypeSynonymInstances #-}"
                        , ""
                        , "import GHC.Generics"
                        , ""
                        , "main = putStrLn \"hello\""
                        , ""
                        , "type Foo = Int"
                        , ""
                        , "instance Show Foo where"
                        , "  show x = undefined"
                        , ""
                        , "instance Show (Int,String) where"
                        , "  show  = undefined"
                        , ""
                        , "data FFF a = FFF Int String a"
                        , "           deriving (Generic,Functor,Traversable)"
                        ]

            liftIO $ (T.lines contents) @?= expected
    ]

unusedTermTests :: TestTree
unusedTermTests = testGroup "unused term code actions" [
    -- ignoreTestBecause "Broken" $ testCase "Prefixes with '_'" $ pendingWith "removed because of HaRe"
    --     runSession hlsCommand fullCaps "test/testdata/" $ do
    --       doc <- openDoc "UnusedTerm.hs" "haskell"
    --
    --       _ <- waitForDiagnosticsSource "typecheck"
    --       cas <- map fromAction <$> getAllCodeActions doc
    --
    --       liftIO $ map (^. L.title) cas `shouldContain` [ "Prefix imUnused with _"]
    --
    --       executeCodeAction $ head cas
    --
    --       edit <- skipManyTill anyMessage $ getDocumentEdit doc
    --
    --       let expected = [ "{-# OPTIONS_GHC -Wall #-}"
    --                      , "module UnusedTerm () where"
    --                      , "_imUnused :: Int -> Int"
    --                      , "_imUnused 1 = 1"
    --                      , "_imUnused 2 = 2"
    --                      , "_imUnused _ = 3"
    --                      ]
    --
    --       liftIO $ edit @?= T.unlines expected

    -- See https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_codeAction
    -- `CodeActionContext`
    testCase "respect 'only' parameter" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc   <- openDoc "CodeActionOnly.hs" "haskell"
        _     <- waitForDiagnostics
        diags <- getCurrentDiagnostics doc
        let params = CodeActionParams doc (Range (Position 2 10) (Position 4 0)) caContext Nothing
            caContext = CodeActionContext (List diags) (Just (List [CodeActionRefactorInline]))
        ResponseMessage _ _ (Right (List res)) <- request TextDocumentCodeAction params
        let cas = map fromAction res
            kinds = map (^. L.kind) cas
        liftIO $ do
            -- TODO: When HaRe is back this should be uncommented
            -- kinds `shouldNotSatisfy` null
            not (any (Just CodeActionRefactorInline /=) kinds) @? "None not CodeActionRefactorInline"
            all (Just CodeActionRefactorInline ==) kinds @? "All CodeActionRefactorInline"
    ]

fromAction :: CAResult -> CodeAction
fromAction (CACodeAction action) = action
fromAction _ = error "Not a code action"

fromCommand :: CAResult -> Command
fromCommand (CACommand command) = command
fromCommand _ = error "Not a command"

noLiteralCaps :: C.ClientCapabilities
noLiteralCaps = def { C._textDocument = Just textDocumentCaps }
  where
    textDocumentCaps = def { C._codeAction = Just codeActionCaps }
    codeActionCaps = C.CodeActionClientCapabilities (Just True) Nothing

onMatch :: [a] -> (a -> Bool) -> String -> IO a
onMatch as pred err = maybe (fail err) return (find pred as)

inspectDiagnostic :: [Diagnostic] -> [T.Text] -> IO Diagnostic
inspectDiagnostic diags s = onMatch diags (\ca -> all (`T.isInfixOf` (ca ^. L.message)) s) err
    where err = "expected diagnostic matching '" ++ show s ++ "' but did not find one"

expectDiagnostic :: [Diagnostic] -> [T.Text] -> IO ()
expectDiagnostic diags s = void $ inspectDiagnostic diags s

inspectCodeAction :: [CAResult] -> [T.Text] -> IO CodeAction
inspectCodeAction cars s = fromAction <$> onMatch cars pred err
    where pred (CACodeAction ca) = all (`T.isInfixOf` (ca ^. L.title)) s
          pred _ = False
          err = "expected code action matching '" ++ show s ++ "' but did not find one"

expectCodeAction :: [CAResult] -> [T.Text] -> IO ()
expectCodeAction cars s = void $ inspectCodeAction cars s

inspectCommand :: [CAResult] -> [T.Text] -> IO Command
inspectCommand cars s = fromCommand <$> onMatch cars pred err
    where pred (CACommand command) = all  (`T.isInfixOf` (command ^. L.title)) s
          pred _ = False
          err = "expected code action matching '" ++ show s ++ "' but did not find one"
