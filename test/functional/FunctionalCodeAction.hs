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
#if __GLASGOW_HASKELL__ < 808
import           Data.Monoid ((<>))
#endif
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
    ignoreTestBecause "Broken" $ testCase "provides 3.8 code actions" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "ApplyRefact2.hs" "haskell"
        diags@(reduceDiag:_) <- waitForDiagnostics

        liftIO $ do
            length diags @?= 2
            reduceDiag ^. L.range @?= Range (Position 1 0) (Position 1 12)
            reduceDiag ^. L.severity @?= Just DsInfo
            reduceDiag ^. L.code @?= Just (StringValue "Eta reduce")
            reduceDiag ^. L.source @?= Just "hlint"

        (CACodeAction ca:_) <- getAllCodeActions doc

        -- Evaluate became redundant id in later hlint versions
        liftIO $ (ca ^. L.title) `elem` ["Apply hint:Redundant id", "Apply hint:Evaluate"] @? "Title contains evaluate"

        executeCodeAction ca

        contents <- getDocumentEdit doc
        liftIO $ contents @?= "main = undefined\nfoo x = x\n"

        noDiagnostics

    , ignoreTestBecause "Broken" $ testCase "falls back to pre 3.8 code actions" $ runSession hlsCommand noLiteralCaps "test/testdata" $ do
        doc <- openDoc "ApplyRefact2.hs" "haskell"

        _ <- waitForDiagnostics

        (CACommand cmd:_) <- getAllCodeActions doc

        -- Evaluate became redundant id in later hlint versions
        liftIO $ (cmd ^. L.title) `elem` ["Apply hint:Redundant id", "Apply hint:Evaluate"] @? "Title contains evaluate"

        executeCommand cmd

        contents <- skipManyTill publishDiagnosticsNotification $ getDocumentEdit doc
        liftIO $ contents @?= "main = undefined\nfoo x = x\n"

        noDiagnostics

    , ignoreTestBecause "Broken" $ testCase "runs diagnostics on save" $ runSession hlsCommand fullCaps "test/testdata" $ do
        let config = def { diagnosticsOnChange = False }
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

        doc <- openDoc "ApplyRefact2.hs" "haskell"
        diags@(reduceDiag:_) <- waitForDiagnostics

        liftIO $ do
            length diags @?= 2
            reduceDiag ^. L.range @?= Range (Position 1 0) (Position 1 12)
            reduceDiag ^. L.severity @?= Just DsInfo
            reduceDiag ^. L.code @?= Just (StringValue "Eta reduce")
            reduceDiag ^. L.source @?= Just "hlint"

        (CACodeAction ca:_) <- getAllCodeActions doc

        -- Evaluate became redundant id in later hlint versions
        liftIO $ (ca ^. L.title) `elem` ["Apply hint:Redundant id", "Apply hint:Evaluate"] @? "Title contains evaluate"

        executeCodeAction ca

        contents <- getDocumentEdit doc
        liftIO $ contents @?= "main = undefined\nfoo x = x\n"
        sendNotification TextDocumentDidSave (DidSaveTextDocumentParams doc)

        noDiagnostics
    ]

renameTests :: TestTree
renameTests = testGroup "rename suggestions" [
    ignoreTestBecause "Broken" $ testCase "works" $ runSession hlsCommand noLiteralCaps "test/testdata" $ do
        doc <- openDoc "CodeActionRename.hs" "haskell"

        _ <- waitForDiagnosticsSource "bios"

        CACommand cmd:_ <- getAllCodeActions doc
        executeCommand cmd

        x:_ <- T.lines <$> documentContents doc
        liftIO $ x @?= "main = putStrLn \"hello\""

    , ignoreTestBecause "Broken" $ testCase "doesn't give both documentChanges and changes"
        $ runSession hlsCommand noLiteralCaps "test/testdata" $ do
            doc <- openDoc "CodeActionRename.hs" "haskell"

            _ <- waitForDiagnosticsSource "bios"

            CACommand cmd <- (!! 2) <$> getAllCodeActions doc
            let Just (List [Object args]) = cmd ^. L.arguments
                Object editParams = args HM.! "fallbackWorkspaceEdit"
            liftIO $ do
                "changes" `HM.member` editParams @? "Contains changes"
                not ("documentChanges" `HM.member` editParams) @? "Doesn't contain documentChanges"

            executeCommand cmd

            _:x:_ <- T.lines <$> documentContents doc
            liftIO $ x @?= "foo = putStrLn \"world\""
    ]

importTests :: TestTree
importTests = testGroup "import suggestions" [
    ignoreTestBecause "Broken" $ testCase "works with 3.8 code action kinds" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "CodeActionImport.hs" "haskell"
        -- No Formatting:
        let config = def { formattingProvider = "none" }
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

        -- ignore the first empty hlint diagnostic publish
        [_,diag:_] <- count 2 waitForDiagnostics
        liftIO $ diag ^. L.message @?= "Variable not in scope: when :: Bool -> IO () -> IO ()"

        actionsOrCommands <- getAllCodeActions doc
        let actns = map fromAction actionsOrCommands

        liftIO $ do
            head actns        ^. L.title @?= "Import module Control.Monad"
            head (tail actns) ^. L.title @?= "Import module Control.Monad (when)"
            forM_ actns $ \a -> do
                a ^. L.kind @?= Just CodeActionQuickFix
                isJust (a ^. L.command) @? "Contains command"
                a ^. L.edit @?= Nothing
                let hasOneDiag (Just (List [_])) = True
                    hasOneDiag _ = False
                hasOneDiag (a ^. L.diagnostics) @? "Has one diagnostic"
            length actns @?= 10

        executeCodeAction (head actns)

        contents <- getDocumentEdit doc
        liftIO $ contents @?= "import Control.Monad\nmain :: IO ()\nmain = when True $ putStrLn \"hello\""
    ]

packageTests :: TestTree
packageTests = testGroup "add package suggestions" [
    ignoreTestBecause "Broken" $ testCase "adds to .cabal files" $ do
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

            contents <- getDocumentEdit . TextDocumentIdentifier =<< getDocUri "add-package-test.cabal"
            liftIO $
                any (\l -> "text -any" `T.isSuffixOf` l || "text : {} -any" `T.isSuffixOf` l) (T.lines contents) @? "Contains text package"

    , ignoreTestBecause "Broken" $ testCase "adds to hpack package.yaml files" $
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

            contents <- getDocumentEdit . TextDocumentIdentifier =<< getDocUri "package.yaml"
            liftIO $ do
                "zlib" `T.isSuffixOf` (T.lines contents !! 3) @? "Contains zlib"
                "zlib" `T.isSuffixOf` (T.lines contents !! 21) @? "Does not contain zlib in unrelated component"
    ]

redundantImportTests :: TestTree
redundantImportTests = testGroup "redundant import code actions" [
    ignoreTestBecause "Broken" $ testCase "remove solitary redundant imports" $
        runSession hlsCommand fullCaps "test/testdata/redundantImportTest/" $ do
            doc <- openDoc "src/CodeActionRedundant.hs" "haskell"

            -- ignore the first empty hlint diagnostic publish
            [_,diag:_] <- count 2 waitForDiagnostics

            let prefixes = [ "The import of `Data.List' is redundant" -- Windows
                        , "The import of ‘Data.List’ is redundant"
                        ]
                in liftIO $ any (`T.isPrefixOf` (diag ^. L.message)) prefixes @? "Contains message"

            mActions <- getAllCodeActions doc

            let allActions@[removeAction, changeAction] = map fromAction mActions

            liftIO $ do
                removeAction ^. L.title @?= "Remove redundant import"
                changeAction ^. L.title @?= "Import instances"
                forM_ allActions $ \a -> a ^. L.kind @?= Just CodeActionQuickFix
                forM_ allActions $ \a -> a ^. L.command @?= Nothing
                forM_ allActions $ \a -> isJust (a ^. L.edit) @? "Has edit"

            executeCodeAction removeAction

            -- No command/applyworkspaceedit should be here, since action
            -- provides workspace edit property which skips round trip to
            -- the server
            contents <- documentContents doc
            liftIO $ contents @?= "module CodeActionRedundant where\nmain :: IO ()\nmain = putStrLn \"hello\""

    , ignoreTestBecause "Broken" $ testCase "doesn't touch other imports" $ runSession hlsCommand noLiteralCaps "test/testdata/redundantImportTest/" $ do
        doc <- openDoc "src/MultipleImports.hs" "haskell"
        _   <- count 2 waitForDiagnostics
        [CACommand cmd, _] <- getAllCodeActions doc
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
    ignoreTestBecause "Broken" $ testCase "works" $
        runSession hlsCommand fullCaps "test/testdata" $ do
            doc <- openDoc "TypedHoles.hs" "haskell"
            _ <- waitForDiagnosticsSource "bios"
            cas <- map (\(CACodeAction x)-> x) <$> getAllCodeActions doc

            suggestion <-
                case ghcVersion of
                GHC88 -> do
                    liftIO $ map (^. L.title) cas `matchList`
                        [ "Substitute hole (Int) with x ([Int])"
                        , "Substitute hole (Int) with foo ([Int] -> Int Valid hole fits include)"
                        , "Substitute hole (Int) with maxBound (forall a. Bounded a => a with maxBound @Int)"
                        , "Substitute hole (Int) with minBound (forall a. Bounded a => a with minBound @Int)"
                        ] @? "Contains substitutions"
                    return "x"
                GHC86 -> do
                    liftIO $ map (^. L.title) cas `matchList`
                        [ "Substitute hole (Int) with x ([Int])"
                        , "Substitute hole (Int) with foo ([Int] -> Int Valid hole fits include)"
                        , "Substitute hole (Int) with maxBound (forall a. Bounded a => a with maxBound @Int)"
                        , "Substitute hole (Int) with minBound (forall a. Bounded a => a with minBound @Int)"
                        ] @? "Contains substitutions"
                    return "x"
                GHC84 -> do
                    liftIO $ map (^. L.title) cas `matchList`
                        [ "Substitute hole (Int) with maxBound (forall a. Bounded a => a)"
                        , "Substitute hole (Int) with minBound (forall a. Bounded a => a)"
                        , "Substitute hole (Int) with undefined (forall (a :: TYPE r). GHC.Stack.Types.HasCallStack => a)"
                        ] @? "Contains substitutions"
                    return "maxBound"

            executeCodeAction $ head cas

            contents <- documentContents doc

            liftIO $ contents @?= T.concat
                    [ "module TypedHoles where\n"
                    , "foo :: [Int] -> Int\n"
                    , "foo x = " <> suggestion
                    ]

      , ignoreTestBecause "Broken" $ testCase "shows more suggestions" $
            runSession hlsCommand fullCaps "test/testdata" $ do
                doc <- openDoc "TypedHoles2.hs" "haskell"
                _ <- waitForDiagnosticsSource "bios"
                cas <- map fromAction <$> getAllCodeActions doc

                suggestion <-
                    case ghcVersion of
                    GHC88 -> do
                        liftIO $ map (^. L.title) cas `matchList`
                            [ "Substitute hole (A) with stuff (A -> A)"
                            , "Substitute hole (A) with x ([A])"
                            , "Substitute hole (A) with foo2 ([A] -> A)"
                            ] @? "Contains substitutions"
                        return "stuff"
                    GHC86 -> do
                        liftIO $ map (^. L.title) cas `matchList`
                            [ "Substitute hole (A) with stuff (A -> A)"
                            , "Substitute hole (A) with x ([A])"
                            , "Substitute hole (A) with foo2 ([A] -> A)"
                            ] @? "Contains substituions"
                        return "stuff"
                    GHC84 -> do
                        liftIO $ map (^. L.title) cas `matchList`
                            [ "Substitute hole (A) with undefined (forall (a :: TYPE r). GHC.Stack.Types.HasCallStack => a)"
                            , "Substitute hole (A) with stuff (A -> A)"
                            , "Substitute hole (A) with x ([A])"
                            , "Substitute hole (A) with foo2 ([A] -> A)"
                            ] @? "Contains substitutions"
                        return "undefined"

                executeCodeAction $ head cas

                contents <- documentContents doc

                liftIO $ (T.lines contents) @?=
                        [ "module TypedHoles2 (foo2) where"
                        , "newtype A = A Int"
                        , "foo2 :: [A] -> A"
                        , "foo2 x = " <> suggestion <> ""
                        , "  where"
                        , "    stuff (A a) = A (a + 1)"
                        ]
    ]
    where
        -- | 'True' if @xs@ contains all of @ys@, possibly in a different order.
        matchList :: (Eq a) => [a] -> [a] -> Bool
        xs `matchList` ys
          | null extra && null missing = True
          | otherwise = False
          where
            extra   = xs \\ ys
            missing = ys \\ xs

signatureTests :: TestTree
signatureTests = testGroup "missing top level signature code actions" [
    ignoreTestBecause "Broken" $ testCase "Adds top level signature" $
      runSession hlsCommand fullCaps "test/testdata/" $ do
        doc <- openDoc "TopLevelSignature.hs" "haskell"

        _ <- waitForDiagnosticsSource "bios"
        cas <- map fromAction <$> getAllCodeActions doc

        liftIO $ "Add signature: main :: IO ()" `elem` (map (^. L.title) cas) @? "Contains code action"

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
    ignoreTestBecause "Broken" $ testCase "Adds TypeSynonymInstances pragma" $
        runSession hlsCommand fullCaps "test/testdata/addPragmas" $ do
            doc <- openDoc "NeedsPragmas.hs" "haskell"

            _ <- waitForDiagnosticsSource "bios"
            cas <- map fromAction <$> getAllCodeActions doc

            liftIO $ "Add \"TypeSynonymInstances\"" `elem` map (^. L.title) cas @? "Contains TypeSynonymInstances code action"
            liftIO $ "Add \"FlexibleInstances\"" `elem` map (^. L.title) cas @? "Contains FlexibleInstances code action"

            executeCodeAction $ head cas

            contents <- getDocumentEdit doc

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
    --       _ <- waitForDiagnosticsSource "bios"
    --       cas <- map fromAction <$> getAllCodeActions doc
    --
    --       liftIO $ map (^. L.title) cas `shouldContain` [ "Prefix imUnused with _"]
    --
    --       executeCodeAction $ head cas
    --
    --       edit <- getDocumentEdit doc
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
    ignoreTestBecause "Broken" $ testCase "respect 'only' parameter" $ runSession hlsCommand fullCaps "test/testdata" $ do
        doc   <- openDoc "CodeActionOnly.hs" "haskell"
        _     <- count 2 waitForDiagnostics -- need to wait for both hlint and ghcmod
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

noLiteralCaps :: C.ClientCapabilities
noLiteralCaps = def { C._textDocument = Just textDocumentCaps }
  where
    textDocumentCaps = def { C._codeAction = Just codeActionCaps }
    codeActionCaps = C.CodeActionClientCapabilities (Just True) Nothing
