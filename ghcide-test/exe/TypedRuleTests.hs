module TypedRuleTests (tests) where

import           Config                       (testWithDummyPluginEmpty')
import           Control.Monad                (forM_)
import           Control.Monad.IO.Class       (liftIO)
import qualified Data.Text                    as T
import           Development.IDE.Core.InputPath
import           Development.IDE.Plugin.Test    (ideResultSuccess)
import           Development.IDE.Test         (waitForAction,
                                               waitForActionError)
import           Development.IDE.Types.Location
import           Language.LSP.Protocol.Types  hiding
                                              (SemanticTokenAbsolute (..),
                                               SemanticTokenRelative (..),
                                               SemanticTokensEdit (..),
                                               mkRange)
import           System.Directory             (createDirectoryIfMissing)
import           System.FilePath              (takeDirectory, (</>))
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "typed rules"
    [ testGroup "InputPath classifiers"
        [ testCase "dependency sources are not project Haskell inputs" $ do
            let dep = toNormalizedFilePath' "/work/.hls/dependencies/base/Data/Maybe.hs"
            toProjectHaskellInput dep @?= Nothing
            unInputPath (toAllHaskellInput dep) @?= dep

        , testCase "project Haskell inputs can be generalized to all Haskell inputs" $ do
            let src = toNormalizedFilePath' "/work/src/Foo.hs"
            case toProjectHaskellInput src of
                Nothing -> assertFailure "Expected project source to classify"
                Just input -> unInputPath (generalizeProjectInput input) @?= src

        , testCase "specific file classifiers reject unrelated paths" $ do
            let cabalFile = toNormalizedFilePath' "/work/pkg/pkg.cabal"
                stackYaml = toNormalizedFilePath' "/work/pkg/stack.yaml"
                source    = toNormalizedFilePath' "/work/pkg/Foo.hs"
            (unInputPath <$> toCabalFileInput cabalFile) @?= Just cabalFile
            toCabalFileInput source @?= Nothing
            (unInputPath <$> toStackYamlInput stackYaml) @?= Just stackYaml
            toStackYamlInput source @?= Nothing

        , testCase "bulk classifiers filter invalid paths" $ do
            let projectFile = toNormalizedFilePath' "/work/src/Foo.hs"
                depFile = toNormalizedFilePath' "/work/.hls/dependencies/pkg/Foo.hs"
                cabalFile = toNormalizedFilePath' "/work/pkg/pkg.cabal"
                stackYaml = toNormalizedFilePath' "/work/stack.yaml"
                files = [projectFile, depFile, cabalFile, stackYaml]
                nonDependencyFiles = [projectFile, cabalFile, stackYaml]
            fmap unInputPath (classifyProjectHaskellInputs files) @?=
                nonDependencyFiles
            fmap unInputPath (classifyCabalFileInputs files) @?= [cabalFile]
            fmap unInputPath (classifyStackYamlInputs files) @?= [stackYaml]

        , testCase "dependency classifier does not match similar directory names" $ do
            let dep = toNormalizedFilePath' "/work/.hls/dependencies/base/Data/Maybe.hs"
                dep2 = toNormalizedFilePath' "/work/.hls/dependencies2/base/Data/Maybe.hs"
                dep3 = toNormalizedFilePath' "/work/.hls/dependencies-extra/Foo.hs"
            toProjectHaskellInput dep @?= Nothing
            assertBool "dependencies2 should remain a project file" (toProjectHaskellInput dep2 /= Nothing)
            assertBool "dependencies-extra should remain a project file" (toProjectHaskellInput dep3 /= Nothing)

        , testCase "classifiers preserve ordering" $ do
            let a = toNormalizedFilePath' "/work/src/A.hs"
                b = toNormalizedFilePath' "/work/src/B.hs"
                c = toNormalizedFilePath' "/work/src/C.hs"

            fmap unInputPath (classifyProjectHaskellInputs [a,b,c]) @?= [a,b,c]

        , testCase "project source remains project source after generalization round trip" $ do
            let src = toNormalizedFilePath' "/work/src/Foo.hs"

            case toProjectHaskellInput src of
                Nothing -> assertFailure "Expected project source"
                Just input -> unInputPath (generalizeProjectInput input) @?= src
        ]

    , testWithDummyPluginEmpty' "project-only rules reject dependency-source inputs" $ \dir -> do
        let dependencyFile = dir </> ".hls" </> "dependencies" </> "pkg" </> "Data" </> "Maybe.hs"
            dependencyDoc = TextDocumentIdentifier (filePathToUri dependencyFile)
            projectOnlyRules =
                [ "typecheck"
                , "getLocatedImports"
                , "getmodsummary"
                , "getmodsummarywithouttimestamps"
                , "getparsedmodule"
                , "ghcsession"
                , "ghcsessiondeps"
                ]
        liftIO $ createDirectoryIfMissing True (takeDirectory dependencyFile)
        liftIO $ writeFile dependencyFile "module Data.Maybe where\n"

        forM_ projectOnlyRules $ \rule -> do
            err <- waitForActionError rule dependencyDoc
            liftIO $ assertBool ("Unexpected error for " <> rule <> ": " <> T.unpack err) $
                "dependency file" `T.isInfixOf` err

        -- Dependency source files still support all-Haskell/file-content rules.
        fileContents <- waitForAction "getFileContents" dependencyDoc
        liftIO $ assertBool "GetFileContents should accept dependency sources" $
            ideResultSuccess fileContents

    , testWithDummyPluginEmpty' "all-haskell rules continue to accept dependency sources" $ \dir -> do
        let dependencyFile = dir </> ".hls" </> "dependencies" </> "pkg" </> "Foo.hs"
            dependencyDoc = TextDocumentIdentifier (filePathToUri dependencyFile)

        liftIO $ createDirectoryIfMissing True (takeDirectory dependencyFile)
        liftIO $ writeFile dependencyFile "module Foo where\nx = 1\n"
        fileContents <- waitForAction "getFileContents" dependencyDoc
        liftIO $ assertBool "GetFileContents should succeed" (ideResultSuccess fileContents)
    ]
