{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
module Dependency where

import qualified Control.Applicative             as Applicative
import           Control.Applicative.Combinators (skipManyTill)
import           Control.Lens                    (preview, (^.))
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.Aeson                      as A
import           Data.Bool                       (bool)
import           Data.List                       (isSuffixOf)
import           Data.Maybe                      (fromMaybe)
import           Data.Proxy                      (Proxy (..))
import           Data.Text                       (isPrefixOf)
import           Development.IDE.GHC.Compat      (GhcVersion (..))
import qualified Language.LSP.Protocol.Lens      as L
import           Language.LSP.Protocol.Message   (FromServerMessage' (FromServerMess),
                                                  SMethod (SMethod_Progress, SMethod_TextDocumentPublishDiagnostics),
                                                  TCustomMessage (NotMess),
                                                  TNotificationMessage (..))
import           Language.LSP.Protocol.Types     (Definition (..), Diagnostic,
                                                  Location (..), Position (..),
                                                  ProgressParams (..),
                                                  Range (..),
                                                  WorkDoneProgressEnd (..),
                                                  _workDoneProgressEnd,
                                                  type (|?) (InL, InR),
                                                  uriToFilePath)
import           Language.LSP.Test               (Session, anyMessage,
                                                  customNotification,
                                                  getDefinitions, message,
                                                  openDoc, satisfyMaybe,
                                                  waitForDiagnostics)
import           System.FilePath                 (splitDirectories, (<.>),
                                                  (</>))
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.ExpectedFailure      (expectFailBecause)
import           Test.Tasty.HUnit                (assertBool, assertFailure,
                                                  (@?=))
import           TestUtils                       (knownBrokenForGhcVersions,
                                                  testSessionWithExtraFiles)

tests :: TestTree
tests =
    testGroup "gotoDefinition for dependencies"
        [ dependencyTermTest
        , dependencyTypeTest
        , transitiveDependencyTest
        , autogenDependencyTest
        , bootDependencyTest
        , whereClauseDependencyTest
        ]

fileDoneIndexing :: [String] -> Session FilePath
fileDoneIndexing fpSuffix =
    skipManyTill anyMessage indexedFile
    where
        indexedFile :: Session FilePath
        indexedFile = do
            NotMess TNotificationMessage{_params} <-
                customNotification (Proxy @"ghcide/reference/ready")
            case A.fromJSON _params of
                A.Success fp -> do
                    let fpDirs :: [String]
                        fpDirs = splitDirectories fp
                    bool Applicative.empty (pure fp) $
                        fpSuffix `isSuffixOf` fpDirs
                other -> error $ "Failed to parse ghcide/reference/ready file: " <> show other

waitForDiagnosticsOrDoneIndexing :: Session [Diagnostic]
waitForDiagnosticsOrDoneIndexing =
    skipManyTill anyMessage (diagnosticsMessage Applicative.<|> doneIndexing)
    where
        diagnosticsMessage :: Session [Diagnostic]
        diagnosticsMessage = do
            diagnosticsNotification <- message SMethod_TextDocumentPublishDiagnostics
            let diagnosticss = diagnosticsNotification ^. L.params . L.diagnostics
            return diagnosticss
        doneIndexing :: Session [Diagnostic]
        doneIndexing = satisfyMaybe $ \case
            FromServerMess SMethod_Progress (TNotificationMessage _ _ (ProgressParams t (preview _workDoneProgressEnd -> Just params))) ->
                case params of
                    (WorkDoneProgressEnd _ m) ->
                        case m of
                            Just message -> bool Nothing (Just []) $
                                "Finished indexing" `isPrefixOf` message
                            _ -> Nothing
                    _ -> Nothing
            _ -> Nothing

-- | Tests that we can go to the definition of a term in a dependency.
-- In this case, we are getting the definition of the data
-- constructor AsyncCancelled.
dependencyTermTest :: TestTree
dependencyTermTest = testSessionWithExtraFiles "dependency" "gotoDefinition term in async" $
    \dir -> do
        doc <- openDoc (dir </> "Dependency" <.> "hs") "haskell"
        _hieFile <- fileDoneIndexing ["Control", "Concurrent", "Async.hie"]
        defs <- getDefinitions doc (Position 5 20)
        let expRange = Range (Position 430 22) (Position 430 36)
        diagnostics <- waitForDiagnosticsOrDoneIndexing
        case defs of
            InL (Definition (InR [Location fp actualRange])) ->
                liftIO $ do
                    let locationDirectories :: [String]
                        locationDirectories =
                            maybe [] splitDirectories $
                                uriToFilePath fp
                    assertBool "AsyncCancelled found in a module that is not Control.Concurrent Async"
                        $ ["Control", "Concurrent", "Async.hs"]
                            `isSuffixOf` locationDirectories
                    diagnostics @?= []
                    actualRange @?= expRange
            wrongLocation ->
                liftIO $
                    assertFailure $ "Wrong location for AsyncCancelled: "
                        ++ show wrongLocation

-- | Tests that we can go to the definition of a type in a dependency.
-- In this case, we are getting the definition of the type AsyncCancelled.
dependencyTypeTest :: TestTree
dependencyTypeTest = testSessionWithExtraFiles "dependency" "gotoDefinition type in async" $
    \dir -> do
        doc <- openDoc (dir </> "Dependency" <.> "hs") "haskell"
        _hieFile <- fileDoneIndexing ["Control", "Concurrent", "Async.hie"]
        defs <- getDefinitions doc (Position 4 21)
        let expRange = Range (Position 430 0) (Position 435 5)
        case defs of
            InL (Definition (InR [Location fp actualRange])) ->
                liftIO $ do
                    let locationDirectories :: [String]
                        locationDirectories =
                            maybe [] splitDirectories $
                                uriToFilePath fp
                    assertBool "AsyncCancelled found in a module that is not Control.Concurrent Async"
                        $ ["Control", "Concurrent", "Async.hs"]
                            `isSuffixOf` locationDirectories
                    actualRange @?= expRange
            wrongLocation ->
                liftIO $
                    assertFailure $ "Wrong location for AsyncCancelled: "
                        ++ show wrongLocation

-- | Tests that we can go to the definition of a dependency, and then
-- from the dependency file we can use gotoDefinition to see a
-- tranisive dependency.
transitiveDependencyTest :: TestTree
transitiveDependencyTest = testSessionWithExtraFiles "dependency" "goto transitive dependency async -> hashable" $
    \dir -> do
        localDoc <- openDoc (dir </> "Dependency" <.> "hs") "haskell"
        _asyncHieFile <- fileDoneIndexing ["Control", "Concurrent", "Async.hie"]
        _hashableHieFile <- fileDoneIndexing ["Data", "Hashable", "Class.hie"]
        asyncDefs <- getDefinitions localDoc (Position 5 20)
        asyncHsFile <- case asyncDefs of
            InL (Definition (InR [Location uri _actualRange])) ->
                liftIO $ do
                    let fp :: FilePath
                        fp = fromMaybe "" $ uriToFilePath uri
                        locationDirectories :: [String]
                        locationDirectories = splitDirectories fp
                    assertBool "AsyncCancelled found in a module that is not Control.Concurrent Async"
                        $ ["Control", "Concurrent", "Async.hs"]
                            `isSuffixOf` locationDirectories
                    pure fp
            wrongLocation ->
                liftIO $
                    assertFailure $ "Wrong location for AsyncCancelled: "
                        ++ show wrongLocation
        asyncDoc <- openDoc asyncHsFile "haskell"
        hashableDefs <- getDefinitions asyncDoc (Position 246 11)
        -- The location of the definition of Hashable in
        -- Data.Hashable.Class
        let expRange = Range (Position 198 14) (Position 198 22)
        case hashableDefs of
            InL (Definition (InR [Location uri actualRange])) ->
                liftIO $ do
                    let locationDirectories :: [String]
                        locationDirectories =
                            maybe [] splitDirectories $
                                uriToFilePath uri
                    assertBool "Hashable found in a module that is not Data.Hashable.Class"
                        $ ["Data", "Hashable", "Class.hs"]
                            `isSuffixOf` locationDirectories
                    actualRange @?= expRange
            wrongLocation ->
                liftIO $
                    assertFailure $ "Wrong location for Hashable: "
                        ++ show wrongLocation

-- | Testing that we can go to a definition in an autogen module of a
-- dependency. We use the repository https://github.com/nlander/minimal-autogen.git
-- as the dependency. It is a minimal package with an autogen module,
-- allowing us to avoid building a larger dependency in CI just for
-- this test.
autogenDependencyTest :: TestTree
autogenDependencyTest = testSessionWithExtraFiles "dependency-autogen" "goto autogen module in dependency" $
        \dir -> do
            localDoc <- openDoc (dir </> "Dependency" <.> "hs") "haskell"
            _hieFile <- fileDoneIndexing ["Paths_minimal_autogen.hie"]
            defs <- getDefinitions localDoc (Position 6 5)
            -- The location of the definition of version in
            -- Paths_minimal_autogen
            let expRange = Range (Position 35 0) (Position 35 7)
            case defs of
                InL (Definition (InR [Location uri actualRange])) ->
                    liftIO $ do
                        let locationDirectories :: [String]
                            locationDirectories =
                                maybe [] splitDirectories $
                                    uriToFilePath uri
                        assertBool "version found in a module that is not Paths_minimal_autogen"
                            $ ["Paths_minimal_autogen.hs"]
                                `isSuffixOf` locationDirectories
                        actualRange @?= expRange
                wrongLocation ->
                    liftIO $
                        assertFailure $ "Wrong location for version: "
                            ++ show wrongLocation

-- | Tests that we can go to a definition in a boot library, that is,
-- one of the libraries that ships with GHC. In this case we are
-- going to a definition in containers. This does not currently work
-- for available GHC versions but hopefully will for later versions
-- of GHC.
bootDependencyTest :: TestTree
bootDependencyTest = knownBrokenForGhcVersions [GHC810, GHC90, GHC92, GHC94, GHC96] "HIE files are not generated by older GHCs" $
    testSessionWithExtraFiles "dependency-boot" "gotoDefinition term in boot library containers" $
        \dir -> do
            doc <- openDoc (dir </> "Dependency" <.> "hs") "haskell"
            _hieFile <- fileDoneIndexing ["Data", "Set", "Internal.hie"]
            defs <- getDefinitions doc (Position 5 20)
            -- The location of the definition of empty in Data.Set.Internal.
            -- This will likely need to be updated when there is a GHC for
            -- which this test can pass.
            let expRange = Range (Position 513 0) (Position 513 11)
            case defs of
                InL (Definition (InR [Location fp actualRange])) ->
                    liftIO $ do
                        let locationDirectories :: [String]
                            locationDirectories =
                                maybe [] splitDirectories $
                                    uriToFilePath fp
                        assertBool "empty found in a module that is not Data.Set.Internal"
                            $ ["Data", "Set", "Internal.hs"]
                                `isSuffixOf` locationDirectories
                        actualRange @?= expRange
                wrongLocation ->
                    liftIO $
                        assertFailure $ "Wrong location for empty: "
                            ++ show wrongLocation

-- | Testing that we can go to a definition in a where clause in a dependency.
-- This currently fails, but it is unclear why.
whereClauseDependencyTest :: TestTree
whereClauseDependencyTest = expectFailBecause "TODO: figure out why where clauses in dependencies are not indexed" $
    testSessionWithExtraFiles "dependency-where" "goto where clause definition in dependency" $
        \dir -> do
            localDoc <- openDoc (dir </> "Dependency" <.> "hs") "haskell"
            _hieFile <- fileDoneIndexing ["Data", "Scientific.hie"]
            scientificDefs <- getDefinitions localDoc (Position 5 5)
            scientificFile <- case scientificDefs of
                InL (Definition (InR [Location uri _actualRange])) ->
                    liftIO $ do
                        let fp :: FilePath
                            fp = fromMaybe "" $ uriToFilePath uri
                            locationDirectories :: [String]
                            locationDirectories = splitDirectories fp
                        assertBool "base10Exponent found in a module that is not Data.Scientific"
                            $ ["Data", "Scientific.hs"]
                                `isSuffixOf` locationDirectories
                        pure fp
                wrongLocation ->
                    liftIO $
                        assertFailure $ "Wrong location for base10Exponent: "
                            ++ show wrongLocation
            scientificDoc <- openDoc scientificFile "haskell"
            -- Where longDiv is referenced in the function body
            -- of unsafeFromRational in Data.Scientific
            longDivDefs <- getDefinitions scientificDoc (Position 367 33)
            -- The location of the definition of longDiv in
            -- the where clause of unsafeFromRational
            let expRange = Range (Position 371 4) (Position 376 55)
            case longDivDefs of
                InL (Definition (InR [Location uri actualRange])) ->
                    liftIO $ do
                        let locationDirectories :: [String]
                            locationDirectories =
                                maybe [] splitDirectories $
                                    uriToFilePath uri
                        assertBool "longDiv found in a module that is not Data.Scientific"
                            $ ["Data", "Scientific.hs"]
                                `isSuffixOf` locationDirectories
                        actualRange @?= expRange
                wrongLocation ->
                    liftIO $
                        assertFailure $ "Wrong location for longDiv: "
                            ++ show wrongLocation
