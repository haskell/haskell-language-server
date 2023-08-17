{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
module Dependency where

import qualified Control.Applicative             as Applicative
import           Control.Applicative.Combinators (skipManyTill)
import           Control.Monad.IO.Class          (liftIO)
import qualified Data.Aeson                      as A
import           Data.Bool                       (bool)
import           Data.List                       (isSuffixOf)
import           Data.Maybe                      (fromMaybe)
import           Data.Proxy                      (Proxy (..))
import           Development.IDE.GHC.Compat      (GhcVersion (..))
import           Language.LSP.Protocol.Message   (TCustomMessage (NotMess),
                                                  TNotificationMessage (..))
import           Language.LSP.Protocol.Types     (Definition (..),
                                                  Location (..), Position (..),
                                                  Range (..),
                                                  type (|?) (InL, InR),
                                                  uriToFilePath)
import           Language.LSP.Test               (Session, anyMessage,
                                                  customNotification,
                                                  getDefinitions, openDoc)
import           System.FilePath                 (splitDirectories, (<.>),
                                                  (</>))
import           Test.Tasty                      (TestTree, testGroup)
import           Test.Tasty.HUnit                (assertBool, assertFailure,
                                                  (@?=))
import           TestUtils                       (testSessionWithExtraFiles, knownBrokenForGhcVersions)

tests :: TestTree
tests =
    testGroup "gotoDefinition for dependencies"
        [ dependencyTest
        , transitiveDependencyTest
        , autogenDependencyTest
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

dependencyTest :: TestTree
dependencyTest = testSessionWithExtraFiles "dependency" "gotoDefinition in async" $
    \dir -> do
        doc <- openDoc (dir </> "Dependency" <.> "hs") "haskell"
        _hieFile <- fileDoneIndexing ["Control", "Concurrent", "Async.hie"]
        defs <- getDefinitions doc (Position 5 20)
        let expRange = Range (Position 430 22) (Position 430 36)
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

-- Tests that we can go to the definition of a dependency, and then
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

-- Testing that we can go to a definition in an autogen module of a
-- dependency. Stylish haskell is a package that has an autogen module,
-- but it doesn't seem to build with ghc 9.0 or earlier. Suggestions on
-- another package we could use for this test are welcome! This test
-- doesn't go directly to the fuction in the autogen module because
-- it is a hidden module, so we can't import that function directly
-- in our project. However, hidden modules are also indexed, so we
-- can go to a definition in a module that imports the autogen module
-- and goto the autogen module from there.
autogenDependencyTest :: TestTree
autogenDependencyTest = knownBrokenForGhcVersions [GHC810, GHC90] "stylish-haskell does not build with older GHC versions" $
    testSessionWithExtraFiles "dependency-autogen" "goto autogen module in dependency" $
        \dir -> do
            localDoc <- openDoc (dir </> "Dependency" <.> "hs") "haskell"
            _hieFile <- fileDoneIndexing ["Paths_stylish_haskell.hie"]
            stylishDefs <- getDefinitions localDoc (Position 5 5)
            stylishFile <- case stylishDefs of
                InL (Definition (InR [Location uri _actualRange])) ->
                    liftIO $ do
                        let fp :: FilePath
                            fp = fromMaybe "" $ uriToFilePath uri
                            locationDirectories :: [String]
                            locationDirectories = splitDirectories fp
                        assertBool "tags found in a module that is not Language.Haskell.Stylish"
                            $ ["Language", "Haskell", "Stylish.hs"]
                                `isSuffixOf` locationDirectories
                        pure fp
                wrongLocation ->
                    liftIO $
                        assertFailure $ "Wrong location for AsyncCancelled: "
                            ++ show wrongLocation
            stylishDoc <- openDoc stylishFile "haskell"
            pathsDefs <- getDefinitions stylishDoc (Position 19 8)
            -- The location of the definition of version in
            -- Paths_stylish_haskell
            let expRange = Range (Position 35 0) (Position 35 7)
            case pathsDefs of
                InL (Definition (InR [Location uri actualRange])) ->
                    liftIO $ do
                        let locationDirectories :: [String]
                            locationDirectories =
                                maybe [] splitDirectories $
                                    uriToFilePath uri
                        assertBool "version found in a module that is not Paths_stylish_haskell"
                            $ ["Paths_stylish_haskell.hs"]
                                `isSuffixOf` locationDirectories
                        actualRange @?= expRange
                wrongLocation ->
                    liftIO $
                        assertFailure $ "Wrong location for version: "
                            ++ show wrongLocation
