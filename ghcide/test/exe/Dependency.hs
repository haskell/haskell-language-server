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
import           Data.Proxy                      (Proxy (..))
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
import           TestUtils                       (testSessionWithExtraFiles)

tests :: TestTree
tests =
    testGroup "gotoDefinition for dependencies"
        [ dependencyTest
        ]
    where
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
