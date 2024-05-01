{-# LANGUAGE DataKinds #-}

module AsyncTests (tests) where

import           Control.Monad
import           Control.Monad.IO.Class        (liftIO)
import           Data.Aeson                    (toJSON)
import           Data.Proxy
import qualified Data.Text                     as T
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types   hiding
                                               (SemanticTokenAbsolute (..),
                                                SemanticTokenRelative (..),
                                                SemanticTokensEdit (..),
                                                mkRange)
import           Language.LSP.Test
-- import Test.QuickCheck.Instances ()
import           Config
import           Development.IDE.Plugin.Test   (TestRequest (BlockSeconds),
                                                blockCommandId)
import           Test.Tasty
import           Test.Tasty.HUnit

-- | Test if ghcide asynchronously handles Commands and user Requests
tests :: TestTree
tests = testGroup "async"
    [
      testWithDummyPluginEmpty "command" $ do
            -- Execute a command that will block forever
            let req = ExecuteCommandParams Nothing blockCommandId Nothing
            void $ sendRequest SMethod_WorkspaceExecuteCommand req
            -- Load a file and check for code actions. Will only work if the command is run asynchronously
            doc <- createDoc "A.hs" "haskell" $ T.unlines
              [ "{-# OPTIONS -Wmissing-signatures #-}"
              , "foo = id"
              ]
            void waitForDiagnostics
            codeLenses <- getAndResolveCodeLenses doc
            liftIO $ [ _title | CodeLens{_command = Just Command{_title}} <- codeLenses] @=?
              [ "foo :: a -> a" ]
    , testWithDummyPluginEmpty "request" $ do
            -- Execute a custom request that will block for 1000 seconds
            void $ sendRequest (SMethod_CustomMethod (Proxy @"test")) $ toJSON $ BlockSeconds 1000
            -- Load a file and check for code actions. Will only work if the request is run asynchronously
            doc <- createDoc "A.hs" "haskell" $ T.unlines
              [ "{-# OPTIONS -Wmissing-signatures #-}"
              , "foo = id"
              ]
            void waitForDiagnostics
            codeLenses <- getAndResolveCodeLenses doc
            liftIO $ [ _title | CodeLens{_command = Just Command{_title}} <- codeLenses] @=?
              [ "foo :: a -> a" ]
    ]
