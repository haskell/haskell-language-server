{-# LANGUAGE GADTs #-}
module ClientSettingsTests (tests) where

import           Control.Applicative.Combinators
import           Control.Monad
import           Data.Aeson                      (toJSON)
import qualified Data.Aeson                      as A
import qualified Data.Text                       as T
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types     hiding
                                                 (SemanticTokenAbsolute (..),
                                                  SemanticTokenRelative (..),
                                                  SemanticTokensEdit (..),
                                                  mkRange)
import           Language.LSP.Test
import           Test.Tasty
import           TestUtils

tests :: TestTree
tests = testGroup "client settings handling"
    [ testSession "ghcide restarts shake session on config changes" $ do
            void $ skipManyTill anyMessage $ message SMethod_ClientRegisterCapability
            void $ createDoc "A.hs" "haskell" "module A where"
            waitForProgressDone
            sendNotification SMethod_WorkspaceDidChangeConfiguration
                (DidChangeConfigurationParams (toJSON (mempty :: A.Object)))
            skipManyTill anyMessage restartingBuildSession

    ]
  where
    restartingBuildSession :: Session ()
    restartingBuildSession = do
        FromServerMess SMethod_WindowLogMessage TNotificationMessage{_params = LogMessageParams{..}} <- loggingNotification
        guard $ "Restarting build session" `T.isInfixOf` _message
