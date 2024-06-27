{-# LANGUAGE GADTs #-}
module ClientSettingsTests (tests) where

import           Config                          (testWithDummyPluginEmpty)
import           Control.Applicative.Combinators
import           Control.Monad
import           Data.Aeson                      (toJSON)
import           Data.Default
import qualified Data.Text                       as T
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types     hiding
                                                 (SemanticTokenAbsolute (..),
                                                  SemanticTokenRelative (..),
                                                  SemanticTokensEdit (..),
                                                  mkRange)
import           Language.LSP.Test
import           Test.Hls                        (waitForProgressDone)
import           Test.Tasty

tests :: TestTree
tests = testGroup "client settings handling"
    [ testWithDummyPluginEmpty "ghcide restarts shake session on config changes" $ do
            setIgnoringLogNotifications False
            void $ createDoc "A.hs" "haskell" "module A where"
            waitForProgressDone
            setConfigSection "haskell" $ toJSON (def :: Config)
            skipManyTill anyMessage restartingBuildSession

    ]
  where
    restartingBuildSession :: Session ()
    restartingBuildSession = do
        FromServerMess SMethod_WindowLogMessage TNotificationMessage{_params = LogMessageParams{..}} <- loggingNotification
        guard $ "Restarting build session" `T.isInfixOf` _message
