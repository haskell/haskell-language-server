{-# LANGUAGE OverloadedStrings #-}

module Config (tests) where

import           Control.Lens            hiding (List, (.=))
import           Control.Monad
import           Data.Aeson
import qualified Data.Text               as T
import           Language.LSP.Test       as Test
import qualified Language.LSP.Types.Lens as L
import           System.FilePath         ((</>))
import           Test.Hls
import           Test.Hls.Command

{-# ANN module ("HLint: ignore Reduce duplication"::String) #-}

tests :: TestTree
tests = testGroup "plugin config" [
      -- Note: there are more comprehensive tests over config in hls-hlint-plugin
      -- TODO: Add generic tests over some example plugin
      configTests
    ]

configTests :: TestTree
configTests = testGroup "config parsing" [
      testCase "empty object as user configuration should not send error logMessage" $ runConfigSession "" $ do
        let config = object []
        sendConfigurationChanged (toJSON config)

        -- Send custom request so server returns a response to prevent blocking
        void $ sendNotification (SCustomMethod "non-existent-method") Null

        logNot <- skipManyTill Test.anyMessage (message SWindowLogMessage)

        liftIO $ (logNot ^. L.params . L.xtype) > MtError
                 || "non-existent-method" `T.isInfixOf` (logNot ^. L.params . L.message)
                    @? "Server sends logMessage with MessageType = Error"
    ]
    where
        runConfigSession :: FilePath -> Session a -> IO a
        runConfigSession subdir  =
            failIfSessionTimeout . runSession hlsCommand fullCaps ("test/testdata" </> subdir)
