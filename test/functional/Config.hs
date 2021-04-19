{-# LANGUAGE OverloadedStrings #-}

module Config (tests) where

import           Control.Lens            hiding (List, (.=))
import           Control.Monad
import           Data.Aeson
import qualified Data.Map                as Map
import qualified Data.Text               as T
import           Ide.Plugin.Config
import qualified Ide.Plugin.Config       as Plugin
import           Language.LSP.Test       as Test
import qualified Language.LSP.Types.Lens as L
import           System.FilePath         ((</>))
import           Test.Hls
import           Test.Hls.Command

{-# ANN module ("HLint: ignore Reduce duplication"::String) #-}

tests :: TestTree
tests = testGroup "plugin config" [
      -- Note: because the flag is treated generically in the plugin handler, we
      -- do not have to test each individual plugin
      hlintTests
    , configTests
    ]

hlintTests :: TestTree
hlintTests = testGroup "hlint plugin enables" [

      testCase "changing hlintOn configuration enables or disables hlint diagnostics" $ runHlintSession "" $ do
        let config = def { hlintOn = True }
        sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

        doc <- openDoc "ApplyRefact2.hs" "haskell"
        testHlintDiagnostics doc

        let config' = def { hlintOn = False }
        sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config'))

        diags' <- waitForDiagnosticsFrom doc

        liftIO $ noHlintDiagnostics diags'

    , testCase "changing hlint plugin configuration enables or disables hlint diagnostics" $ runHlintSession "" $ do
        let config = def { hlintOn = True }
        sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

        doc <- openDoc "ApplyRefact2.hs" "haskell"
        testHlintDiagnostics doc

        let config' = pluginGlobalOn config "hlint" False
        sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config'))

        diags' <- waitForDiagnosticsFrom doc

        liftIO $ noHlintDiagnostics diags'

    , testCase "adding hlint flags to plugin configuration removes hlint diagnostics" $ runHlintSession "" $ do
        let config = def { hlintOn = True }
        sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

        doc <- openDoc "ApplyRefact2.hs" "haskell"
        testHlintDiagnostics doc

        let config' = hlintConfigWithFlags ["--ignore=Redundant id", "--hint=test-hlint-config.yaml"]
        sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config'))

        diags' <- waitForDiagnosticsFrom doc

        liftIO $ noHlintDiagnostics diags'

    , testCase "adding hlint flags to plugin configuration adds hlint diagnostics" $ runHlintSession "" $ do
        let config = def { hlintOn = True }
        sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

        doc <- openDoc "ApplyRefact7.hs" "haskell"

        expectNoMoreDiagnostics 3 doc "hlint"

        let config' = hlintConfigWithFlags ["--with-group=generalise"]
        sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config'))

        diags' <- waitForDiagnosticsFromSource doc "hlint"
        d <- liftIO $ inspectDiagnostic diags' ["Use <>"]

        liftIO $ do
            length diags' @?= 1
            d ^. L.range @?= Range (Position 1 10) (Position 1 21)
            d ^. L.severity @?= Just DsInfo
    ]
    where
        runHlintSession :: FilePath -> Session a -> IO a
        runHlintSession subdir  =
            failIfSessionTimeout . runSession hlsCommand fullCaps ("test/testdata/hlint" </> subdir)

        noHlintDiagnostics :: [Diagnostic] -> Assertion
        noHlintDiagnostics diags =
            Just "hlint" `notElem` map (^. L.source) diags @? "There are no hlint diagnostics"

        testHlintDiagnostics doc = do
            diags <- waitForDiagnosticsFromSource doc "hlint"
            liftIO $ length diags > 0 @? "There are hlint diagnostics"

configTests :: TestTree
configTests = testGroup "config parsing" [
      testCase "empty object as user configuration should not send error logMessage" $ runConfigSession "" $ do
        let config = object []
        sendNotification SWorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

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

pluginGlobalOn :: Config -> T.Text -> Bool -> Config
pluginGlobalOn config pid state = config'
  where
      pluginConfig = def { plcGlobalOn = state }
      config' = def { plugins = Map.insert pid pluginConfig (plugins config) }

hlintConfigWithFlags :: [T.Text] -> Config
hlintConfigWithFlags flags =
  def
    { hlintOn = True
    , Plugin.plugins = Map.fromList [("hlint",
        def { Plugin.plcConfig = unObject $ object ["flags" .= flags] }
    )] }
  where
    unObject (Object obj) = obj
    unObject _            = undefined
