{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Config (tests) where

import           Control.DeepSeq
import           Control.Lens            hiding (List, (.=))
import           Control.Monad
import           Data.Aeson
import           Data.Hashable
import qualified Data.HashMap.Strict     as HM
import qualified Data.Map                as Map
import qualified Data.Text               as T
import           Data.Typeable           (Typeable)
import           Development.IDE         (RuleResult, action, define,
                                          getFilesOfInterestUntracked,
                                          getPluginConfigAction, ideErrorText,
                                          uses_)
import           Development.IDE.Test    (expectDiagnostics)
import           GHC.Generics
import           Ide.Plugin.Config
import           Ide.Types
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
      configParsingTests, genericConfigTests
    ]

configParsingTests :: TestTree
configParsingTests = testGroup "config parsing"
    [ testCase "empty object as user configuration should not send error logMessage" $ runConfigSession "" $ do
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

genericConfigTests :: TestTree
genericConfigTests = testGroup "generic plugin config"
    [
        testCase "custom defaults" $ runConfigSession "diagnostics" $ do
            _doc <- createDoc "Foo.hs" "haskell" "module Foo where\nfoo = False"
            -- getting only the standard diagnostics means the plugin wasn't enabled
            expectDiagnostics standardDiagnostics
   ,    testCase "custom defaults and user config on some other plugin" $ runConfigSession "diagnostics" $ do
            _doc <- createDoc "Foo.hs" "haskell" "module Foo where\nfoo = False"
            -- test that the user config doesn't accidentally override the initial config
            sendConfigurationChanged $ toJSON (changeConfig "someplugin" def{plcHoverOn = False})
            -- getting only the expected diagnostics means the plugin wasn't enabled
            expectDiagnostics standardDiagnostics
    ,   expectFailBecause "partial config is not supported" $
        testCase "custom defaults and non overlapping user config" $ runConfigSession "diagnostics" $ do
            _doc <- createDoc "Foo.hs" "haskell" "module Foo where\nfoo = False"
            -- test that the user config doesn't accidentally override the initial config
            sendConfigurationChanged $ toJSON (changeConfig testPluginId def{plcHoverOn = False})
            -- getting only the expected diagnostics means the plugin wasn't enabled
            expectDiagnostics standardDiagnostics
    ,   testCase "custom defaults and overlapping user plugin config" $ runConfigSession "diagnostics" $ do
            _doc <- createDoc "Foo.hs" "haskell" "module Foo where\nfoo = False"
            -- test that the user config overrides the default initial config
            sendConfigurationChanged $ toJSON (changeConfig testPluginId def{plcGlobalOn = True})
            -- getting only the expected diagnostics means the plugin wasn't enabled
            expectDiagnostics testPluginDiagnostics
    ,   testCase "custom defaults and non plugin user config" $ runConfigSession "diagnostics" $ do
            _doc <- createDoc "Foo.hs" "haskell" "module Foo where\nfoo = False"
            -- test that the user config doesn't accidentally override the initial config
            sendConfigurationChanged $ toJSON (def {formattingProvider = "foo"})
            -- getting only the expected diagnostics means the plugin wasn't enabled
            expectDiagnostics standardDiagnostics
    ]
    where
        standardDiagnostics = [("Foo.hs", [(DsWarning, (1,0), "Top-level binding")])]
        testPluginDiagnostics = [("Foo.hs", [(DsError, (0,0), "testplugin")])]

        runConfigSession subdir =
            failIfSessionTimeout . runSessionWithServer @() (const plugin) ("test/testdata" </> subdir)

        testPluginId = "testplugin"
        -- A disabled-by-default plugin that creates diagnostics
        plugin = (defaultPluginDescriptor testPluginId)
          {
            pluginConfigDescriptor = configDisabled
          , pluginRules = do
              action $ do
                plc <- getPluginConfigAction testPluginId
                when (plcGlobalOn plc && plcDiagnosticsOn plc) $ do
                    files <- getFilesOfInterestUntracked
                    void $ uses_ GetTestDiagnostics $ HM.keys files
              define mempty $ \GetTestDiagnostics file -> do
                let diags = [ideErrorText file "testplugin"]
                return (diags,Nothing)
          }
        -- A config that disables the plugin initially
        configDisabled = defaultConfigDescriptor{
            configInitialGenericConfig = def{plcGlobalOn = False, plcDiagnosticsOn = False}
        }
        changeConfig :: PluginId -> PluginConfig -> Config
        changeConfig plugin conf =
            def{plugins = Map.fromList [(plugin, conf)]}


data GetTestDiagnostics = GetTestDiagnostics
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetTestDiagnostics
instance NFData   GetTestDiagnostics
type instance RuleResult GetTestDiagnostics = ()
