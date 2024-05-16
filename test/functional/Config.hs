{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
module Config (tests) where

import           Control.DeepSeq
import           Control.Monad
import           Data.Hashable
import qualified Data.HashMap.Strict  as HM
import qualified Data.Map             as Map
import           Data.Typeable        (Typeable)
import           Development.IDE      (RuleResult, action, define,
                                       getFilesOfInterestUntracked,
                                       getPluginConfigAction, ideErrorText,
                                       uses_)
import           Development.IDE.Test (expectDiagnostics)
import           GHC.Generics
import           Ide.Plugin.Config
import           Ide.Types
import           Language.LSP.Test    as Test
import           System.FilePath      ((</>))
import           Test.Hls

{-# ANN module ("HLint: ignore Reduce duplication"::String) #-}

tests :: TestTree
tests = testGroup "plugin config" [
      -- Note: there are more comprehensive tests over config in hls-hlint-plugin
      -- TODO: Add generic tests over some example plugin
       genericConfigTests
    ]

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
            setHlsConfig $ changeConfig "someplugin" def{plcHoverOn = False}
            -- getting only the expected diagnostics means the plugin wasn't enabled
            expectDiagnostics standardDiagnostics
    ,   expectFailBecause "partial config is not supported" $
        testCase "custom defaults and non overlapping user config" $ runConfigSession "diagnostics" $ do
            _doc <- createDoc "Foo.hs" "haskell" "module Foo where\nfoo = False"
            -- test that the user config doesn't accidentally override the initial config
            setHlsConfig $ changeConfig testPluginId def{plcHoverOn = False}
            -- getting only the expected diagnostics means the plugin wasn't enabled
            expectDiagnostics standardDiagnostics
    ,   testCase "custom defaults and overlapping user plugin config" $ runConfigSession "diagnostics" $ do
            _doc <- createDoc "Foo.hs" "haskell" "module Foo where\nfoo = False"
            -- test that the user config overrides the default initial config
            setHlsConfig $ changeConfig testPluginId def{plcGlobalOn = True}
            -- getting only the expected diagnostics means the plugin wasn't enabled
            expectDiagnostics testPluginDiagnostics
    ,   testCase "custom defaults and non plugin user config" $ runConfigSession "diagnostics" $ do
            _doc <- createDoc "Foo.hs" "haskell" "module Foo where\nfoo = False"
            -- test that the user config doesn't accidentally override the initial config
            setHlsConfig $ def {formattingProvider = "foo"}
            -- getting only the expected diagnostics means the plugin wasn't enabled
            expectDiagnostics standardDiagnostics
    ]
    where
        standardDiagnostics = [("Foo.hs", [(DiagnosticSeverity_Warning, (1,0), "Top-level binding")])]
        testPluginDiagnostics = [("Foo.hs", [(DiagnosticSeverity_Error, (0,0), "testplugin")])]

        runConfigSession subdir session = do
          failIfSessionTimeout $
            runSessionWithTestConfig def
                {testConfigSession=def {ignoreConfigurationRequests=False}, testShiftRoot=True
                , testPluginDescriptor=plugin, testDirLocation=Left ("test/testdata" </> subdir)} (const session)

        testPluginId = "testplugin"
        -- A disabled-by-default plugin that creates diagnostics
        plugin = mkPluginTestDescriptor' @() pd testPluginId
        pd plId = (defaultPluginDescriptor plId "")
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
            def{plugins = Map.insert plugin conf (plugins def)}


data GetTestDiagnostics = GetTestDiagnostics
    deriving (Eq, Show, Typeable, Generic)
instance Hashable GetTestDiagnostics
instance NFData   GetTestDiagnostics
type instance RuleResult GetTestDiagnostics = ()
