{-# LANGUAGE OverloadedStrings #-}

module Config (tests) where

import           Control.Lens hiding (List)
import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Default
import qualified Data.Map as Map
import qualified Data.Text as T
import           Ide.Plugin.Config
import           Language.Haskell.LSP.Test as Test
import           Language.Haskell.LSP.Types
import qualified Language.Haskell.LSP.Types.Lens as L
import           System.FilePath ((</>))
import           Test.Hls.Util
import           Test.Tasty
import           Test.Tasty.HUnit

{-# ANN module ("HLint: ignore Reduce duplication"::String) #-}

tests :: TestTree
tests = testGroup "plugin config" [
      -- Note: because the flag is treated generically in the plugin handler, we
      -- do not have to test each individual plugin
      hlintTests
    ]

hlintTests :: TestTree
hlintTests = testGroup "hlint plugin enables" [

      testCase "changing hlintOn configuration enables or disables hlint diagnostics" $ runHlintSession "" $ do
        let config = def { hlintOn = True }
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

        doc <- openDoc "ApplyRefact2.hs" "haskell"
        testHlintDiagnostics doc

        let config' = def { hlintOn = False }
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config'))

        diags' <- waitForDiagnosticsFrom doc

        liftIO $ noHlintDiagnostics diags'

    , testCase "changing hlint plugin configuration enables or disables hlint diagnostics" $ runHlintSession "" $ do
        let config = def { hlintOn = True }
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config))

        doc <- openDoc "ApplyRefact2.hs" "haskell"
        testHlintDiagnostics doc

        let config' = pluginGlobalOn config "hlint" False
        sendNotification WorkspaceDidChangeConfiguration (DidChangeConfigurationParams (toJSON config'))

        diags' <- waitForDiagnosticsFrom doc

        liftIO $ noHlintDiagnostics diags'

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

pluginGlobalOn :: Config -> T.Text -> Bool -> Config
pluginGlobalOn config pid state = config'
  where
      pluginConfig = def { plcGlobalOn = state }
      config' = def { plugins = Map.insert pid pluginConfig (plugins config) }
