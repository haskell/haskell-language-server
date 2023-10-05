{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module FunctionalCodeAction (tests) where

import           Control.Lens               hiding (List)
import qualified Data.Text                  as T
import           Ide.Plugin.Config
import qualified Language.LSP.Protocol.Lens as L
import           Language.LSP.Test          as Test
import           Test.Hls
import           Test.Hls.Command

{-# ANN module ("HLint: ignore Reduce duplication"::String) #-}

tests :: TestTree
tests = testGroup "code actions"
#if hls_refactor
    [ ignoreInEnv [HostOS Windows, GhcVer GHC94] "Diagnostic failure for Windows-ghc9.4.2" importQualifiedTests
    , ignoreInEnv [HostOS Windows, GhcVer GHC94] "Diagnostic failure for Windows-ghc9.4.2" importQualifiedPostTests
#endif
    ]


-- TODO: move these tests to hls-refactor-plugin
importQualifiedTests :: TestTree
importQualifiedTests = testGroup "import qualified prefix suggestions" [
    testCase "qualified import works with 3.8 code action kinds" $ runSessionWithConfig (def {lspConfig = hlsConfigToClientConfig testConfig}) hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "CodeActionImportQualified.hs" "haskell"
        (diag:_) <- waitForDiagnosticsFrom doc
        liftIO $ diag ^. L.message @?=
           if ghcVersion >= GHC96
           then "Variable not in scope: Control.when :: Bool -> IO () -> IO ()\nNB: no module named ‘Control’ is imported."
           else "Not in scope: ‘Control.when’\nNo module named ‘Control’ is imported."

        actionsOrCommands <- getAllCodeActions doc
        let actns = map fromAction actionsOrCommands

        let importQualifiedSuggestion = "import qualified Control.Monad as Control"
        importControlMonadQualified <- liftIO $ inspectCodeAction actionsOrCommands [importQualifiedSuggestion]
        liftIO $ do
            dontExpectCodeAction actionsOrCommands ["import Control.Monad (when)"]
            length actns >= 5 @? "There are some actions"

        executeCodeAction importControlMonadQualified

        contents <- documentContents doc
        liftIO $ contents @?= "import qualified Control.Monad as Control\nmain :: IO ()\nmain = Control.when True $ putStrLn \"hello\"\n"
    ]

-- TODO: move these tests to ghcide, or the plugin that provides this code action.
importQualifiedPostTests :: TestTree
importQualifiedPostTests = testGroup "import qualified postfix suggestions" [
    testCase "qualified import in postfix position works with 3.8 code action kinds" $ runSessionWithConfig (def {lspConfig = hlsConfigToClientConfig testConfig}) hlsCommand fullCaps "test/testdata" $ do
        doc <- openDoc "CodeActionImportPostQualified.hs" "haskell"
        (diag:_) <- waitForDiagnosticsFrom doc
        liftIO $ diag ^. L.message @?=
           if ghcVersion >= GHC96
           then "Variable not in scope: Control.when :: Bool -> IO () -> IO ()\nNB: no module named ‘Control’ is imported."
           else "Not in scope: ‘Control.when’\nNo module named ‘Control’ is imported."

        actionsOrCommands <- getAllCodeActions doc
        let actns = map fromAction actionsOrCommands

        let importQualifiedPostSuggestion = "import Control.Monad qualified as Control"
        importControlMonadQualified <- liftIO $ inspectCodeAction actionsOrCommands [importQualifiedPostSuggestion]
        liftIO $ do
            dontExpectCodeAction actionsOrCommands ["import qualified Control.Monad as Control", "import Control.Monad (when)"]
            length actns >= 5 @? "There are some actions"

        executeCodeAction importControlMonadQualified

        contents <- documentContents doc
        liftIO $ T.lines contents !! 2 @?= "import Control.Monad qualified as Control"
    ]

testConfig :: Config
testConfig = def {
  formattingProvider = "none"
  }


