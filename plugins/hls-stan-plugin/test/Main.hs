module Main
  ( main,
  )
where

import           Control.Lens               ((^.))
import           Control.Monad              (void)
import           Data.List                  (find)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Ide.Plugin.Stan            as Stan
import           Ide.Types
import qualified Language.LSP.Protocol.Lens as L
import           System.FilePath
import           Test.Hls

main :: IO ()
main = defaultTestRunner tests

tests :: TestTree
tests =
  testGroup
    "stan suggestions"
    [ testCase "provides diagnostics" $
        runStanSession "" $ do
          doc <- openDoc "test.hs" "haskell"
          diags@(reduceDiag : _) <- waitForDiagnosticsFromSource doc "stan"
          liftIO $ do
            length diags @?= 1
            reduceDiag ^. L.range @?= Range (Position 0 0) (Position 3 19)
            reduceDiag ^. L.severity @?= Just DiagnosticSeverity_Hint
            let expectedPrefix = " ✲ Name:        "
            assertBool "" $ T.isPrefixOf expectedPrefix (reduceDiag ^. L.message)
            reduceDiag ^. L.source @?= Just "stan"
          return ()
    , testCase "ignores diagnostics from .stan.toml" $
        runStanSession "" $ do
          doc <- openDoc "dir/configTest.hs" "haskell"
          diags <- waitForDiagnosticsFromSource doc "stan"
          liftIO $ length diags @?= 0
          return ()
    ]

testDir :: FilePath
testDir = "plugins/hls-stan-plugin/test/testdata"

stanPlugin :: PluginTestDescriptor Stan.Log
stanPlugin = mkPluginTestDescriptor enabledStanDescriptor "stan"
  where
    -- We have to explicitly enable the plugin as it is disabled by default as
    -- per request: https://github.com/haskell/haskell-language-server/issues/3916
    --
    enabledStanDescriptor recorder plId =
      let stanPluginDescriptor = Stan.descriptor recorder plId
        in  stanPluginDescriptor
        { pluginConfigDescriptor = (pluginConfigDescriptor stanPluginDescriptor)
            { configInitialGenericConfig = (configInitialGenericConfig (pluginConfigDescriptor stanPluginDescriptor))
                { plcGlobalOn = True
                }
            }
        }

runStanSession :: FilePath -> Session a -> IO a
runStanSession subdir =
  failIfSessionTimeout . runSessionWithServer def stanPlugin (testDir </> subdir)
