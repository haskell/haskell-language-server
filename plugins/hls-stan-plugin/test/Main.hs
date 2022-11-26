module Main
  ( main,
  )
where

import           Control.Lens            ((^.))
import           Control.Monad           (void)
import           Data.List               (find)
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.IO            as T
import qualified Ide.Plugin.Stan         as Stan
import qualified Language.LSP.Types.Lens as L
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
            reduceDiag ^. L.severity @?= Just DsHint
            let expectedPrefix = " ✲ Name:        "
            assertBool "" $ T.isPrefixOf expectedPrefix (reduceDiag ^. L.message)
            reduceDiag ^. L.source @?= Just "stan"
          return ()
    ]

testDir :: FilePath
testDir = "test/testdata"

stanPlugin :: PluginTestDescriptor Stan.Log
stanPlugin = mkPluginTestDescriptor Stan.descriptor "stan"

runStanSession :: FilePath -> Session a -> IO a
runStanSession subdir =
  failIfSessionTimeout . runSessionWithServer stanPlugin (testDir </> subdir)
