
module PluginParsedResultTests (tests) where

import           Development.IDE.Test (expectNoMoreDiagnostics)
import           Language.LSP.Test
import           System.FilePath
-- import Test.QuickCheck.Instances ()
import           Test.Tasty
import           TestUtils

tests :: TestTree
tests =
  ignoreForGHC92Plus "No need for this plugin anymore!" $
  testSessionWithExtraFiles "plugin-recorddot" "parsedResultAction plugin" $ \dir -> do
    _ <- openDoc (dir</> "RecordDot.hs") "haskell"
    expectNoMoreDiagnostics 2
