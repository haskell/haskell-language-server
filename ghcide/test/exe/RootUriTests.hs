
module RootUriTests (tests) where

import           Control.Monad.IO.Class   (liftIO)
import           Development.IDE.GHC.Util
import           Development.IDE.Test     (expectNoMoreDiagnostics)
import           Language.LSP.Test
import           System.FilePath
-- import Test.QuickCheck.Instances ()
import           Config
import           Data.Default             (def)
import           Test.Hls                 (TestConfig (..),
                                           runSessionWithTestConfig)
import           Test.Hls.FileSystem      (copyDir)
import           Test.Tasty
import           Test.Tasty.HUnit


-- | checks if we use InitializeParams.rootUri for loading session
tests :: TestTree
tests = testCase "use rootUri" . runTest "dirA" "dirB" $ \dir -> do
  let bPath = dir </> "dirB/Foo.hs"
  bSource <- liftIO $ readFileUtf8 bPath
  _ <- createDoc "Foo.hs" "haskell" bSource
  expectNoMoreDiagnostics 0.5
  where
    -- similar to run' except we can configure where to start ghcide and session
    runTest :: FilePath -> FilePath -> (FilePath -> Session ()) -> IO ()
    runTest dir1 dir2 = runSessionWithTestConfig
        def
        {
            testPluginDescriptor = dummyPlugin
            , testDirLocation = Right $ mkIdeTestFs [copyDir "rootUri"]
            , testServerRoot = Just dir1
            , testClientRoot = Just dir2
            , testShiftRoot = True
        }


