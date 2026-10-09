
module NonLspCommandLine (tests) where

import           Control.Monad            ((>=>))
import           Data.Foldable            (for_)
import           Development.Shake        (getDirectoryFilesIO)
import           System.Directory         (copyFile, createDirectoryIfMissing)
import           System.Directory.Extra   (canonicalizePath)
import           System.Environment.Blank (setEnv)
import           System.Exit              (ExitCode (ExitSuccess))
import           System.FilePath          (takeDirectory, (</>))
import qualified System.IO.Extra
import           System.Process.Extra     (CreateProcess (cwd), proc,
                                           readCreateProcessWithExitCode)
import           Test.Tasty
import           Test.Tasty.HUnit
import           Config                   (testDataDir)


-- A test to ensure that the command line ghcide workflow stays working
tests :: TestTree
tests = testGroup "ghcide command line"
  [ testCase "works" $ withTempDir $ \dir -> do
        ghcide <- locateGhcideExecutable
        copyTestDataFiles dir "multi"
        let cmd = (proc ghcide ["a/A.hs"]){cwd = Just dir}

        setEnv "HOME" "/homeless-shelter" False

        (ec, _, _) <- readCreateProcessWithExitCode cmd ""

        ec @?= ExitSuccess
  ]

locateGhcideExecutable :: IO FilePath
locateGhcideExecutable = pure "ghcide"

-- | Version of 'System.IO.Extra.withTempDir' that canonicalizes the path
-- Which we need to do on macOS since the $TMPDIR can be in @/private/var@ or
-- @/var@
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir f = System.IO.Extra.withTempDir $ canonicalizePath >=> f


copyTestDataFiles :: FilePath -> FilePath -> IO ()
copyTestDataFiles dir prefix = do
  -- Copy all the test data files to the temporary workspace
  testDataFiles <- getDirectoryFilesIO (testDataDir </> prefix) ["//*"]
  for_ testDataFiles $ \f -> do
    createDirectoryIfMissing True $ dir </> takeDirectory f
    copyFile (testDataDir </> prefix </> f) (dir </> f)
