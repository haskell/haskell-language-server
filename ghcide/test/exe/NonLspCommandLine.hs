
module NonLspCommandLine (tests) where

import           Development.IDE.Test.Runfiles
import           System.Environment.Blank      (setEnv)
import           System.Exit                   (ExitCode (ExitSuccess))
import           System.Process.Extra          (CreateProcess (cwd), proc,
                                                readCreateProcessWithExitCode)
import           Test.Tasty
import           Test.Tasty.HUnit
import           TestUtils


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
