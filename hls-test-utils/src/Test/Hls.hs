module Test.Hls
  ( module Test.Tasty.HUnit,
    module Test.Tasty,
    module Test.Tasty.ExpectedFailure,
    module Test.Hls.Util,
    module Language.LSP.Types,
    module Language.LSP.Test,
    module Control.Monad.IO.Class,
    module Control.Applicative.Combinators,
    defaultTestRunner,
    goldenGitDiff,
    testCommand,
    def,
  )
where

import           Control.Applicative.Combinators
import           Control.Monad.IO.Class
import           Data.ByteString.Lazy            (ByteString)
import           Data.Default                    (def)
import           Language.LSP.Test
import           Language.LSP.Types
import           Test.Hls.Util
import           Test.Tasty                      hiding (Timeout)
import           Test.Tasty.ExpectedFailure
import           Test.Tasty.Golden
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.Runners
import           Test.Tasty.Runners.AntXML

-- | ingredient: xml runner writes json file of test results (https://github.com/ocharles/tasty-ant-xml/blob/master/Test/Tasty/Runners/AntXML.hs)
--               rerunningTests allow rerun of failed tests (https://github.com/ocharles/tasty-rerun/blob/master/src/Test/Tasty/Ingredients/Rerun.hs)
defaultTestRunner :: TestTree -> IO ()
defaultTestRunner =
  defaultMainWithIngredients
    [antXMLRunner, rerunningTests [listingTests, consoleTestReporter]]

gitDiff :: FilePath -> FilePath -> [String]
gitDiff fRef fNew = ["git", "diff", "--no-index", "--text", "--exit-code", fRef, fNew]

goldenGitDiff :: TestName -> FilePath -> IO ByteString -> TestTree
goldenGitDiff name = goldenVsStringDiff name gitDiff

testCommand :: String
testCommand = "test-server"
