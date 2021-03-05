module Main where

import           Class
import           Command
import           Completion
import           Config
import           Deferred
import           Definition
import           Diagnostic
import           Eval
import           Format
import           FunctionalBadProject
import           FunctionalCodeAction
import           FunctionalLiquid
import           HaddockComments
import           HieBios
import           Highlight
import           ModuleName
import           Progress
import           Reference
import           Rename
import           Splice
import           TypeSynonyms
import           Symbol
import           Test.Tasty
import           Test.Tasty.Ingredients.Rerun
import           Test.Tasty.Runners           (consoleTestReporter,
                                               listingTests)
import           Test.Tasty.Runners.AntXML
import           TypeDefinition

main :: IO ()
main =
    -- ingredient: xml runner writes json file of test results (https://github.com/ocharles/tasty-ant-xml/blob/master/Test/Tasty/Runners/AntXML.hs)
    --             rerunningTests allow rerun of failed tests (https://github.com/ocharles/tasty-rerun/blob/master/src/Test/Tasty/Ingredients/Rerun.hs)
    defaultMainWithIngredients
        [antXMLRunner, rerunningTests [listingTests, consoleTestReporter]]
        $ testGroup
            "haskell-language-server"
            [ Class.tests
            , Command.tests
            , Completion.tests
            , Config.tests
            , Deferred.tests
            , Definition.tests
            , Diagnostic.tests
            , Eval.tests
            , Format.tests
            , FunctionalBadProject.tests
            , FunctionalCodeAction.tests
            , FunctionalLiquid.tests
            , HieBios.tests
            , Highlight.tests
            , ModuleName.tests
            , Progress.tests
            , Reference.tests
            , Rename.tests
            , Symbol.tests
            , TypeDefinition.tests
            , Splice.tests
            , TypeSynonyms.tests
            , HaddockComments.tests
            ]
