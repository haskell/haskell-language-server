module Main where

import Test.Tasty
import Test.Tasty.Runners (listingTests, consoleTestReporter)
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.Runners.AntXML

import Command
import Completion
import Deferred
import Definition
import Diagnostic
import Format
import FunctionalBadProject
import FunctionalCodeAction
import FunctionalLiquid
import HieBios
import Highlight
import Progress
import Reference
import Rename
import Symbol
import TypeDefinition

main :: IO ()
main =
    -- ingredient: xml runner writes json file of test results (https://github.com/ocharles/tasty-ant-xml/blob/master/Test/Tasty/Runners/AntXML.hs)
    --             rerunningTests allow rerun of failed tests (https://github.com/ocharles/tasty-rerun/blob/master/src/Test/Tasty/Ingredients/Rerun.hs)
    defaultMainWithIngredients [
          antXMLRunner
        , rerunningTests [ listingTests, consoleTestReporter ]
        ]
    $ testGroup "haskell-language-server" [
          Command.tests
        , Completion.tests
        , Deferred.tests
        , Definition.tests
        , Diagnostic.tests
        , Format.tests
        , FunctionalBadProject.tests
        , FunctionalCodeAction.tests
        , FunctionalLiquid.tests
        , HieBios.tests
        , Highlight.tests
        , Progress.tests
        , Reference.tests
        , Rename.tests
        , Symbol.tests
        , TypeDefinition.tests
        ]