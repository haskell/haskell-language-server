module Main where

import Command
-- import Completion
-- import Deferred
-- import Definition
-- import Diagnostic
-- import Format
import FunctionalBadProject
-- import FunctionalCodeAction
-- import FunctionalLiquid
-- import HieBios
-- import Highlight
import Progress
import Reference
import Rename
import Symbol
import TypeDefinition
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "HIE" [
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