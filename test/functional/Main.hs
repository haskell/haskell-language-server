module Main where

import           Class
import           Command
import           Completion
import           Config
import           Deferred
import           Definition
import           Diagnostic
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
import           Symbol
import           Test.Hls
import           TypeDefinition

main :: IO ()
main = defaultTestRunner
        $ testGroup
            "haskell-language-server"
            [ Class.tests
            , Command.tests
            , Completion.tests
            , Config.tests
            , Deferred.tests
            , Definition.tests
            , Diagnostic.tests
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
            , HaddockComments.tests
            ]
