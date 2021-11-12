module Main where

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
import           HieBios
import           Highlight
import           Progress
import           Reference
import           Symbol
import           Test.Hls
import           TypeDefinition

main :: IO ()
main = defaultTestRunner
        $ testGroup
            "haskell-language-server"
            [ Command.tests
            , Completion.tests
            , Config.tests
            , Deferred.tests
            , Definition.tests
            , Diagnostic.tests
            , ignoreInEnv [HostOS Windows, GhcVer GHC90] "Tests gets stuck in ci" $ Format.tests
            , FunctionalBadProject.tests
            , FunctionalCodeAction.tests
            , FunctionalLiquid.tests
            , HieBios.tests
            , Highlight.tests
            , ignoreInEnv [HostOS Windows, GhcVer GHC90] "Tests gets stuck in ci" $ Progress.tests
            , Reference.tests
            , ignoreInEnv [HostOS Windows, GhcVer GHC90] "Tests gets stuck in ci" $ Symbol.tests
            , TypeDefinition.tests
            ]
