module Main where

import Command
import Completion
import Deferred
import Definition
import Diagnostic
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "HIE" [
          Command.tests
        , Completion.tests
        , Deferred.tests
        , Definition.tests
        , Diagnostic.tests
    ]