module Main where

import Test.Tasty
import Command
import Completion
import Deferred

main :: IO ()
main = defaultMain $ testGroup "HIE" [
          Command.tests
        , Completion.tests
        , Deferred.tests
    ]