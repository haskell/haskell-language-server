module Main where

import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Test.Hls.Util
import Test.Tasty
import Test.Tasty.Ingredients.Rerun

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
main = do
    setupBuildToolFiles

    -- run a test session to warm up the cache to prevent timeouts in other tests
    putStrLn "Warming up haskell-language-server cache..."
    runSessionWithConfig (defaultConfig { messageTimeout = 120 }) hieCommand fullCaps "test/testdata" $
        liftIO $ putStrLn "haskell-language-server cache is warmed up"

    --TODO Test runner with config like HSpec??

    -- test tree
    defaultMainWithRerun $ testGroup "haskell-language-server" [
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