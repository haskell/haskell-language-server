module Main where

import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Test.Tasty
import TestUtils
import Commands
import Completions

main :: IO ()
main = do
  setupBuildToolFiles
  -- run a test session to warm up the cache to prevent timeouts in other tests
  putStrLn "Warming up HIE cache..."
  runSessionWithConfig (defaultConfig { messageTimeout = 120 }) hieCommand fullCaps "test/testdata" $
    liftIO $ putStrLn "HIE cache is warmed up"

  defaultMain $ testGroup "HIE" [
        Commands.tests
      , Completions.tests
    ]