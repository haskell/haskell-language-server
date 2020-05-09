module Main where

import Control.Monad.IO.Class
import Language.Haskell.LSP.Test
import Test.Tasty
import TestUtils
-- import TastyUtils

import Ide.Plugin.Brittany (tests)

main :: IO ()
main = do
  setupBuildToolFiles
  -- run a test session to warm up the cache to prevent timeouts in other tests
  putStrLn "Warming up HIE cache..."
  runSessionWithConfig (defaultConfig { messageTimeout = 120 }) hieCommand fullCaps "test/testdata" $
    liftIO $ putStrLn "HIE cache is warmed up"

  defaultMain tree

tree :: TestTree
tree = testGroup "HIE" [
    Ide.Plugin.Brittany.tests
    ]