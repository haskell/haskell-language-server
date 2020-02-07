module Main where

import           Control.Monad.IO.Class
import           Language.Haskell.LSP.Test
import qualified FunctionalSpec
import           Test.Hspec.Runner (hspecWith)
import           TestUtils

main :: IO ()
main = do
  setupBuildToolFiles
  -- run a test session to warm up the cache to prevent timeouts in other tests
  putStrLn "Warming up HIE cache..."
  putStrLn $ "hieCommand: " ++ hieCommand
  runSessionWithConfig (defaultConfig { messageTimeout = 120 }) hieCommand fullCaps "test/testdata" $
    liftIO $ putStrLn "HIE cache is warmed up"

  config <- getHspecFormattedConfig "functional"
  withFileLogging logFilePath $ hspecWith config FunctionalSpec.spec
