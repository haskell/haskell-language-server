module Test.Hls.Command
  ( hlsCommand,
    hlsCommandExamplePlugin,
    hlsCommandVomit,
    logFilePath,
  )
where

import           Data.Maybe         (fromMaybe)
import           System.Environment (lookupEnv)
import           System.IO.Unsafe   (unsafePerformIO)
import           Test.Hls

logFilePath :: String
logFilePath = "hls-" ++ show ghcVersion ++ ".log"

-- | The command to execute the version of hls for the current compiler.
--
-- Both @stack test@ and @cabal new-test@ setup the environment so @hls@ is
-- on PATH. Cabal seems to respond to @build-tool-depends@ specifically while
-- stack just puts all project executables on PATH.
hlsCommand :: String
{-# NOINLINE hlsCommand #-}
hlsCommand = unsafePerformIO $ do
  testExe <- fromMaybe "haskell-language-server" <$> lookupEnv "HLS_TEST_EXE"
  pure $ testExe ++ " --lsp -d -j4"

hlsCommandVomit :: String
hlsCommandVomit = hlsCommand ++ " --vomit"

hlsCommandExamplePlugin :: String
hlsCommandExamplePlugin = hlsCommand ++ " --example"
