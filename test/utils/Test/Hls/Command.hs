module Test.Hls.Command
  ( hlsExeCommand
  , hlsLspCommand
  , hlsWrapperLspCommand
  , hlsWrapperExeCommand
  )
where

import           Data.Maybe         (fromMaybe)
import           System.Environment (lookupEnv)
import           System.IO.Unsafe   (unsafePerformIO)

-- | The command to execute the version of hls for the current compiler.
--
-- Both @stack test@ and @cabal new-test@ setup the environment so @hls@ is
-- on PATH. Cabal seems to respond to @build-tool-depends@ specifically while
-- stack just puts all project executables on PATH.
hlsExeCommand :: String
{-# NOINLINE hlsExeCommand #-}
hlsExeCommand = unsafePerformIO $ do
  testExe <- fromMaybe "haskell-language-server" <$> lookupEnv "HLS_TEST_EXE"
  pure testExe

hlsLspCommand :: String
hlsLspCommand = hlsExeCommand ++ " --lsp --test -d -j4"

hlsWrapperLspCommand :: String
hlsWrapperLspCommand = hlsWrapperExeCommand ++ " --lsp --test -d -j4"

hlsWrapperExeCommand :: String
{-# NOINLINE hlsWrapperExeCommand #-}
hlsWrapperExeCommand = unsafePerformIO $ do
  testExe <- fromMaybe "haskell-language-server-wrapper" <$> lookupEnv "HLS_WRAPPER_TEST_EXE"
  pure testExe
