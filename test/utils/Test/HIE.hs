{-# LANGUAGE CPP, OverloadedStrings #-}
module Test.HIE (hieCommand) where

data GhcVersion
  = GHC88
  | GHC86
  | GHC84
  deriving (Eq,Show)

ghcVersion :: GhcVersion
#if (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,8,0,0)))
ghcVersion = GHC88
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,6,0,0)))
ghcVersion = GHC86
#elif (defined(MIN_VERSION_GLASGOW_HASKELL) && (MIN_VERSION_GLASGOW_HASKELL(8,4,0,0)))
ghcVersion = GHC84
#endif

logFilePath :: String
logFilePath = "hie-" ++ show ghcVersion ++ ".log"

-- | The command to execute the version of hie for the current compiler.
--
-- Both @stack test@ and @cabal new-test@ setup the environment so @hie@ is
-- on PATH. Cabal seems to respond to @build-tool-depends@ specifically while
-- stack just puts all project executables on PATH.
hieCommand :: String
-- hieCommand = "hie --lsp --bios-verbose -d -l test-logs/" ++ logFilePath
-- hieCommand = "haskell-language-server --lsp"
-- hieCommand = "haskell-language-server --lsp --test --shake-profiling=test-logs/" ++ logFilePath
hieCommand = "haskell-language-server --lsp -d -l test-logs/" ++ logFilePath