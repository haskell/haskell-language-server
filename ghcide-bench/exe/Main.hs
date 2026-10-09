{- An automated benchmark built around the simple experiment described in:

  > https://neilmitchell.blogspot.com/2020/05/fixing-space-leaks-in-ghcide.html

  As an example project, it unpacks Cabal-3.2.0.0 in the local filesystem and
  loads the module 'Distribution.Simple'. The rationale for this choice is:

    - It's convenient to download with `cabal unpack Cabal-3.2.0.0`
    - It has very few dependencies, and all are already needed to build ghcide
    - Distribution.Simple has 235 transitive module dependencies, so non trivial

  The experiments are sequences of lsp commands scripted using lsp-test.
  A more refined approach would be to record and replay real IDE interactions,
  once the replay functionality is available in lsp-test.
  A more declarative approach would be to reuse ide-debug-driver:

  > https://github.com/digital-asset/daml/blob/master/compiler/damlc/ide-debug-driver/README.md

  The result of an experiment is a total duration in seconds after a preset
  number of iterations. There is ample room for improvement:
     - Statistical analysis to detect outliers and auto infer the number of iterations needed
     - GC stats analysis (currently -S is printed as part of the experiment)
     - Analysis of performance over the commit history of the project

  How to run:
     1. `cabal exec cabal run ghcide-bench -- -- ghcide-bench-options`
     1. `stack build ghcide:ghcide-bench && stack exec ghcide-bench -- -- ghcide-bench-options`

  Note that the package database influences the response times of certain actions,
  e.g. code actions, and therefore the two methods above do not necessarily
  produce the same results.

 -}

{-# LANGUAGE ImplicitParams #-}

import           Control.Exception.Safe
import           Control.Monad
import           Experiments
import           Options.Applicative
import           System.IO

optsP :: Parser (Config, Bool)
optsP = (,) <$> configP <*> switch (long "no-clean")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  (config, noClean) <- execParser $ info (optsP <**> helper) fullDesc
  let ?config = config

  hPrint stderr config

  output "starting test"

  SetupResult{..} <- setup

  runBenchmarks experiments `finally` unless noClean cleanUp
