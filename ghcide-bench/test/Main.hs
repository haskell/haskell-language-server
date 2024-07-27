-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ImplicitParams        #-}
{-# OPTIONS_GHC -Wno-deprecations -Wno-unticked-promoted-constructors #-}

module Main (main) where

import           Data.List.Extra
import qualified Experiments                  as Bench
import           Language.LSP.Test
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.Ingredients.Rerun (defaultMainWithRerun)

main :: IO ()
main = defaultMainWithRerun benchmarkTests

benchmarkTests :: TestTree
benchmarkTests =
    let ?config = Bench.defConfig
            { Bench.verbosity = Bench.Quiet
            , Bench.repetitions = Just 3
            , Bench.buildTool = Bench.Cabal
            } in
    withResource Bench.setup Bench.cleanUp $ \getResource -> testGroup "benchmark experiments"
    [ testCase (Bench.name e) $ do
        Bench.SetupResult{Bench.benchDir} <- getResource
        res <- Bench.runBench (runInDir benchDir) e
        assertBool "did not successfully complete 5 repetitions" $ Bench.success res
        | e <- Bench.experiments
        , Bench.name e /= "edit" -- the edit experiment does not ever fail
        , Bench.name e /= "hole fit suggestions" -- is too slow!
        -- the cradle experiments are way too slow
        , not ("cradle" `isInfixOf` Bench.name e)
    ]

runInDir :: FilePath -> Session a -> IO a
runInDir dir = runSessionWithConfig defaultConfig cmd fullLatestClientCaps dir
  where
    -- TODO use HLS instead of ghcide
    cmd = "ghcide --lsp --test --verbose -j2 --cwd " <> dir
