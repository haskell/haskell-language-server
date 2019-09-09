-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.Test.Runfiles
  ( locateGhcideExecutable
  ) where

import System.FilePath ((</>), FilePath)

import DA.Bazel.Runfiles


locateGhcideExecutable :: IO FilePath
locateGhcideExecutable = locateRunfiles ghcideExePath
  where
    ghcideExePath = mainWorkspace </> exe "compiler/ghcide/ghcide-exe"
