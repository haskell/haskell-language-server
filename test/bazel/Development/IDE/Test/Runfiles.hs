-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.Test.Runfiles
  ( locateHieCoreExecutable
  ) where

import System.FilePath ((</>), FilePath)

import DA.Bazel.Runfiles


locateHieCoreExecutable :: IO FilePath
locateHieCoreExecutable = locateRunfiles hieCoreExePath
  where
    hieCoreExePath = mainWorkspace </> exe "compiler/hie-core/hie-core-exe"
