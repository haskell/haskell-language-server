-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.Test.Runfiles
  ( locateGhcideExecutable
  ) where

locateGhcideExecutable :: IO FilePath
locateGhcideExecutable = pure "ghcide"
