-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main(main) where

import           Ide.Arguments (Arguments (..), GhcideArguments (..),
                                getArguments)
import           Ide.Main      (defaultMain)
import           Plugins

main :: IO ()
main = do
    args <- getArguments "haskell-language-server" (idePlugins False)

    let withExamples =
            case args of
                Ghcide GhcideArguments{..} -> argsExamplePlugin
                _                          -> False

    defaultMain args (idePlugins withExamples)
