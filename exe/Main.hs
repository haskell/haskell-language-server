-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main(main) where

import           Ide.Arguments (Arguments (..), LspArguments (..), getArguments)
import           Ide.Main      (defaultMain)
import           Main.Utf8     (withUtf8)
import           Plugins

main :: IO ()
main = withUtf8 $ do
    args <- getArguments "haskell-language-server"

    let withExamples =
            case args of
                LspMode LspArguments{..} -> argsExamplePlugin
                _                        -> False

    defaultMain args (idePlugins withExamples)
