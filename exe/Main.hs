-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main(main) where

import           Ide.Arguments             (Arguments (..), LspArguments (..),
                                            getArguments)
import           Ide.Main                  (defaultMain)
import           Ide.Types                 (IdePlugins)

 -- haskell-language-server plugins
import           Ide.Plugin.Eval           as Eval
import           Ide.Plugin.Example        as Example
import           Ide.Plugin.Example2       as Example2
import           Ide.Plugin.Floskell       as Floskell
import           Ide.Plugin.Fourmolu       as Fourmolu
import           Ide.Plugin.GhcIde         as GhcIde
import           Ide.Plugin.ImportLens     as ImportLens
import           Ide.Plugin.Ormolu         as Ormolu
import           Ide.Plugin.Retrie         as Retrie
import           Ide.Plugin.StylishHaskell as StylishHaskell
import           Ide.Plugin.Tactic         as Tactic
#if AGPL
import           Ide.Plugin.Brittany       as Brittany
#endif
import           Ide.Plugin                (pluginDescToIdePlugins)
import           Ide.Plugin.ModuleName     as ModuleName
import           Ide.Plugin.Pragmas        as Pragmas


-- ---------------------------------------------------------------------

-- | The plugins configured for use in this instance of the language
-- server.
-- These can be freely added or removed to tailor the available
-- features of the server.

idePlugins :: Bool -> IdePlugins
idePlugins includeExamples = pluginDescToIdePlugins allPlugins
  where
    allPlugins = if includeExamples
                   then basePlugins ++ examplePlugins
                   else basePlugins
    basePlugins =
      [ GhcIde.descriptor  "ghcide"
      , Pragmas.descriptor  "pragmas"
      , Floskell.descriptor "floskell"
      , Fourmolu.descriptor "fourmolu"
      , Tactic.descriptor "tactic"
      -- , genericDescriptor     "generic"
      -- , ghcmodDescriptor      "ghcmod"
      , Ormolu.descriptor   "ormolu"
      , StylishHaskell.descriptor "stylish-haskell"
      , Retrie.descriptor "retrie"
#if AGPL
      , Brittany.descriptor    "brittany"
#endif
      , Eval.descriptor "eval"
      , ImportLens.descriptor "importLens"
      , ModuleName.descriptor "moduleName"
      ]
    examplePlugins =
      [Example.descriptor  "eg"
      ,Example2.descriptor "eg2"
      ]

-- ---------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArguments "haskell-language-server"

    let withExamples =
            case args of
                LspMode (LspArguments{..}) -> argsExamplePlugin
                _                          -> False

    defaultMain args (idePlugins withExamples)
