{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import           Data.Default
import           Development.IDE.Main
import qualified Development.IDE.Plugin.HLS.GhcIde as Ghcide
import           Ide.Plugin.Tactic                 as T
import           Ide.PluginUtils

main :: IO ()
main = defaultMain def
  { argsHlsPlugins = pluginDescToIdePlugins $
    [ T.descriptor "tactic"
    ] <>
    Ghcide.descriptors
  }

