{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import Data.Default
import Development.IDE.Main
import qualified Development.IDE.Plugin.HLS.GhcIde as Ghcide
import Ide.Plugin.Config
import Ide.Plugin.Brittany as B
import Ide.PluginUtils

main :: IO ()
main = defaultMain def
  { argsHlsPlugins = pluginDescToIdePlugins $
    [ B.descriptor "brittany"
    ] <>
    Ghcide.descriptors
  , argsDefaultHlsConfig = def { formattingProvider = "brittany" }
  }
