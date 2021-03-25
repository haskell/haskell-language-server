
{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Data.Default
import           Development.IDE.Main
import qualified Development.IDE.Plugin.HLS.GhcIde as Ghcide
import qualified Ide.Plugin.Class                  as Class
import           Ide.Plugin.Config
import           Ide.PluginUtils

main :: IO ()
main = defaultMain def
  { argsHlsPlugins = pluginDescToIdePlugins $
    [ Class.descriptor "class"
    ] <>
    Ghcide.descriptors
  }
