{-# LANGUAGE OverloadedStrings #-}
module Main(main) where

import           Data.Default
import           Development.IDE.Main
import qualified Development.IDE.Plugin.HLS.GhcIde as Ghcide
import           Ide.Plugin.Config
import qualified Ide.Plugin.Splice                 as Splice
import           Ide.PluginUtils

main :: IO ()
main = defaultMain def
  { argsHlsPlugins = pluginDescToIdePlugins $
    [ Splice.descriptor "splice"
    ] <>
    Ghcide.descriptors
  }
