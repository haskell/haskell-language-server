{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.Render.Config
( properties
, getRenderConfig
, RenderConfig (..)
) where

import qualified Data.Text             as T
import           Development.IDE
import           Ide.Plugin.Properties
import           Ide.Types             (PluginId)

newtype RenderConfig = RenderConfig
  { render_cfg_filepath      :: FilePath
  }
  deriving (Eq, Ord, Show)

properties :: Properties
    '[ 'PropertyKey "filepath" 'TString
     ]
properties = emptyProperties
  & defineStringProperty #filepath
    "Path to a Haskell file from which to load Render Actions" "./RenderActions.hs"

getRenderConfig :: PluginId -> Action RenderConfig
getRenderConfig plId =
    RenderConfig
    . T.unpack <$> usePropertyAction #filepath plId properties
