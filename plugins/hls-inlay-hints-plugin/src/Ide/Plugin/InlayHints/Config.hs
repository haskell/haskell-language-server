{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.InlayHints.Config (
    InlayHintsConfig(..),
    getInlayHintsConfig,
    properties
) where

import           Development.IDE       (Action, usePropertyAction)
import           Ide.Plugin.Properties
import           Ide.Types             (PluginId)

-- | The Inlay Hints plugin configuration. (see 'properties')
data InlayHintsConfig = InlayHintsConfig
  { enableAll          :: Bool
  , enableFixity       :: Bool
  , enableHole         :: Bool
  , enableLocalBinding :: Bool
  }
  deriving (Eq, Ord, Show)

properties :: Properties
    '[ 'PropertyKey "all" 'TBoolean
    , 'PropertyKey "fixity" 'TBoolean
    , 'PropertyKey "hole" 'TBoolean
    , 'PropertyKey "localBinding" 'TBoolean
    ]
properties = emptyProperties
    & defineBooleanProperty #localBinding
      "Enable the local binding type (e.g. `let`) inlay hints" False
    & defineBooleanProperty #hole
      "Enable the hole type inlay hints" False
    & defineBooleanProperty #fixity
      "Enable the operator fixity inlay hints" False
    & defineBooleanProperty #all
      "Enable ALL inlay hints" False

getInlayHintsConfig :: PluginId -> Action InlayHintsConfig
getInlayHintsConfig plId =
    InlayHintsConfig
    <$> usePropertyAction #all plId properties
    <*> usePropertyAction #fixity plId properties
    <*> usePropertyAction #hole plId properties
    <*> usePropertyAction #localBinding plId properties
