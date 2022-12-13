{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.Eval.Config
  ( properties
  , getEvalConfig
  , EvalConfig(..)
  ) where

import           Development.IDE
import           Ide.Plugin.Properties
import           Ide.Types             (PluginId)

-- | The Eval plugin configuration. (see 'properties')
data EvalConfig = EvalConfig
  { eval_cfg_diff      :: Bool
  , eval_cfg_exception :: Bool
  }
  deriving (Eq, Ord, Show)

properties :: Properties
    '[ 'PropertyKey "exception" 'TBoolean
     , 'PropertyKey "diff" 'TBoolean
     ]
properties = emptyProperties
  & defineBooleanProperty #diff
    "Enable the diff output (WAS/NOW) of eval lenses" True
  & defineBooleanProperty #exception
    "Enable marking exceptions with `*** Exception:` similarly to doctest and GHCi." False

getEvalConfig :: PluginId -> Action EvalConfig
getEvalConfig plId =
    EvalConfig
    <$> usePropertyAction #diff plId properties
    <*> usePropertyAction #exception plId properties
