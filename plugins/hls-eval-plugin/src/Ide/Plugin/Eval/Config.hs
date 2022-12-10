{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.Eval.Config
  ( properties
  , getEvalConfig
  , EvalConfig(..)
  ) where

import           Ide.Plugin.Config     (Config)
import           Ide.Plugin.Properties
import           Ide.PluginUtils       (usePropertyLsp)
import           Ide.Types             (PluginId)
import           Language.LSP.Server   (MonadLsp)

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

getEvalConfig :: (MonadLsp Config m) => PluginId -> m EvalConfig
getEvalConfig plId =
    EvalConfig
    <$> usePropertyLsp #diff plId properties
    <*> usePropertyLsp #exception plId properties
