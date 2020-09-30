module Ide.Plugin.Tactic.Debug
  ( unsafeRender
  , unsafeRender'
  , traceM
  , traceShowId
  , trace
  ) where

import Debug.Trace
import DynFlags (unsafeGlobalDynFlags)
import Outputable

------------------------------------------------------------------------------
-- | Print something
unsafeRender :: Outputable a => a -> String
unsafeRender = unsafeRender' . ppr

unsafeRender' :: SDoc -> String
unsafeRender' = showSDoc unsafeGlobalDynFlags

