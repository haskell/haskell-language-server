module Ide.Plugin.Tactic.Debug
  ( module Ide.Plugin.Tactic.Debug
  , traceM
  , traceShowId
  , trace
  ) where

import Debug.Trace
import DynFlags (unsafeGlobalDynFlags)
import Outputable hiding ((<>))

------------------------------------------------------------------------------
-- | Print something
unsafeRender :: Outputable a => a -> String
unsafeRender = unsafeRender' . ppr

unsafeRender' :: SDoc -> String
unsafeRender' = showSDoc unsafeGlobalDynFlags

