module Ide.Plugin.Tactic.Debug
  ( unsafeRender
  , unsafeRender'
  , traceM
  , traceShowId
  , trace
  , traceMX
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

traceMX :: (Monad m, Show a) => String -> a -> m ()
traceMX str a = traceM $ mappend ("!!!" <> str <> ": ") $ show a
