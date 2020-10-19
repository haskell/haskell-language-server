module Ide.Plugin.Tactic.Debug
  ( unsafeRender
  , unsafeRender'
  , traceM
  , traceShowId
  , trace
  , traceX
  , traceIdX
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

traceX :: (Show a) => String -> a -> b -> b
traceX str a = trace (mappend ("!!!" <> str <> ": ") $ show a)

traceIdX :: (Show a) => String -> a -> a
traceIdX str a = trace (mappend ("!!!" <> str <> ": ") $ show a) a

