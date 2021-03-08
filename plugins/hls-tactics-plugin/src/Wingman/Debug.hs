{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

module Wingman.Debug
  ( unsafeRender
  , unsafeRender'
  , traceM
  , traceShowId
  , trace
  , traceX
  , traceIdX
  , traceMX
  , traceFX
  ) where

import           Control.DeepSeq
import           Control.Exception
import           Debug.Trace
import           DynFlags          (unsafeGlobalDynFlags)
import           Outputable        hiding ((<>))
import           System.IO.Unsafe  (unsafePerformIO)

#if __GLASGOW_HASKELL__ >= 808
import           PlainPanic        (PlainGhcException)
type GHC_EXCEPTION = PlainGhcException
#else
import           Panic             (GhcException)
type GHC_EXCEPTION = GhcException
#endif


------------------------------------------------------------------------------
-- | Print something
unsafeRender :: Outputable a => a -> String
unsafeRender = unsafeRender' . ppr


unsafeRender' :: SDoc -> String
unsafeRender' sdoc = unsafePerformIO $ do
  let z = showSDoc unsafeGlobalDynFlags sdoc
  -- We might not have unsafeGlobalDynFlags (like during testing), in which
  -- case GHC panics. Instead of crashing, let's just fail to print.
  !res <- try @GHC_EXCEPTION $ evaluate $ deepseq z z
  pure $ either (const "<unsafeRender'>") id res
{-# NOINLINE unsafeRender' #-}

traceMX :: (Monad m, Show a) => String -> a -> m ()
traceMX str a = traceM $ mappend ("!!!" <> str <> ": ") $ show a

traceX :: (Show a) => String -> a -> b -> b
traceX str a = trace (mappend ("!!!" <> str <> ": ") $ show a)

traceIdX :: (Show a) => String -> a -> a
traceIdX str a = trace (mappend ("!!!" <> str <> ": ") $ show a) a

traceFX :: String -> (a -> String) -> a -> a
traceFX str f a = trace (mappend ("!!!" <> str <> ": ") $ f a) a

