{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
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
import           Data.Either (fromRight)
import qualified Data.Text as T
import qualified Debug.Trace
import           Development.IDE.GHC.Compat (PlainGhcException, Outputable(..), SDoc)
import           Development.IDE.GHC.Util (printOutputable)
import           System.IO.Unsafe  (unsafePerformIO)

------------------------------------------------------------------------------
-- | Print something
unsafeRender :: Outputable a => a -> String
unsafeRender = unsafeRender' . ppr


unsafeRender' :: SDoc -> String
unsafeRender' sdoc = unsafePerformIO $ do
  let z = T.unpack $ printOutputable sdoc
  -- We might not have unsafeGlobalDynFlags (like during testing), in which
  -- case GHC panics. Instead of crashing, let's just fail to print.
  !res <- try @PlainGhcException $ evaluate $ deepseq z z
  pure $ fromRight "<unsafeRender'>" res
{-# NOINLINE unsafeRender' #-}

traceMX :: (Monad m, Show a) => String -> a -> m ()
traceMX str a = traceM $ mappend ("!!!" <> str <> ": ") $ show a

traceX :: (Show a) => String -> a -> b -> b
traceX str a = trace (mappend ("!!!" <> str <> ": ") $ show a)

traceIdX :: (Show a) => String -> a -> a
traceIdX str a = trace (mappend ("!!!" <> str <> ": ") $ show a) a

traceFX :: String -> (a -> String) -> a -> a
traceFX str f a = trace (mappend ("!!!" <> str <> ": ") $ f a) a

traceM :: Applicative f => String -> f ()
trace :: String -> a -> a
traceShowId :: Show a => a -> a
#ifdef DEBUG
traceM = Debug.Trace.traceM
trace = Debug.Trace.trace
traceShowId = Debug.Trace.traceShowId
#else
traceM _ = pure ()
trace _ = id
traceShowId = id
#endif
