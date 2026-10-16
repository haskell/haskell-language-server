{-# LANGUAGE OverloadedStrings #-}

module Development.IDE.Spans.Fixity (lookupFixities) where

import           Control.Monad.IO.Class          (MonadIO, liftIO)
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromMaybe)
import qualified Data.Set                        as S
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat.Util as Util

-- | Convert a HieAST to FixityTree with fixity info gathered
lookupFixities :: MonadIO m => HscEnv -> TcGblEnv -> S.Set Name -> m (M.Map Name Fixity)
lookupFixities hscEnv tcGblEnv names
    = liftIO
    $ fmap (fromMaybe M.empty . snd)
    $ initTcWithGbl hscEnv tcGblEnv (realSrcLocSpan $ mkRealSrcLoc "<dummy>" 1 1)
    $ M.traverseMaybeWithKey (\_ v -> v)
    $ M.fromSet lookupFixity names
  where
    lookupFixity name = do
      f <- Util.handleGhcException
        (const $ pure Nothing)
        (Just <$> lookupFixityRn name)
      if f == Just defaultFixity
      then pure Nothing
      else pure f
