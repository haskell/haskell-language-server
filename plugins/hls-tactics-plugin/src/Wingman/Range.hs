{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes     #-}

module Wingman.Range where

import           Development.IDE hiding (rangeToRealSrcSpan, rangeToSrcSpan)
import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Util as FS



------------------------------------------------------------------------------
-- | Convert a DAML compiler Range to a GHC SrcSpan
-- TODO(sandy): this doesn't belong here
rangeToSrcSpan :: String -> Range -> SrcSpan
rangeToSrcSpan file range = RealSrcSpan (rangeToRealSrcSpan file range) Nothing


rangeToRealSrcSpan :: String -> Range -> RealSrcSpan
rangeToRealSrcSpan file (Range (Position startLn startCh) (Position endLn endCh)) =
  mkRealSrcSpan
    (mkRealSrcLoc (FS.fsLit file) (startLn + 1) (startCh + 1))
    (mkRealSrcLoc (FS.fsLit file) (endLn + 1) (endCh + 1))

