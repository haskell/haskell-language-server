module Ide.Plugin.Tactic.BindSites
  ( BindSites ()
  , bindSites
  , getDefiningBindings
  ) where

import           Data.IntervalMap.FingerTree (IntervalMap, Interval (..))
import qualified Data.IntervalMap.FingerTree as IM
import qualified Data.Map as M
import qualified Data.Set as S
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error
import           Development.IDE.Types.Location
import           NameEnv
import           SrcLoc


-- TODO(sandy): Consolidate this with LocalBindings
data BindSites = BindSites
  { unBindSites :: IntervalMap Position (NameEnv (Name, Maybe Type))
  }

------------------------------------------------------------------------------
-- | Turn a 'RealSrcSpan' into an 'Interval'.
realSrcSpanToInterval :: RealSrcSpan -> Interval Position
realSrcSpanToInterval rss =
  Interval
    (realSrcLocToPosition $ realSrcSpanStart rss)
    (realSrcLocToPosition $ realSrcSpanEnd   rss)

------------------------------------------------------------------------------
-- | Compute which identifiers are in scope at every point in the AST. Use
-- 'getDefiningBindings' to find the results.
bindSites :: RefMap -> BindSites
bindSites refmap = BindSites $ foldr (uncurry IM.insert) mempty $ do
  (ident, refs)      <- M.toList refmap
  Right name         <- pure ident
  (_, ident_details) <- refs
  let ty = identType ident_details
  info        <- S.toList $ identInfo ident_details
  Just scope <- pure $ getBindSiteFromContext info
  pure ( realSrcSpanToInterval scope
       , unitNameEnv name (name,ty)
       )

------------------------------------------------------------------------------
-- | Given a 'BindSites', get every binding currently active at a given
-- 'RealSrcSpan',
getDefiningBindings :: BindSites -> RealSrcSpan -> [(Name, Maybe Type)]
getDefiningBindings bs rss
  = nameEnvElts
  $ foldMap snd
  $ IM.dominators (realSrcSpanToInterval rss)
  $ unBindSites bs

