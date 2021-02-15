{-# LANGUAGE TypeApplications #-}

module Ide.Plugin.Tactic.CaseSplit
  ( mkFirstAgda
  , iterateSplit
  , splitToDecl
  ) where

import Control.Lens
import Data.Bool (bool)
import Development.IDE.GHC.Compat
import GHC.Exts (IsString(fromString))
import GHC.SourceGen (funBinds, match)
import Ide.Plugin.Tactic.GHC
import Ide.Plugin.Tactic.Types
import OccName



mkFirstAgda :: [Pat GhcPs] -> HsExpr GhcPs -> AgdaMatch
mkFirstAgda pats (Lambda pats' body) = mkFirstAgda (pats <> pats') body
mkFirstAgda pats body = AgdaMatch pats body


agdaSplit :: AgdaMatch -> [AgdaMatch]
agdaSplit (AgdaMatch pats (Case (HsVar _ (L _ var)) matches)) = do
  (i, pat) <- zip [id @Int 0 ..] pats
  case pat of
    VarPat _ (L _ patname) | eqRdrName patname var -> do
      (case_pat, body) <- matches
      -- TODO(sandy): use an at pattern if necessary
      pure $ AgdaMatch (pats & ix i .~ case_pat) body
    _ -> []
agdaSplit x = [x]


wildify :: AgdaMatch -> AgdaMatch
wildify (AgdaMatch pats body) =
  let make_wild = bool id (wildifyT (allOccNames body)) $ not $ containsHole body
   in AgdaMatch (make_wild pats) body


splitToDecl :: OccName -> [AgdaMatch] -> LHsDecl GhcPs
splitToDecl name ams = noLoc $ funBinds (fromString . occNameString . occName $ name) $ do
  AgdaMatch pats body <- ams
  pure $ match pats body


iterateSplit :: AgdaMatch -> [AgdaMatch]
iterateSplit am =
  let iterated = iterate (agdaSplit =<<) $ pure am
   in fmap wildify . head . drop 5 $ iterated

