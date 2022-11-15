{-# LANGUAGE EmptyCase #-}
module Wingman.CaseSplit
  ( mkFirstAgda
  , iterateSplit
  , splitToDecl
  ) where

import           Data.Bool (bool)
import           Data.Data hiding (Prefix, Infix)
import           Data.Generics hiding (Prefix, Infix)
import           Data.Set (Set)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat
import           GHC.Exts (IsString (fromString))
import           GHC.SourceGen
import           Wingman.GHC
import           Wingman.Types
import GHC.SourceGen.Binds
import GHC.SourceGen.Name
import Development.IDE.GHC.ExactPrint (annotateDecl)
import Language.Haskell.GHC.ExactPrint (runTransformT)
import Data.Either (fromRight)
import Language.Haskell.GHC.ExactPrint.Transform (setEntryDP)
import GHC (DeltaPos(..), SrcSpanAnn'(..), EpAnn (..), emptyComments, Anchor (..), realSrcSpan, AnchorOperation (..))
import GHC.Types.SrcLoc (generatedSrcSpan)
import Language.Haskell.GHC.ExactPrint.ExactPrint (showAst)



------------------------------------------------------------------------------
-- | Construct an 'AgdaMatch' from patterns in scope (should be the LHS of the
-- match) and a body.
mkFirstAgda :: [Pat GhcPs] -> HsExpr GhcPs -> AgdaMatch
mkFirstAgda pats (Lambda pats' body) = mkFirstAgda (pats <> pats') body
mkFirstAgda pats body                = AgdaMatch pats body


------------------------------------------------------------------------------
-- | Transform an 'AgdaMatch' whose body is a case over a bound pattern, by
-- splitting it into multiple matches: one for each alternative of the case.
agdaSplit :: AgdaMatch -> [AgdaMatch]
agdaSplit (AgdaMatch pats (Case (HsVar _ (L _ var)) matches))
  -- Ensure the thing we're destructing is actually a pattern that's been
  -- bound.
  | containsVar var pats
  = do
    (pat, body) <- matches
    -- TODO(sandy): use an at pattern if necessary
    pure $ AgdaMatch (rewriteVarPat var pat pats) $ unLoc body
agdaSplit x = [x]


------------------------------------------------------------------------------
-- | Replace unused bound patterns with wild patterns.
wildify :: AgdaMatch -> AgdaMatch
wildify (AgdaMatch pats body) =
  let make_wild = bool id (wildifyT (allOccNames body)) $ not $ containsHole body
   in AgdaMatch (make_wild pats) body


------------------------------------------------------------------------------
-- | Helper function for 'wildify'.
wildifyT :: Data a => Set OccName -> a -> a
wildifyT (S.map occNameString -> used) = everywhere $ mkT $ \case
  VarPat _ (L _ var) | S.notMember (occNameString $ occName var) used -> wildP
  (x :: Pat GhcPs)                                                    -> x


------------------------------------------------------------------------------
-- | Determine whether the given 'RdrName' exists as a 'VarPat' inside of @a@.
containsVar :: Data a => RdrName -> a -> Bool
containsVar name = everything (||) $
  mkQ False (\case
    VarPat _ (L _ var) -> eqRdrName name var
    (_ :: Pat GhcPs)   -> False
      )
  `extQ` \case
    HsRecField _ lbl _ True ->  eqRdrName name $ unLoc $ rdrNameFieldOcc $ unLoc lbl
    (_ :: HsRecField' (FieldOcc GhcPs) (PatCompat GhcPs)) -> False


------------------------------------------------------------------------------
-- | Replace a 'VarPat' with the given @'Pat' GhcPs@.
rewriteVarPat :: Data a => RdrName -> Pat GhcPs -> a -> a
rewriteVarPat name rep = everywhere $
  mkT (\case
    VarPat xVarPat (L _ var) | eqRdrName name var -> rep
    (x :: Pat GhcPs)                        -> x
      )
  `extT` \case
    HsRecField ann lbl _ True
      | eqRdrName name $ unLoc $ rdrNameFieldOcc $ unLoc lbl
          -> HsRecField ann lbl (toPatCompat rep) False
    (x :: HsRecField' (FieldOcc GhcPs) (PatCompat GhcPs)) -> x


------------------------------------------------------------------------------
-- | Construct an 'HsDecl' from a set of 'AgdaMatch'es.
splitToDecl
    :: DynFlags -> Maybe LexicalFixity
    -> OccName  -- ^ The name of the function
    -> [AgdaMatch]
    -> LHsDecl GhcPs
splitToDecl dflags fixity name ams = do
  let res = traceX "fixity" fixity $
        -- L (SrcSpanAnn (EpAnn (Anchor (realSrcSpan generatedSrcSpan) (MovedAnchor $ DifferentLine 1 0)) mempty emptyComments) generatedSrcSpan) $
        L (SrcSpanAnn EpAnnNotUsed generatedSrcSpan) $
          funBindsWithFixity fixity (fromString . occNameString . occName $ name) $ do
            AgdaMatch pats body <- ams
            pure $ match pats body
      res' = either error (\(a,b,c) -> a) $ runTransformT $ annotateDecl dflags res
      -- There is a bug here such that each match doesn't get a delta to be on the next line, and so we manually set thos
      -- deltas...
      res'' = case res' of
        L l (ValD xValD funBind@FunBind {fun_matches=MG xMg (L lMatches (m:ms)) originMg}) ->
          L l (ValD xValD (funBind {fun_matches=MG xMg (L lMatches $ (m:(flip setEntryDP (DifferentLine 1 0) <$> ms))) originMg}))
        _ -> error "bad"
    in trace "split_to_decl" $ res''

------------------------------------------------------------------------------
-- | Sometimes 'agdaSplit' exposes another opportunity to do 'agdaSplit'. This
-- function runs it a few times, hoping it will find a fixpoint.
iterateSplit :: AgdaMatch -> [AgdaMatch]
iterateSplit am =
  let iterated = iterate (agdaSplit =<<) $ pure am
   in fmap wildify . (!! 5) $ iterated

