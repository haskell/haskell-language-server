module Ide.Plugin.Tactic.CodeGen where

import Data.Map (Map)
import GHC.Exts
import GHC.SourceGen.Binds
import GHC.SourceGen.Expr
import GHC.SourceGen.Overloaded
import GHC.SourceGen.Pat
import OccName
import           DataCon
import           Development.IDE.GHC.Compat
import           Ide.Plugin.Tactic.Types
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Machinery
import Name
import Ide.Plugin.Tactic.Naming
import Data.Traversable
import Type hiding (Var)
import Data.List
import Control.Monad.Except


------------------------------------------------------------------------------
-- | Combinator for performign case splitting, and running sub-rules on the
-- resulting matches.
destruct' :: (DataCon -> Judgement -> Rule) -> OccName -> Judgement -> Rule
destruct' f term jdg@(Judgement hy _ g) = do
  case find ((== term) . fst) $ toList hy of
    Nothing -> throwError $ UndefinedHypothesis term
    Just (_, t) ->
      case splitTyConApp_maybe $ unCType t of
        Nothing -> throwError $ GoalMismatch "destruct" g
        Just (tc, apps) -> do
          fmap noLoc
              $ case' (var' term)
              <$> do
            for (tyConDataCons tc) $ \dc -> do
              let args = dataConInstArgTys dc apps
              names <- mkManyGoodNames hy args

              let pat :: Pat GhcPs
                  pat = conP (fromString $ occNameString $ nameOccName $ dataConName dc)
                      $ fmap bvar' names

              let j = destructing term
                    $ introducing (zip names $ coerce args)
                    $ withNewGoal g jdg
              sg <- f dc j
              pure $ match [pat] $ unLoc sg


------------------------------------------------------------------------------
-- | Construct a data con with subgoals for each field.
buildDataCon
    :: Map OccName CType  -- ^ In-scope bindings
    -> DataCon            -- ^ The data con to build
    -> [Type]             -- ^ Type arguments for the data con
    -> RuleM (LHsExpr GhcPs)
buildDataCon hy dc apps = do
  let args = dataConInstArgTys dc apps
  sgs <- traverse (newSubgoal hy . CType) args
  pure
    . noLoc
    . foldl' (@@)
        (HsVar noExtField $ noLoc $ Unqual $ nameOccName $ dataConName dc)
    $ fmap unLoc sgs


------------------------------------------------------------------------------
-- | Like 'var', but works over standard GHC 'OccName's.
var' :: Var a => OccName -> a
var' = var . fromString . occNameString

------------------------------------------------------------------------------
-- | Like 'bvar', but works over standard GHC 'OccName's.
bvar' :: BVar a => OccName -> a
bvar' = bvar . fromString . occNameString

