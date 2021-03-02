{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}

{-# LANGUAGE DeriveDataTypeable #-}
module Ide.Plugin.Tactic.CodeGen
  ( module Ide.Plugin.Tactic.CodeGen
  , module Ide.Plugin.Tactic.CodeGen.Utils
  ) where


import           Control.Lens ((%~), (<>~), (&))
import           Control.Monad.Except
import           Data.Generics.Labels ()
import           Data.List
import qualified Data.Set as S
import           Data.Traversable
import           DataCon
import           Development.IDE.GHC.Compat
import           GHC.Exts
import           GHC.SourceGen.Binds
import           GHC.SourceGen.Expr
import           GHC.SourceGen.Overloaded
import           GHC.SourceGen.Pat
import           Ide.Plugin.Tactic.CodeGen.Utils
import           Ide.Plugin.Tactic.GHC
import           Ide.Plugin.Tactic.Judgements
import           Ide.Plugin.Tactic.Machinery
import           Ide.Plugin.Tactic.Naming
import           Ide.Plugin.Tactic.Types
import           Type hiding (Var)
import Data.Generics.Twins
import Data.Data (Data, Typeable, cast)
import GhcPlugins (occName, occNameString)
import Control.Monad.Writer
import Data.Generics hiding (isAlgType)


destructMatches
    :: (DataCon -> Judgement -> Rule)
       -- ^ How to construct each match
    -> Maybe OccName
       -- ^ Scrutinee
    -> CType
       -- ^ Type being destructed
    -> Judgement
    -> RuleM (Synthesized [RawMatch])
destructMatches f scrut t jdg = do
  let hy = jEntireHypothesis jdg
      g  = jGoal jdg
  case splitTyConApp_maybe $ unCType t of
    Nothing -> throwError $ GoalMismatch "destruct" g
    Just (tc, apps) -> do
      let dcs = tyConDataCons tc
      case dcs of
        [] -> throwError $ GoalMismatch "destruct" g
        _ -> fmap unzipTrace $ for dcs $ \dc -> do
          let args = dataConInstOrigArgTys' dc apps
          names <- mkManyGoodNames (hyNamesInScope hy) args
          let hy' = patternHypothesis scrut dc jdg
                  $ zip names
                  $ coerce args
              j = introduce hy'
                $ withNewGoal g jdg
          ext <- f dc j
          pure $ ext
            & #syn_trace %~ rose ("match " <> show dc <> " {" <> intercalate ", " (fmap show names) <> "}")
                          . pure
            & #syn_scoped <>~ hy'
            & #syn_val     %~ match [mkDestructPat dc names] . unLoc


------------------------------------------------------------------------------
-- | Produces a pattern for a data con and the names of its fields.
mkDestructPat :: DataCon -> [OccName] -> Pat GhcPs
mkDestructPat dcon names
  | isTupleDataCon dcon =
      tuple pat_args
  | otherwise =
      infixifyPatIfNecessary dcon $
        conP
          (coerceName $ dataConName dcon)
          pat_args
  where
    pat_args = fmap bvar' names


infixifyPatIfNecessary :: DataCon -> Pat GhcPs -> Pat GhcPs
infixifyPatIfNecessary dcon x
  | dataConIsInfix dcon =
      case x of
        ConPatIn op (PrefixCon [lhs, rhs]) ->
          ConPatIn op $ InfixCon lhs rhs
        y -> y
  | otherwise = x



unzipTrace :: [Synthesized a] -> Synthesized [a]
unzipTrace = sequenceA


-- | Essentially same as 'dataConInstOrigArgTys' in GHC,
--  but only accepts universally quantified types as the second arguments
--  and automatically introduces existentials.
--
-- NOTE: The behaviour depends on GHC's 'dataConInstOrigArgTys'.
--       We need some tweaks if the compiler changes the implementation.
dataConInstOrigArgTys'
  :: DataCon
      -- ^ 'DataCon'structor
  -> [Type]
      -- ^ /Universally/ quantified type arguments to a result type.
      --   It /MUST NOT/ contain any dictionaries, coercion and existentials.
      --
      --   For example, for @MkMyGADT :: b -> MyGADT a c@, we
      --   must pass @[a, c]@ as this argument but not @b@, as @b@ is an existential.
  -> [Type]
      -- ^ Types of arguments to the DataCon with returned type is instantiated with the second argument.
dataConInstOrigArgTys' con uniTys =
  let exvars = dataConExTys con
   in dataConInstOrigArgTys con $
        uniTys ++ fmap mkTyVarTy exvars
      -- Rationale: At least in GHC <= 8.10, 'dataConInstOrigArgTys'
      -- unifies the second argument with DataCon's universals followed by existentials.
      -- If the definition of 'dataConInstOrigArgTys' changes,
      -- this place must be changed accordingly.

------------------------------------------------------------------------------
-- | Combinator for performing case splitting, and running sub-rules on the
-- resulting matches.

destruct' :: (DataCon -> Judgement -> Rule) -> HyInfo CType -> Judgement -> Rule
destruct' f hi jdg = do
  when (isDestructBlacklisted jdg) $ throwError NoApplicableTactic
  let term = hi_name hi
  ext
      <- destructMatches
           f
           (Just term)
           (hi_type hi)
           $ disallowing AlreadyDestructed [term] jdg
  pure $ ext
    & #syn_trace     %~ rose ("destruct " <> show term) . pure
    & #syn_used_vals %~ S.insert term
    & #syn_val       %~ noLoc . case' (var' term)


------------------------------------------------------------------------------
-- | Combinator for performign case splitting, and running sub-rules on the
-- resulting matches.
destructLambdaCase' :: (DataCon -> Judgement -> Rule) -> Judgement -> Rule
destructLambdaCase' f jdg = do
  when (isDestructBlacklisted jdg) $ throwError NoApplicableTactic
  let g  = jGoal jdg
  case splitFunTy_maybe (unCType g) of
    Just (arg, _) | isAlgType arg ->
      fmap (fmap noLoc lambdaCase) <$>
        destructMatches f Nothing (CType arg) jdg
    _ -> throwError $ GoalMismatch "destructLambdaCase'" g


------------------------------------------------------------------------------
-- | Construct a data con with subgoals for each field.
buildDataCon
    :: Judgement
    -> DataCon            -- ^ The data con to build
    -> [Type]             -- ^ Type arguments for the data con
    -> RuleM (Synthesized (LHsExpr GhcPs))
buildDataCon jdg dc tyapps = do
  let args = dataConInstOrigArgTys' dc tyapps
  ext
      <- fmap unzipTrace
       $ traverse ( \(arg, n) ->
                    newSubgoal
                  . filterSameTypeFromOtherPositions dc n
                  . blacklistingDestruct
                  . flip withNewGoal jdg
                  $ CType arg
                  ) $ zip args [0..]
  pure $ ext
    & #syn_trace %~ rose (show dc) . pure
    & #syn_val   %~ mkCon dc


data Tree =  Leaf | Branch Tree Int Tree
  deriving (Data, Typeable)

test = case' (var "x") [match [bvar "y"] $ var "z"]
test2 = case' (var "z") [match [bvar "y"] $ var "x"]

instance Show RdrName where
  show = unsafeRender

-- mkQQ :: (Typeable a, Typeable b, Typeable c, Monad m)
--      => (a -> a -> m a) -> b -> c -> m c
-- mkQQ (f::a -> a -> m a) x y =
--   case (cast x,cast y) of
--     (Just (x'::a),Just (y'::a)) -> cast (f x' y')
--     _                           -> pure ()




gzipQ :: forall r. GenericQ (GenericQ [r]) -> GenericQ (GenericQ [r])
gzipQ f = go
  where
    go :: GenericQ (GenericQ [r])
    go x y = f x y <>
      if toConstr x == toConstr y
        then join $ gzipWithQ go x y
        else mempty

mkQQ :: (Monoid r, Typeable a, Typeable b) => (a -> b -> r) -> GenericQ (GenericQ r)
mkQQ f a1 a2 =
  case (cast a1, cast a2) of
    (Just x, Just y) -> f x y
    _ -> mempty


bleck :: [(RdrName, RdrName)]
bleck = gzipQ (mkQQ $ \x y -> pure (x, y)) test test2


