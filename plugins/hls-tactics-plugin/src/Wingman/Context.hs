module Wingman.Context where

import           Bag
import           Control.Arrow
import           Control.Monad.Reader
import           Data.Coerce (coerce)
import           Data.Foldable.Extra (allM)
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat
import           GhcPlugins (ExternalPackageState (eps_inst_env), piResultTys, eps_fam_inst_env)
import           InstEnv (lookupInstEnv, InstEnvs(..), is_dfun)
import           OccName
import           TcRnTypes
import           TcType (tcSplitTyConApp, tcSplitPhiTy)
import           TysPrim (alphaTys)
import           Wingman.GHC (normalizeType)
import           Wingman.Judgements.Theta
import           Wingman.Types


mkContext
    :: Config
    -> [(OccName, CType)]
    -> TcGblEnv
    -> ExternalPackageState
    -> KnownThings
    -> [Evidence]
    -> Context
mkContext cfg locals tcg eps kt ev = fix $ \ctx ->
  Context
    { ctxDefiningFuncs
        = fmap (second $ coerce $ normalizeType ctx) locals
    , ctxModuleFuncs
        = fmap (second (coerce $ normalizeType ctx) . splitId)
        . mappend (locallyDefinedMethods tcg)
        . (getFunBindId =<<)
        . fmap unLoc
        . bagToList
        $ tcg_binds tcg
    , ctxConfig = cfg
    , ctxFamInstEnvs =
        (eps_fam_inst_env eps, tcg_fam_inst_env tcg)
    , ctxInstEnvs =
        InstEnvs
          (eps_inst_env eps)
          (tcg_inst_env tcg)
          (tcVisibleOrphanMods tcg)
    , ctxKnownThings = kt
    , ctxTheta = evidenceToThetaType ev
    }


locallyDefinedMethods :: TcGblEnv -> [Id]
locallyDefinedMethods
  = foldMap classMethods
  . mapMaybe tyConClass_maybe
  . tcg_tcs



splitId :: Id -> (OccName, CType)
splitId = occName &&& CType . idType


getFunBindId :: HsBindLR GhcTc GhcTc -> [Id]
getFunBindId (AbsBinds _ _ _ abes _ _ _)
  = abes >>= \case
      ABE _ poly _ _ _ -> pure poly
      _                -> []
getFunBindId _ = []


getCurrentDefinitions :: MonadReader Context m => m [(OccName, CType)]
getCurrentDefinitions = asks ctxDefiningFuncs


------------------------------------------------------------------------------
-- | Extract something from 'KnownThings'.
getKnownThing :: MonadReader Context m => (KnownThings -> a) -> m a
getKnownThing f = asks $ f . ctxKnownThings


------------------------------------------------------------------------------
-- | Like 'getInstance', but uses a class from the 'KnownThings'.
getKnownInstance :: MonadReader Context m => (KnownThings -> Class) -> [Type] -> m (Maybe (Class, PredType))
getKnownInstance f tys = do
  cls <- getKnownThing f
  getInstance cls tys


------------------------------------------------------------------------------
-- | Determine if there is an instance that exists for the given 'Class' at the
-- specified types. Deeply checks contexts to ensure the instance is actually
-- real.
--
-- If so, this returns a 'PredType' that corresponds to the type of the
-- dictionary.
getInstance :: MonadReader Context m => Class -> [Type] -> m (Maybe (Class, PredType))
getInstance cls tys = do
  env <- asks ctxInstEnvs
  let (mres, _, _) = lookupInstEnv False env cls tys
  case mres of
    ((inst, mapps) : _) -> do
      -- Get the instantiated type of the dictionary
      let df = piResultTys (idType $ is_dfun inst) $ zipWith fromMaybe alphaTys mapps
      -- pull off its resulting arguments
      let (theta, df') = tcSplitPhiTy df
      allM hasClassInstance theta >>= \case
        True -> pure $ Just (cls, df')
        False -> pure Nothing
    _ -> pure Nothing


------------------------------------------------------------------------------
-- | Like 'getInstance', but only returns whether or not it succeeded. Can fail
-- fast, and uses a cached Theta from the context.
hasClassInstance :: MonadReader Context m => PredType -> m Bool
hasClassInstance predty = do
  theta <- asks ctxTheta
  case S.member (CType predty) theta of
    True -> pure True
    False -> do
      let (con, apps) = tcSplitTyConApp predty
      case tyConClass_maybe con of
        Nothing -> pure False
        Just cls -> fmap isJust $ getInstance cls apps

