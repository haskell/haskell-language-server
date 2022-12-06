{-# LANGUAGE CPP #-}

module Wingman.Context where

import           Control.Arrow
import           Control.Monad.Reader
import           Data.Coerce (coerce)
import           Data.Foldable.Extra (allM)
import           Data.Maybe (fromMaybe, isJust, mapMaybe)
import qualified Data.Set as S
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           Wingman.GHC (normalizeType)
import           Wingman.Judgements.Theta
import           Wingman.Types

#if __GLASGOW_HASKELL__ >= 900
import GHC.Tc.Utils.TcType
#endif


mkContext
    :: Config
    -> [(OccName, CType)]
    -> TcGblEnv
    -> HscEnv
    -> ExternalPackageState
    -> [Evidence]
    -> Context
mkContext cfg locals tcg hscenv eps ev = fix $ \ctx ->
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
    , ctxTheta = evidenceToThetaType ev
    , ctx_hscEnv = hscenv
    , ctx_occEnv = tcg_rdr_env tcg
    , ctx_module = extractModule tcg
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

