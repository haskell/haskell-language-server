module Wingman.Context where

import Bag
import Control.Arrow
import Control.Monad.Reader
import Development.IDE.GHC.Compat
import OccName
import TcRnTypes
import Wingman.FeatureSet (FeatureSet)
import Wingman.Types
import InstEnv (InstMatch, lookupInstEnv, InstEnvs(..))
import GhcPlugins (ExternalPackageState (eps_inst_env))
import Data.Maybe (listToMaybe)


mkContext
    :: FeatureSet
    -> [(OccName, CType)]
    -> TcGblEnv
    -> ExternalPackageState
    -> KnownThings
    -> Context
mkContext features locals tcg eps kt = Context
  { ctxDefiningFuncs = locals
  , ctxModuleFuncs = fmap splitId
                   . (getFunBindId =<<)
                   . fmap unLoc
                   . bagToList
                   $ tcg_binds tcg
  , ctxFeatureSet = features
  , ctxInstEnvs =
      InstEnvs
        (eps_inst_env eps)
        (tcg_inst_env tcg)
        (tcVisibleOrphanMods tcg)
  , ctxKnownThings = kt
  }


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


getKnownThing :: MonadReader Context m => (KnownThings -> a) -> m a
getKnownThing f = asks $ f . ctxKnownThings


getKnownInstance :: MonadReader Context m => (KnownThings -> Class) -> [Type] -> m (Maybe InstMatch)
getKnownInstance f tys = do
  cls <- getKnownThing f
  getInstance cls tys


getInstance :: MonadReader Context m => Class -> [Type] -> m (Maybe InstMatch)
getInstance cls tys = do
  env <- asks ctxInstEnvs
  let (res, _, _) = lookupInstEnv False env cls tys
  pure $ listToMaybe res

