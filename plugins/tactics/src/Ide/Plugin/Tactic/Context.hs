{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Ide.Plugin.Tactic.Context where

import Bag
import Control.Arrow
import Control.Monad.Reader
import Development.IDE.GHC.Compat
import Ide.Plugin.Tactic.Types
import OccName
import TcRnTypes
import Ide.Plugin.Tactic.GHC (tacticsThetaTy)
import Ide.Plugin.Tactic.Machinery (methodHypothesis)
import Data.Maybe (mapMaybe)


mkContext :: [(OccName, CType)] -> TcGblEnv -> Context
mkContext locals tcg = Context
  { ctxDefiningFuncs = locals
  , ctxModuleFuncs = fmap splitId
                   . (getFunBindId =<<)
                   . fmap unLoc
                   . bagToList
                   $ tcg_binds tcg
  }


contextMethodHypothesis :: Context -> [(OccName, CType)]
contextMethodHypothesis
  = join
  . concatMap
      ( mapMaybe methodHypothesis
      . fmap unCType
      . traceIdX "tacticsTheta"
      . fmap CType
      . tacticsThetaTy
      . unCType
      . traceIdX "method hypothesis"
      -- TODO(sandy): unify the poly type with the defined type
      . snd
      )
  -- TODO(sandy): use the defining funcs to find the poly type in module funcs
  . ctxModuleFuncs


splitId :: Id -> (OccName, CType)
splitId = occName &&& CType . idType


getFunBindId :: HsBindLR GhcTc GhcTc -> [Id]
getFunBindId (AbsBinds _ _ _ abes _ _ _)
  = abes >>= \case
      ABE _ poly _ _ _ -> pure poly
      _ -> []
getFunBindId _ = []


getCurrentDefinitions :: MonadReader Context m => m [(OccName, CType)]
getCurrentDefinitions = asks $ ctxDefiningFuncs

getModuleHypothesis :: MonadReader Context m => m [(OccName, CType)]
getModuleHypothesis = asks ctxModuleFuncs

