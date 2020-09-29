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


mkContext :: [(OccName, CType)] -> TcGblEnv -> Context
mkContext locals
  = Context locals
  . fmap splitId
  . (getFunBindId =<<)
  . fmap unLoc
  . bagToList
  . tcg_binds


splitId :: Id -> (OccName, CType)
splitId = occName &&& CType . idType


getFunBindId :: HsBindLR GhcTc GhcTc -> [Id]
getFunBindId (AbsBinds _ _ _ abes _ _ _)
  = abes >>= \case
      ABE _ poly _ _ _ -> pure poly
      _ -> []
getFunBindId _ = []


getCurrentDefinitions :: MonadReader Context m => m [OccName]
getCurrentDefinitions = asks $ fmap fst . ctxDefiningFuncs

getModuleHypothesis :: MonadReader Context m => m [(OccName, CType)]
getModuleHypothesis = asks ctxModuleFuncs

