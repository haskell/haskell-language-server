module Wingman.Context where

import Bag
import Control.Arrow
import Control.Monad.Reader
import Development.IDE.GHC.Compat
import OccName
import TcRnTypes
import Wingman.FeatureSet (FeatureSet)
import Wingman.Types


mkContext :: FeatureSet -> [(OccName, CType)] -> TcGblEnv -> Context
mkContext features locals tcg = Context
  { ctxDefiningFuncs = locals
  , ctxModuleFuncs = fmap splitId
                   . (getFunBindId =<<)
                   . fmap unLoc
                   . bagToList
                   $ tcg_binds tcg
  , ctxFeatureSet = features
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

