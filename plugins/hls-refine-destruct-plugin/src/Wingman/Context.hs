module Wingman.Context where

import Control.Arrow
import Development.IDE.GHC.Compat
import Wingman.Types


mkContext
    :: Config
    -> Context
mkContext = Context


splitId :: Id -> (OccName, CType)
splitId = occName &&& CType . idType

