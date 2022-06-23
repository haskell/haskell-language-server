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
import           Wingman.Types

#if __GLASGOW_HASKELL__ >= 900
import GHC.Tc.Utils.TcType
#endif


mkContext
    :: Config
    -> Context
mkContext = Context


splitId :: Id -> (OccName, CType)
splitId = occName &&& CType . idType
