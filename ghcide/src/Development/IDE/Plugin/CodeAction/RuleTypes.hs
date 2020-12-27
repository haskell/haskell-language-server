{-# LANGUAGE TypeFamilies #-}
module Development.IDE.Plugin.CodeAction.RuleTypes
    (PackageExports(..)
    ,IdentInfo(..)
    ) where

import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Development.IDE.GHC.Util
import Development.IDE.Types.Exports
import Development.Shake (RuleResult)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

-- Rule type for caching Package Exports
type instance RuleResult PackageExports = ExportsMap

newtype PackageExports = PackageExports HscEnvEq
    deriving (Eq, Show, Typeable, Generic)

instance Hashable PackageExports
instance NFData   PackageExports
instance Binary   PackageExports
