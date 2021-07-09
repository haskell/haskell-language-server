{-# LANGUAGE TypeFamilies #-}
module Development.IDE.Plugin.CodeAction.RuleTypes
    (PackageExports(..)
    ,IdentInfo(..)
    ) where

import           Control.DeepSeq                (NFData)
import           Data.Binary                    (Binary)
import           Data.Hashable                  (Hashable)
import           Data.Typeable                  (Typeable)
import           Development.IDE.Graph          (RuleResult)
import           Development.IDE.Types.Exports
import           Development.IDE.Types.HscEnvEq (HscEnvEq)
import           GHC.Generics                   (Generic)

-- Rule type for caching Package Exports
type instance RuleResult PackageExports = ExportsMap

newtype PackageExports = PackageExports HscEnvEq
    deriving (Eq, Show, Typeable, Generic)

instance Hashable PackageExports
instance NFData   PackageExports
instance Binary   PackageExports
