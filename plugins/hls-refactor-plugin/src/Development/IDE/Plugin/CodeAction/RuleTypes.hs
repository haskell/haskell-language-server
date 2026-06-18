module Development.IDE.Plugin.CodeAction.RuleTypes
    (PackageExports(..)
    ,IdentInfo(..)
    ) where

import           Control.DeepSeq                (NFData)
import           Data.Hashable                  (Hashable)
import           Development.IDE.Types.Exports
import           Development.IDE.Types.HscEnvEq (HscEnvEq)
import           GHC.Generics                   (Generic)
import           Ide.Types                      (InputClass (NoFile), RuleInput,
                                                 RuleResult)

-- Rule type for caching Package Exports
type instance RuleResult PackageExports = ExportsMap
type instance RuleInput PackageExports = NoFile

newtype PackageExports = PackageExports HscEnvEq
    deriving (Eq, Show, Generic)

instance Hashable PackageExports
instance NFData   PackageExports
