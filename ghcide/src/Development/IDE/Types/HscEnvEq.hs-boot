module Development.IDE.Types.HscEnvEq
(   HscEnvEq,
    hscEnv,
    hscEnvWithImportPaths,
    updateHscEnvEq,
    envImportPaths,
    deps
) where

import           Data.Set                      (Set)
import           Data.Unique                   (Unique)
import           Development.IDE.GHC.Compat
import           Development.IDE.Graph.Classes
import           Development.IDE.Types.Exports (ExportsMap)

-- | An 'HscEnv' with equality. Two values are considered equal
--   if they are created with the same call to 'newHscEnvEq'.
data HscEnvEq = HscEnvEq
    { envUnique             :: !Unique
    , hscEnv                :: !HscEnv
    , deps                  :: [(UnitId, DynFlags)]
               -- ^ In memory components for this HscEnv
               -- This is only used at the moment for the import dirs in
               -- the DynFlags
    , envImportPaths        :: Maybe (Set FilePath)
        -- ^ If Just, import dirs originally configured in this env
        --   If Nothing, the env import dirs are unaltered
    , envPackageExports     :: IO ExportsMap
    , envVisibleModuleNames :: IO (Maybe [ModuleName])
        -- ^ 'listVisibleModuleNames' is a pure function,
        -- but it could panic due to a ghc bug: https://github.com/haskell/haskell-language-server/issues/1365
        -- So it's wrapped in IO here for error handling
        -- If Nothing, 'listVisibleModuleNames' panic
    }

instance Show HscEnvEq
instance Hashable HscEnvEq
instance NFData HscEnvEq

updateHscEnvEq :: HscEnvEq -> HscEnv -> IO HscEnvEq

hscEnvWithImportPaths :: HscEnvEq -> HscEnv
