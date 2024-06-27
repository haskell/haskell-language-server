module Development.IDE.Types.HscEnvEq
(   HscEnvEq,
    hscEnv, newHscEnvEq,
    updateHscEnvEq,
    envPackageExports,
    envVisibleModuleNames,
) where


import           Control.Concurrent.Async        (Async, async, waitCatch)
import           Control.Concurrent.Strict       (modifyVar, newVar)
import           Control.DeepSeq                 (force, rwhnf)
import           Control.Exception               (evaluate, mask, throwIO)
import           Control.Monad.Extra             (eitherM, join, mapMaybeM)
import           Data.Either                     (fromRight)
import           Data.Unique                     (Unique)
import qualified Data.Unique                     as Unique
import           Development.IDE.GHC.Compat      hiding (newUnique)
import qualified Development.IDE.GHC.Compat.Util as Maybes
import           Development.IDE.GHC.Error       (catchSrcErrors)
import           Development.IDE.GHC.Util        (lookupPackageConfig)
import           Development.IDE.Graph.Classes
import           Development.IDE.Types.Exports   (ExportsMap, createExportsMap)
import           OpenTelemetry.Eventlog          (withSpan)

-- | An 'HscEnv' with equality. Two values are considered equal
--   if they are created with the same call to 'newHscEnvEq' or
--   'updateHscEnvEq'.
data HscEnvEq = HscEnvEq
    { envUnique             :: !Unique
    , hscEnv                :: !HscEnv
    , envPackageExports     :: IO ExportsMap
    , envVisibleModuleNames :: IO (Maybe [ModuleName])
        -- ^ 'listVisibleModuleNames' is a pure function,
        -- but it could panic due to a ghc bug: https://github.com/haskell/haskell-language-server/issues/1365
        -- So it's wrapped in IO here for error handling
        -- If Nothing, 'listVisibleModuleNames' panic
    }

updateHscEnvEq :: HscEnvEq -> HscEnv -> IO HscEnvEq
updateHscEnvEq oldHscEnvEq newHscEnv = do
  let update newUnique = oldHscEnvEq { envUnique = newUnique, hscEnv = newHscEnv }
  update <$> Unique.newUnique

-- | Wrap an 'HscEnv' into an 'HscEnvEq'.
newHscEnvEq :: HscEnv -> IO HscEnvEq
newHscEnvEq hscEnv = do
    let dflags = hsc_dflags hscEnv

    envUnique <- Unique.newUnique

    -- it's very important to delay the package exports computation
    envPackageExports <- onceAsync $ withSpan "Package Exports" $ \_sp -> do
        -- compute the package imports
        let pkgst   = unitState hscEnv
            depends = explicitUnits pkgst
            modules =
                [ m
                | d        <- depends
                , Just pkg <- [lookupPackageConfig d hscEnv]
                , (modName, maybeOtherPkgMod) <- unitExposedModules pkg
                , let m = case maybeOtherPkgMod of
                        -- When module is re-exported from another package,
                        -- the origin module is represented by value in Just
                        Just otherPkgMod -> otherPkgMod
                        Nothing          -> mkModule (mkUnit pkg) modName
                ]

            doOne m = do
                modIface <- initIfaceLoad hscEnv $
                    loadInterface "" m (ImportByUser NotBoot)
                return $ case modIface of
                    Maybes.Failed    _r -> Nothing
                    Maybes.Succeeded mi -> Just mi
        modIfaces <- mapMaybeM doOne modules
        return $ createExportsMap modIfaces

    -- similar to envPackageExports, evaluated lazily
    envVisibleModuleNames <- onceAsync $
      fromRight Nothing
        <$> catchSrcErrors
          dflags
          "listVisibleModuleNames"
          (evaluate . force . Just $ listVisibleModuleNames hscEnv)

    return HscEnvEq{..}

instance Show HscEnvEq where
  show HscEnvEq{envUnique} = "HscEnvEq " ++ show (Unique.hashUnique envUnique)

instance Eq HscEnvEq where
  a == b = envUnique a == envUnique b

instance NFData HscEnvEq where
  rnf (HscEnvEq a b _ _) =
      -- deliberately skip the package exports map and visible module names
      rnf (Unique.hashUnique a) `seq` rwhnf b

instance Hashable HscEnvEq where
  hashWithSalt s = hashWithSalt s . envUnique

-- | Given an action, produce a wrapped action that runs at most once.
--   The action is run in an async so it won't be killed by async exceptions
--   If the function raises an exception, the same exception will be reraised each time.
onceAsync :: IO a -> IO (IO a)
onceAsync act = do
    var <- newVar OncePending
    let run as = eitherM throwIO pure (waitCatch as)
    pure $ mask $ \unmask -> join $ modifyVar var $ \v -> case v of
        OnceRunning x -> pure (v, unmask $ run x)
        OncePending -> do
            x <- async (unmask act)
            pure (OnceRunning x, unmask $ run x)

data Once a = OncePending | OnceRunning (Async a)
