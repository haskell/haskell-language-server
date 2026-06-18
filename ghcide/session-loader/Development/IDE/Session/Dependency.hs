module Development.IDE.Session.Dependency where

import           Control.Exception.Safe as Safe
import           Data.Either.Extra
import qualified Data.Map.Strict        as Map
import           Data.Time.Clock
import           System.Directory

type DependencyInfo = Map.Map FilePath (Maybe UTCTime)

-- | Check if any dependency has been modified lately.
checkDependencyInfo :: DependencyInfo -> IO Bool
checkDependencyInfo old_di = do
  di <- getDependencyInfo (Map.keys old_di)
  return (di == old_di)

-- Note [Multi Cradle Dependency Info]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- Why do we implement our own file modification tracking here?
-- The primary reason is that the custom caching logic is quite complicated and going into shake
-- adds even more complexity and more indirection. I did try for about 5 hours to work out how to
-- use shake rules rather than IO but eventually gave up.

-- | Computes a mapping from a filepath to its latest modification date.
-- See Note [Multi Cradle Dependency Info] why we do this ourselves instead
-- of letting shake take care of it.
getDependencyInfo :: [FilePath] -> IO DependencyInfo
getDependencyInfo fs = Map.fromList <$> mapM do_one fs

  where
    safeTryIO :: IO a -> IO (Either IOException a)
    safeTryIO = Safe.try

    do_one :: FilePath -> IO (FilePath, Maybe UTCTime)
    do_one fp = (fp,) . eitherToMaybe <$> safeTryIO (getModificationTime fp)
