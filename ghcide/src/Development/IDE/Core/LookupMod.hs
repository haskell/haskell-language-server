module Development.IDE.Core.LookupMod (lookupMod, LookupModule) where

import           Control.Concurrent                (newEmptyMVar, putMVar,
                                                    readMVar)
import           Control.Concurrent.STM            (atomically)
import           Control.Monad.IO.Class            (MonadIO (liftIO))
import           Control.Monad.RWS                 (asks)
import           Control.Monad.Trans.Maybe         (MaybeT (MaybeT))
import qualified Data.ByteString                   as BS
import           Data.Function                     ((&))
import           Development.IDE.Core.Compile      (loadHieFile)
import           Development.IDE.Core.Shake        (HieDbWriter (HieDbWriter, indexQueue),
                                                    IdeAction,
                                                    ShakeExtras (ideNc, lspEnv),
                                                    mkUpdater)
import           Development.IDE.Core.WorkerThread (writeTaskQueue)
import           Development.IDE.GHC.Compat        (HieFile (hie_hs_src))
import           Development.IDE.GHC.Compat.Core   (ModuleName, Unit,
                                                    moduleNameSlashes)
import           Development.IDE.Types.Location    (Uri, filePathToUri',
                                                    toNormalizedFilePath')
import qualified Development.IDE.Types.Location    as LSP
import           GHC.MVar                          (MVar)
import qualified HieDb
import           Language.LSP.Server               (LanguageContextEnv (resRootPath))
import           System.Directory                  (createDirectoryIfMissing,
                                                    doesFileExist,
                                                    getPermissions,
                                                    setOwnerExecutable,
                                                    setOwnerWritable,
                                                    setPermissions)
import           System.FilePath                   (takeDirectory, (<.>), (</>))
-- | Gives a Uri for the module, given the .hie file location and the the module info
-- The Bool denotes if it is a boot module
type LookupModule m = FilePath -> ModuleName -> Unit -> Bool -> MaybeT m Uri

-- | Eventually this will lookup/generate URIs for files in dependencies, but not in the
-- project. Right now, this is just a stub.
lookupMod ::
  -- | access the database
  HieDbWriter ->
  -- | The `.hie` file we got from the database
  FilePath ->
  ModuleName ->
  Unit ->
  -- | Is this file a boot file?
  Bool ->
  MaybeT IdeAction Uri
lookupMod HieDbWriter{indexQueue} hieFile moduleName uid _boot = MaybeT $ do
  -- We need the project root directory to determine where to put
  -- the .hls directory.
  mProjectRoot <- (resRootPath =<<) <$> asks lspEnv
  case mProjectRoot of
    Nothing -> pure Nothing
    Just projectRoot -> do
      -- Database writes happen asynchronously. We use Mvar to mark
      -- completion of the database update
      completionToken <- liftIO newEmptyMVar
      -- Write out the contents of the dependency source to the
      -- .hls/dependencies directory, generate a URI for that
      -- location, and update the HieDb database with the source
      -- file location
      moduleUri <- writeAndIndexHieFile projectRoot completionToken
      -- wait for the database update to be completed.
      -- Reading the completionToken is blocked until it has
      -- a value
      liftIO $ readMVar completionToken
      pure $ Just moduleUri
  where
    writeAndIndexHieFile :: FilePath -> MVar () -> IdeAction Uri
    writeAndIndexHieFile projectRoot completionToken = do
      fileExists <- liftIO $ doesFileExist writeOutPath
      -- No need to write out the file if it already exists
      if fileExists then pure () else do
        nc <- asks ideNc
        liftIO $ do
          -- Create the directory where we will put the source
          createDirectoryIfMissing True $ takeDirectory writeOutPath
          -- Load a raw Bytestring of the source from the HIE file
          moduleSource <- hie_hs_src <$> loadHieFile (mkUpdater nc) hieFile
          -- Write the source into the .hls/dependencies directory
          BS.writeFile writeOutPath moduleSource
          fileDefaultPermissions <- getPermissions writeOutPath
          let filePermissions = fileDefaultPermissions
                              & setOwnerWritable False
                              & setOwnerExecutable False
          -- Set the source file to readonly permissions.
          setPermissions writeOutPath filePermissions
      liftIO $ atomically $
        writeTaskQueue indexQueue $ \withHieDb -> do
          withHieDb $ \db ->
            -- Add a source file to the database row for
            -- the HIE file
            HieDb.addSrcFile db hieFile writeOutPath False
          -- Mark completion of the database update.
          putMVar completionToken ()
      pure moduleUri

      where
        writeOutDir :: FilePath
        writeOutDir = projectRoot </> ".hls" </> "dependencies" </> show uid

        -- The module name is separated into directories, with the
        -- last part of the module name giving the name of the
        -- haskell file with a .hs extension
        writeOutFile :: FilePath
        writeOutFile = moduleNameSlashes moduleName <.> "hs"

        writeOutPath :: FilePath
        writeOutPath = writeOutDir </> writeOutFile

        moduleUri :: Uri
        moduleUri = LSP.fromNormalizedUri $ filePathToUri' $ toNormalizedFilePath' writeOutPath
