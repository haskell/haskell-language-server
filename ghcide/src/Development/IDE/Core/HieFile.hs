module Development.IDE.Core.HieFile
  ( HieFileCheck(..)
  , checkHieFile
  , readHieFileFromDisk
  , HieFileLog(..)
  ) where

import Control.Exception (SomeException, displayException)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Bool (bool)

import qualified Development.IDE.GHC.Compat as Compat
import qualified Development.IDE.GHC.Compat.Util as Util
import qualified HieDb

import Development.IDE.Core.Compile (loadHieFile)
import Development.IDE.Core.Shake
import Development.IDE.GHC.Compat (HieFile)
import Development.IDE.Types.Location
import Ide.Logger
import System.Directory
import Control.Exception.Safe (tryAny)
import Control.Monad.Trans.Except (except)

data HieFileLog
  = LogLoading !NormalizedFilePath
  | LogMissing !NormalizedFilePath
  | LogLoadingFail !NormalizedFilePath !SomeException
  | LogLoadingSuccess !NormalizedFilePath
  deriving Show

instance Pretty HieFileLog where
  pretty = \case
    LogLoading path ->
      "LOADING HIE FILE FOR" <+> pretty (fromNormalizedFilePath path)
    LogMissing path ->
      "MISSING HIE FILE" <+> pretty (fromNormalizedFilePath path)
    LogLoadingFail path e ->
      nest 2 $
        vcat
          [ "FAILED LOADING HIE FILE" <+> pretty (fromNormalizedFilePath path)
          , pretty (displayException e)
          ]
    LogLoadingSuccess path ->
      "SUCCEEDED LOADING HIE FILE" <+> pretty (fromNormalizedFilePath path)

-- | The result of checkHieFile, which returns a reason why an
-- HIE file should not be indexed, or the data necessary for
-- indexing in the HieDb database.
data HieFileCheck
  = HieFileMissing
  | HieAlreadyIndexed
  | CouldNotLoadHie SomeException
  | DoIndexing Util.Fingerprint HieFile

-- | checkHieFile verifies that an HIE file exists, that it has not already
-- been indexed, and attempts to load it. This is intended to happen before
-- any indexing of HIE files in the HieDb database. In addition to returning
-- a HieFileCheck, this function also handles logging.
checkHieFile
  :: Recorder (WithPriority HieFileLog)
  -> ShakeExtras
  -> String
  -> NormalizedFilePath
  -> IO HieFileCheck
checkHieFile recorder se@ShakeExtras{withHieDb} tag hieFileLocation = do
  hieFileExists <- doesFileExist $
    fromNormalizedFilePath hieFileLocation

  bool
    logHieFileMissing
    checkExistingHieFile
    hieFileExists
  where

    -- Log that the HIE file does not exist where we expect that it should.
    logHieFileMissing :: IO HieFileCheck
    logHieFileMissing = do
      let logMissing :: HieFileLog
          logMissing = LogMissing hieFileLocation

      logWith recorder Debug logMissing
      pure HieFileMissing

    -- When we know that the HIE file exists, check that it has not already
    -- been indexed. If it hasn't, try to load it.
    checkExistingHieFile :: IO HieFileCheck
    checkExistingHieFile = do
      hieFileHash <- Util.getFileHash $
        fromNormalizedFilePath hieFileLocation

      mrow <- withHieDb $
        \hieDb -> HieDb.lookupHieFileFromHash hieDb hieFileHash

      dbHieFileLocation <-
        traverse (makeAbsolute . HieDb.hieModuleHieFile) mrow

      bool
        (tryLoadingHieFile hieFileHash)
        (pure HieAlreadyIndexed)
        (Just hieFileLocation == fmap toNormalizedFilePath' dbHieFileLocation)

    -- Attempt to load the HIE file, logging on failure
    -- (logging happens in readHieFileFromDisk).
    -- If the file loads successfully, return the data necessary
    -- for indexing it in the HieDb database.
    tryLoadingHieFile :: Util.Fingerprint -> IO HieFileCheck
    tryLoadingHieFile hieFileHash = do
      ehf <- runIdeAction tag se $
        runExceptT $
          readHieFileFromDisk
            recorder
            hieFileLocation

      pure $ case ehf of
        Left err -> CouldNotLoadHie err
        Right hf -> DoIndexing hieFileHash hf

readHieFileFromDisk
  :: Recorder (WithPriority HieFileLog)
  -> NormalizedFilePath
  -> ExceptT SomeException IdeAction Compat.HieFile
readHieFileFromDisk recorder hieLoc = do
  nc <- asks ideNc

  res <- liftIO $
    tryAny $
      loadHieFile (mkUpdater nc) (fromNormalizedFilePath hieLoc)

  case res of
    Left e ->
      liftIO $
        logWith recorder Debug $
          LogLoadingFail hieLoc e

    Right _ ->
      liftIO $
        logWith recorder Debug $
          LogLoadingSuccess hieLoc

  except res
