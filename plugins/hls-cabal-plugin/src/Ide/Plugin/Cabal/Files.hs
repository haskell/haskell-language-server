module Ide.Plugin.Cabal.Files where

import           Control.Monad                          (filterM)
import           Control.Monad.IO.Class                 (MonadIO, liftIO)
import           Control.Monad.Trans.Except
import           Data.ByteString                        (ByteString)
import qualified Data.ByteString.Char8                  as B
import qualified Data.Text                              as T
import           Distribution.PackageDescription.Quirks (patchQuirks)
import           Distribution.Simple.Utils              (safeHead)
import           Ide.Plugin.Cabal.Orphans               ()
import           Ide.Plugin.Error
import           System.Directory                       (doesFileExist,
                                                         listDirectory)
import           System.FilePath

{- | Given a path to a haskell file, returns the closest cabal file.
  If a package.yaml is present in same directory as the .cabal file, returns nothing,
  because adding a dependency to a generated cabal file will break propagation of changes
  from package.yaml to cabal files in stack projects.
  If cabal file wasn't found, returns Nothing.
-}
findResponsibleCabalFile :: FilePath -> IO (Maybe FilePath)
findResponsibleCabalFile haskellFilePath = do
  let dirPath = dropFileName haskellFilePath
      allDirPaths = reverse $ scanl1 (</>) (splitPath dirPath) -- sorted from most to least specific
  go allDirPaths
 where
  go [] = pure Nothing
  go (path : ps) = do
    objects <- listDirectory path
    let objectsWithPaths = map (\obj -> path <> obj) objects
        objectsCabalExtension = filter (\c -> takeExtension c == ".cabal") objectsWithPaths
    cabalFiles <- filterM (\c -> doesFileExist c) objectsCabalExtension
    case safeHead cabalFiles of
      Nothing -> go ps
      Just cabalFile -> guardAgainstHpack path cabalFile
       where
        guardAgainstHpack :: FilePath -> FilePath -> IO (Maybe FilePath)
        guardAgainstHpack path cabalFile = do
          exists <- doesFileExist $ path </> "package.yaml"
          if exists then pure Nothing else pure $ Just cabalFile

{- | Gives a cabal file's contents or throws error.

  Inspired by @readCabalFile@ in cabal-add, Distribution.Client.Main

  This is a fallback option!
  Use only if the `GetFileContents` fails.
-}
readCabalFile :: (MonadIO m) => FilePath -> ExceptT PluginError m ByteString
readCabalFile fileName = do
  cabalFileExists <- liftIO $ doesFileExist fileName
  if cabalFileExists
    then snd . patchQuirks <$> liftIO (B.readFile fileName)
    else throwE $ PluginInternalError $ T.pack ("Failed to read cabal file at " <> fileName)
