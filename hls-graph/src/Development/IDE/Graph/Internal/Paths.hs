{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Development.IDE.Graph.Internal.Paths (getDataFile) where

import           Paths_hls_graph

#ifndef FILE_EMBED
import           Control.Exception  (SomeException (SomeException), catch)
import           Control.Monad      (filterM)
import           System.Directory   (doesFileExist, getCurrentDirectory)
import           System.Environment (getExecutablePath)
import           System.FilePath    (takeDirectory, (</>))
import           System.IO.Unsafe   (unsafePerformIO)
#endif

#ifdef FILE_EMBED
import qualified Data.ByteString    as BS
import qualified Data.ByteString    as LBS
import           Data.FileEmbed

initDataDirectory :: IO ()
initDataDirectory = pure ()

htmlDataFiles :: [(FilePath, BS.ByteString)]
htmlDataFiles =
  [ ("profile.html",  $(embedFile "html/profile.html"))
  , ("progress.html", $(embedFile "html/progress.html"))
  , ("shake.js",      $(embedFile "html/shake.js"))
  ]

readDataFileHTML :: FilePath -> IO LBS.ByteString
readDataFileHTML file = do
    case lookup file htmlDataFiles of
      Nothing -> fail $ "Could not find data file " ++ file ++ " in embedded data files!"
      Just x  -> pure (LBS.fromStrict x)

manualDirData :: [(FilePath, BS.ByteString)]
manualDirData = $(embedDir "docs/manual")

hasManualData :: IO Bool
hasManualData = pure True

copyManualData :: FilePath -> IO ()
copyManualData dest = do
    createDirectoryRecursive dest
    forM_ manualDirData $ \(file, bs) -> do
        BS.writeFile (dest </> file) bs

#else
-- We want getDataFileName to be relative to the current directory on program startup,
-- even if we issue a change directory command. Therefore, first call caches, future ones read.

{-# NOINLINE dataDirs #-}
dataDirs :: [String]
dataDirs = unsafePerformIO $ do
    datdir <- getDataDir
    exedir <- takeDirectory <$> getExecutablePath `catch` \SomeException{} -> pure ""
    curdir <- getCurrentDirectory
    pure $ [datdir] ++ [exedir | exedir /= ""] ++ [curdir]


getDataFile :: FilePath -> IO FilePath
getDataFile file = do
    let poss = map (</> file) dataDirs
    res <- filterM doesFileExist poss
    case res of
        [] -> fail $ unlines $ ("Could not find data file " ++ file ++ ", looked in:") : map ("  " ++) poss
        x:_ -> pure x

#endif
