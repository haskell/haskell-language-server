{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Development.IDE.Graph.Internal.Paths (readDataFileHTML) where

#ifndef FILE_EMBED
import           Control.Exception    (SomeException (SomeException), catch)
import           Control.Monad        (filterM)
import           PathsHlsGraph
import           System.Directory     (doesFileExist, getCurrentDirectory)
import           System.Environment   (getExecutablePath)
import           System.FilePath      (takeDirectory, (</>))
import           System.IO.Unsafe     (unsafePerformIO)
#endif
import qualified Data.ByteString.Lazy as LBS

#ifdef FILE_EMBED
import qualified Data.ByteString      as BS
import           Data.FileEmbed

htmlDataFiles :: [(FilePath, BS.ByteString)]
htmlDataFiles =
  [
#ifdef __GHCIDE__
    ("profile.html",  $(embedFile "hls-graph/html/profile.html"))
  , ("shake.js",      $(embedFile "hls-graph/html/shake.js"))
#else
    ("profile.html",  $(embedFile "html/profile.html"))
  , ("shake.js",      $(embedFile "html/shake.js"))
#endif
  ]

readDataFileHTML :: FilePath -> IO LBS.ByteString
readDataFileHTML file = do
    case lookup file htmlDataFiles of
      Nothing -> fail $ "Could not find data file " ++ file ++ " in embedded data files!"
      Just x  -> pure (LBS.fromStrict x)

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

readDataFileHTML :: FilePath -> IO LBS.ByteString
readDataFileHTML file = LBS.readFile =<< getDataFile ("html" </> file)

#endif
