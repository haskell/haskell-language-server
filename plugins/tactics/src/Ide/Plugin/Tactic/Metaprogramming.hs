{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Tactic.Metaprogramming where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Directory
import           System.FilePath ((<.>), (</>))
import Control.Monad.Extra (unlessM)


configDir :: IO FilePath
configDir = do
  dir <- getXdgDirectory XdgConfig "hls-tactics-plugin"
  unlessM (doesDirectoryExist dir) $ createDirectory dir
  let scratch = dir </> scratchName
  unlessM (doesFileExist scratch) $ T.writeFile scratch ""
  pure dir


scratchName :: FilePath
scratchName = "scratch" <.> "htp"


getScratchContents :: IO T.Text
getScratchContents = do
  dir <- configDir
  T.readFile $ dir </> scratchName


getKnownFiles :: IO [FilePath]
getKnownFiles = do
  dir <- configDir
  fmap (dir </>) . filter (/= scratchName) <$> listDirectory dir

