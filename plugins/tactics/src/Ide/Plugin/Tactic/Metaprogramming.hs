{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Tactic.Metaprogramming where

import           Control.Monad
import           Control.Monad.Extra (unlessM)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (UTCTime)
import           Data.Traversable
import           Ide.Plugin.Tactic.Types
import           System.Directory
import           System.FilePath ((</>))


buildCache :: Map FilePath (UTCTime, T.Text) -> MetaprogramCache
buildCache files = MetaprogramCache $ M.fromList $ do
  (fp, (mtime, contents))  <- M.toList files
  case parseMetaprogram fp mtime contents of
    Left err -> do
      traceM $ "WARNING: unable to parse " <> fp <> ": " <> err
      mzero
    Right mp -> pure (fp, mp)


-- updateCache :: Map FilePath (UTCTime, T.Text) -> MetaprogramCache  -> MetaprogramCache
-- updateCache


configDir :: IO FilePath
configDir = do
  dir <- getXdgDirectory XdgConfig "hls-tactics-plugin"
  unlessM (doesDirectoryExist dir) $ createDirectory dir
  pure dir


getKnownFiles :: IO (Map FilePath UTCTime)
getKnownFiles = do
  dir    <- configDir
  files  <- fmap (dir </>) <$> listDirectory dir
  mtimes <- for files getModificationTime
  pure $ M.fromList $ zip files mtimes


parseMetaprogram :: FilePath -> UTCTime -> T.Text -> Either String Metaprogram
parseMetaprogram fp mtime = const $ Left "unimplemented"

