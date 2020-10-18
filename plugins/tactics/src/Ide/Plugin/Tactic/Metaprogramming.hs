{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Tactic.Metaprogramming where

import           Control.Monad.Extra (unlessM)
import qualified Data.Map as M
import qualified Data.Text as T
import           Ide.Plugin.Tactic.Types
import           Language.Haskell.LSP.Types
import           System.Directory
import           System.FilePath ((</>))


buildCache :: [Metaprogram] -> MetaprogramCache
buildCache mps = MetaprogramCache $ M.fromList $ do
  mp <- mps
  pure (mp_name mp, mp)


configDir :: IO FilePath
configDir = do
  dir <- getXdgDirectory XdgConfig "hls-tactics-plugin"
  unlessM (doesDirectoryExist dir) $ createDirectory dir
  pure dir


getKnownFiles :: IO [NormalizedFilePath]
getKnownFiles = do
  dir    <- configDir
  files  <- fmap (dir </>) <$> listDirectory dir
  pure $ fmap toNormalizedFilePath files


parseMetaprogram :: T.Text -> Either String Metaprogram
parseMetaprogram = const $ Left "unimplemented"

