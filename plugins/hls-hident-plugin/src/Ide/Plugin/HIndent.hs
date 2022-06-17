{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.HIndent
  (
  descriptor
  , provider
  )
where

import           Control.Monad.IO.Class (liftIO)
import           Data.Binary.Builder    (toLazyByteString)
import qualified Data.ByteString        as B (concat)
import qualified Data.ByteString.Lazy   as BL (toChunks)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import qualified Data.Yaml
import           Development.IDE        (IdeState, fromNormalizedFilePath)
import           HIndent                (reformat)
import           HIndent.Types          (Config, defaultConfig)
import           Ide.PluginUtils        (extractRange, fullRange, normalize,
                                         responseError)
import           Ide.Types              (FormattingHandler,
                                         FormattingType (FormatRange, FormatText),
                                         PluginDescriptor (pluginHandlers),
                                         PluginId, defaultPluginDescriptor,
                                         mkFormattingHandlers)
import           Language.LSP.Types     as J (List (List), TextEdit (TextEdit))
import           Path                   (filename, toFilePath)
import qualified Path.Find              as Path
import qualified Path.IO                as Path

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkFormattingHandlers provider
  }

provider :: FormattingHandler IdeState
provider ide typ contents fp _opts = do
  let file = fromNormalizedFilePath fp
  let (range, selectedContents) = case typ of
        FormatText    -> (fullRange contents, contents)
        FormatRange r -> (normalize r, extractRange r contents)
  let extensions = Nothing  -- Extensions should be covered by Config
  config <- liftIO getHIndentConfig
  let result = HIndent.reformat config extensions (Just file) (TE.encodeUtf8 selectedContents)
  convertToTextEdit result range
    where
      builderToText = TE.decodeUtf8 . B.concat . BL.toChunks . toLazyByteString
      convertToTextEdit result range = case result of
        Left  err -> return $ Left $ responseError $ T.pack $ "hident: " ++ err
        Right new -> return $ Right $ J.List [TextEdit range (builderToText new)]

-- Copied from
-- https://github.com/mihaimaruseac/hindent/blob/master/src/main/Main.hs#L77
-- Can be removed with Path.Find and it dependences, once this issue got closed:
-- https://github.com/mihaimaruseac/hindent/issues/585


getHIndentConfig :: IO Config
getHIndentConfig = do
  cur <- Path.getCurrentDir
  homeDir <- Path.getHomeDir
  mfile <-
    Path.findFileUp cur ((== ".hindent.yaml") . toFilePath . filename) (Just homeDir)
  case mfile of
    Nothing -> return defaultConfig
    Just file -> do
      result <- Data.Yaml.decodeFileEither (toFilePath file)
      case result of
        Left e       -> error (show e)
        Right config -> return config
