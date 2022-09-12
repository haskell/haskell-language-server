{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Floskell
  ( descriptor
  , provider
  ) where

import           Control.Monad.IO.Class
import qualified Data.Text               as T
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Development.IDE         hiding (pluginHandlers)
import           Floskell
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Types

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkFormattingHandlers provider
  }

-- ---------------------------------------------------------------------

-- | Format provider of Floskell.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: FormattingHandler IdeState
provider _ideState typ contents fp _ = liftIO $ do
    let file = fromNormalizedFilePath fp
    config <- findConfigOrDefault file
    let (range, selectedContents) = case typ of
          FormatText    -> (fullRange contents, contents)
          FormatRange r -> (normalize r, extractRange r contents)
        result = reformat config (Just file) . TL.encodeUtf8 $ TL.fromStrict selectedContents
    case result of
      Left  err -> pure $ Left $ responseError $ T.pack $ "floskellCmd: " ++ err
      Right new -> pure $ Right $ List [TextEdit range . TL.toStrict $ TL.decodeUtf8 new]

-- | Find Floskell Config, user and system wide or provides a default style.
-- Every directory of the filepath will be searched to find a user configuration.
-- Also looks into places such as XDG_CONFIG_DIRECTORY<https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html>.
-- This function may not throw an exception and returns a default config.
findConfigOrDefault :: FilePath -> IO AppConfig
findConfigOrDefault file = do
  mbConf <- findAppConfigIn file
  case mbConf of
    Just confFile -> readAppConfig confFile
    Nothing ->
      let gibiansky = head (filter (\s -> styleName s == "gibiansky") styles)
      in pure $ defaultAppConfig { appStyle = gibiansky }

-- ---------------------------------------------------------------------
