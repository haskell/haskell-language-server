{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Floskell
  ( descriptor
  , provider
  ) where

import           Control.Monad.Except        (throwError)
import           Control.Monad.IO.Class
import           Data.List                   (find)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL
import           Development.IDE             hiding (pluginHandlers)
import           Floskell
import           Ide.Plugin.Error
import           Ide.PluginUtils
import           Ide.Types
import           Language.LSP.Protocol.Types

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId desc)
  { pluginHandlers = mkFormattingHandlers provider
  }
  where
    desc = "Provides formatting of Haskell files via floskell. Built with floskell-" <> VERSION_floskell

-- ---------------------------------------------------------------------

-- | Format provider of Floskell.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: FormattingHandler IdeState
provider _ideState _token typ contents fp _ = do
    let file = fromNormalizedFilePath fp
    config <- liftIO $ findConfigOrDefault file
    let (range, selectedContents) = case typ of
          FormatText    -> (fullRange contents, contents)
          FormatRange r -> (normalize r, extractTextInRange (extendToFullLines r) contents)
        result = reformat config (Just file) $ TL.fromStrict selectedContents
    case result of
      Left  err -> throwError $ PluginInternalError $ T.pack $ "floskellCmd: " ++ err
      Right new -> pure $ InL [TextEdit range $ TL.toStrict new]

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
      pure $ case find (\s -> styleName s == "gibiansky") styles of
        Just gibiansky -> defaultAppConfig { appStyle = gibiansky }
        Nothing        -> defaultAppConfig

-- ---------------------------------------------------------------------
