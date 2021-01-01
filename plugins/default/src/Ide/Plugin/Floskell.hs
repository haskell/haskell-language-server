{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Floskell
  (
    descriptor
  , provider
  )
where

import qualified Data.ByteString.Lazy           as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding             as T
import           Development.IDE as D
import           Floskell
import           Ide.PluginUtils
import           Ide.Types
import           Language.Haskell.LSP.Types
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginFormattingProvider = Just provider
  }

-- ---------------------------------------------------------------------

-- | Format provider of Floskell.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: FormattingProvider IdeState IO
provider _lf _ideState typ contents fp _ = do
    let file = fromNormalizedFilePath fp
    config <- findConfigOrDefault file
    let (range, selectedContents) = case typ of
          FormatText    -> (fullRange contents, contents)
          FormatRange r -> (r, extractRange r contents)
        result = reformat config (Just file) (BS.fromStrict (T.encodeUtf8 selectedContents))
    case result of
      Left  err -> return $ Left $ responseError (T.pack $  "floskellCmd: " ++ err)
      Right new -> return $ Right $ List [TextEdit range (T.decodeUtf8 (BS.toStrict new))]

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
      in return $ defaultAppConfig { appStyle = gibiansky }

-- ---------------------------------------------------------------------
