{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Floskell
  (
    plugin
  )
where

#if __GLASGOW_HASKELL__ >= 806
#if __GLASGOW_HASKELL__ >= 808
import           Control.Monad.IO.Class         ( MonadIO(..) )
#else
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO(..)
                                                )
#endif
import qualified Data.Text as T
#endif

import qualified Data.ByteString.Lazy           as BS
import qualified Data.Text.Encoding             as T
import           Development.IDE.Plugin
import           Development.IDE.Types.Diagnostics as D
import           Development.IDE.Types.Location
import           Floskell
import           Ide.Plugin.Formatter
import           Language.Haskell.LSP.Types
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------
-- New style plugin

plugin :: Plugin
plugin = formatterPlugin provider

-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------

-- | Format provider of Floskell.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: FormattingProvider IO
provider _ideState typ contents fp _ = do
    let file = fromNormalizedFilePath fp
    config <- liftIO $ findConfigOrDefault file
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
-- ---------------------------------------------------------------------
-- ---------------------------------------------------------------------


extractRange :: Range -> T.Text -> T.Text
extractRange (Range (Position sl _) (Position el _)) s = newS
  where focusLines = take (el-sl+1) $ drop sl $ T.lines s
        newS = T.unlines focusLines

-- | Gets the range that covers the entire text
fullRange :: T.Text -> Range
fullRange s = Range startPos endPos
  where startPos = Position 0 0
        endPos = Position lastLine 0
        {-
        In order to replace everything including newline characters,
        the end range should extend below the last line. From the specification:
        "If you want to specify a range that contains a line including
        the line ending character(s) then use an end position denoting
        the start of the next line"
        -}
        lastLine = length $ T.lines s

-- ---------------------------------------------------------------------
