{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Formatter
  (
    formatting
  , rangeFormatting
  , noneProvider
  , responseError
  , extractRange
  , fullRange
  )
where

import qualified Data.Map  as Map
import qualified Data.Text as T
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.Rules
-- import           Development.IDE.LSP.Server
-- import           Development.IDE.Plugin
import           Development.IDE.Types.Diagnostics as D
import           Development.IDE.Types.Location
-- import           Development.Shake hiding ( Diagnostic )
import           Ide.Types
import           Ide.Plugin.Config
import qualified Language.Haskell.LSP.Core as LSP
-- import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

formatting :: Map.Map PluginId (FormattingProvider IO)
           -> LSP.LspFuncs Config -> IdeState -> DocumentFormattingParams
           -> IO (Either ResponseError (List TextEdit))
formatting providers lf ideState
    (DocumentFormattingParams (TextDocumentIdentifier uri) params _mprogress)
  = doFormatting lf providers ideState FormatText uri params

-- ---------------------------------------------------------------------

rangeFormatting :: Map.Map PluginId (FormattingProvider IO)
                -> LSP.LspFuncs Config -> IdeState -> DocumentRangeFormattingParams
                -> IO (Either ResponseError (List TextEdit))
rangeFormatting providers lf ideState
    (DocumentRangeFormattingParams (TextDocumentIdentifier uri) range params _mprogress)
  = doFormatting lf providers ideState (FormatRange range) uri params

-- ---------------------------------------------------------------------

doFormatting :: LSP.LspFuncs Config -> Map.Map PluginId (FormattingProvider IO)
             -> IdeState -> FormattingType -> Uri -> FormattingOptions
             -> IO (Either ResponseError (List TextEdit))
doFormatting lf providers ideState ft uri params = do
  mc <- LSP.config lf
  let mf = maybe "none" formattingProvider mc
  case Map.lookup (PluginId mf) providers of
      Just provider ->
        case uriToFilePath uri of
          Just (toNormalizedFilePath -> fp) -> do
            (_, mb_contents) <- runAction ideState $ getFileContents fp
            case mb_contents of
              Just contents -> provider ideState ft contents fp params
              Nothing -> return $ Left $ responseError $ T.pack $ "Formatter plugin: could not get file contents for " ++ show uri
          Nothing -> return $ Left $ responseError $ T.pack $ "Formatter plugin: uriToFilePath failed for: " ++ show uri
      Nothing -> return $ Left $ responseError $ T.pack $ "Formatter plugin: no formatter found for:[" ++ T.unpack mf ++ "]"

-- ---------------------------------------------------------------------

noneProvider :: FormattingProvider IO
noneProvider _ _ _ _ _ = return $ Right (List [])

-- ---------------------------------------------------------------------

responseError :: T.Text -> ResponseError
responseError txt = ResponseError InvalidParams txt Nothing

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
