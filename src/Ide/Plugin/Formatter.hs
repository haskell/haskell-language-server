{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Formatter
  (
    formatterPlugins
  , FormattingType(..)
  , FormattingProvider
  , responseError
  , extractRange
  , fullRange
  )
where

import qualified Data.Map  as Map
import qualified Data.Text as T
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.Rules
import           Development.IDE.LSP.Server
import           Development.IDE.Plugin
import           Development.IDE.Types.Diagnostics as D
import           Development.IDE.Types.Location
import           Development.Shake hiding ( Diagnostic )
import           Ide.Plugin.Config
import qualified Language.Haskell.LSP.Core as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

formatterPlugins :: [(T.Text, FormattingProvider IO)] -> Plugin Config
formatterPlugins providers = Plugin rules (handlers (Map.fromList providers))

-- ---------------------------------------------------------------------
-- New style plugin

rules :: Rules ()
rules = mempty

handlers :: Map.Map T.Text (FormattingProvider IO) -> PartialHandlers Config
handlers providers = PartialHandlers $ \WithMessage{..} x -> return x
    { LSP.documentFormattingHandler
        = withResponse RspDocumentFormatting (formatting providers)
    , LSP.documentRangeFormattingHandler
        = withResponse RspDocumentRangeFormatting (rangeFormatting providers)
    }

-- handlers :: FormattingProvider IO -> T.Text -> PartialHandlers c
-- handlers provider configName = PartialHandlers $ \WithMessage{..} x -> return x
--     { LSP.documentFormattingHandler
--         = withResponse RspDocumentFormatting (formatting provider configName)
--     , LSP.documentRangeFormattingHandler
--         = withResponse RspDocumentRangeFormatting (rangeFormatting provider configName)
--     }

-- ---------------------------------------------------------------------

formatting :: Map.Map T.Text (FormattingProvider IO)
           -> LSP.LspFuncs Config -> IdeState -> DocumentFormattingParams
           -> IO (Either ResponseError (List TextEdit))
formatting providers lf ideState
    (DocumentFormattingParams (TextDocumentIdentifier uri) params _mprogress)
  = doFormatting lf providers ideState FormatText uri params

-- ---------------------------------------------------------------------

rangeFormatting :: Map.Map T.Text (FormattingProvider IO)
                -> LSP.LspFuncs Config -> IdeState -> DocumentRangeFormattingParams
                -> IO (Either ResponseError (List TextEdit))
rangeFormatting providers lf ideState
    (DocumentRangeFormattingParams (TextDocumentIdentifier uri) range params _mprogress)
  = doFormatting lf providers ideState (FormatRange range) uri params

-- ---------------------------------------------------------------------

doFormatting :: LSP.LspFuncs Config -> Map.Map T.Text (FormattingProvider IO)
             -> IdeState -> FormattingType -> Uri -> FormattingOptions
             -> IO (Either ResponseError (List TextEdit))
doFormatting lf providers ideState ft uri params = do
  mc <- LSP.config lf
  let mf = maybe "none" formattingProvider mc
  case Map.lookup mf providers of
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

-- | Format the given Text as a whole or only a @Range@ of it.
-- Range must be relative to the text to format.
-- To format the whole document, read the Text from the file and use 'FormatText'
-- as the FormattingType.
data FormattingType = FormatText
                    | FormatRange Range


-- | To format a whole document, the 'FormatText' @FormattingType@ can be used.
-- It is required to pass in the whole Document Text for that to happen, an empty text
-- and file uri, does not suffice.
type FormattingProvider m
        = IdeState
        -> FormattingType  -- ^ How much to format
        -> T.Text -- ^ Text to format
        -> NormalizedFilePath -- ^ location of the file being formatted
        -> FormattingOptions -- ^ Options for the formatter
        -> m (Either ResponseError (List TextEdit)) -- ^ Result of the formatting

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
