{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Formatter
  (
    formatterPlugin
  , FormattingType(..)
  , FormattingProvider
  , responseError
  )
where

import qualified Data.Text as T
import           Development.IDE.Core.FileStore
import           Development.IDE.Core.Rules
import           Development.IDE.LSP.Server
import           Development.IDE.Plugin
import           Development.IDE.Types.Diagnostics as D
import           Development.IDE.Types.Location
import           Development.Shake hiding ( Diagnostic )
import qualified Language.Haskell.LSP.Core as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

formatterPlugin :: FormattingProvider IO -> Plugin
formatterPlugin provider = Plugin rules (handlers provider)

-- ---------------------------------------------------------------------
-- New style plugin

rules :: Rules ()
rules = mempty

handlers :: FormattingProvider IO -> PartialHandlers
handlers provider = PartialHandlers $ \WithMessage{..} x -> return x
    { LSP.documentFormattingHandler
        = withResponse RspDocumentFormatting (formatting provider)
    , LSP.documentRangeFormattingHandler
        = withResponse RspDocumentRangeFormatting (rangeFormatting provider)
    }

-- ---------------------------------------------------------------------

formatting :: FormattingProvider IO
           -> LSP.LspFuncs () -> IdeState -> DocumentFormattingParams
           -> IO (Either ResponseError (List TextEdit))
formatting provider _lf ideState
    (DocumentFormattingParams (TextDocumentIdentifier uri) params _mprogress)
  = doFormatting provider ideState FormatText uri params

-- ---------------------------------------------------------------------

rangeFormatting :: FormattingProvider IO
                -> LSP.LspFuncs () -> IdeState -> DocumentRangeFormattingParams
                -> IO (Either ResponseError (List TextEdit))
rangeFormatting provider _lf ideState
    (DocumentRangeFormattingParams (TextDocumentIdentifier uri) range params _mprogress)
  = doFormatting provider ideState (FormatRange range) uri params

-- ---------------------------------------------------------------------

doFormatting :: FormattingProvider IO
             -> IdeState -> FormattingType -> Uri -> FormattingOptions
             -> IO (Either ResponseError (List TextEdit))
doFormatting provider ideState ft uri params
  = case uriToFilePath uri of
    Just (toNormalizedFilePath -> fp) -> do
      (_, mb_contents) <- runAction ideState $ getFileContents fp
      case mb_contents of
        Just contents -> provider ideState ft contents fp params
        Nothing -> return $ Left $ responseError $ T.pack $ "Formatter plugin: could not get file contents for " ++ show uri
    Nothing -> return $ Left $ responseError $ T.pack $ "Formatter plugin: uriToFilePath failed for: " ++ show uri

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
