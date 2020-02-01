{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Ormolu
  (
    plugin
  )
where

#if __GLASGOW_HASKELL__ >= 806
import           Control.Exception
#if __GLASGOW_HASKELL__ >= 808
import           Control.Monad.IO.Class         ( MonadIO(..) )
#else
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO(..)
                                                )
#endif
import           Data.Char
import qualified Data.Text as T
import           GHC
import           Ormolu
import qualified DynFlags as D
import qualified EnumSet  as S
import qualified HIE.Bios as BIOS
#endif

import           Control.Monad
import           Data.List
import           Data.Maybe
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
-- New style plugin

plugin :: Plugin
plugin = Plugin ormoluRules ormoluHandlers

ormoluRules :: Rules ()
ormoluRules = mempty

ormoluHandlers :: PartialHandlers
ormoluHandlers = PartialHandlers $ \WithMessage{..} x -> return x
    { LSP.documentFormattingHandler
        = withResponse RspDocumentFormatting formatting
    , LSP.documentRangeFormattingHandler
        = withResponse RspDocumentRangeFormatting rangeFormatting
    }

formatting :: LSP.LspFuncs () -> IdeState -> DocumentFormattingParams -> IO (Either ResponseError (List TextEdit))
formatting _lf ideState (DocumentFormattingParams (TextDocumentIdentifier uri) params _mprogress)
  = doFormatting ideState FormatText uri params

rangeFormatting :: LSP.LspFuncs () -> IdeState -> DocumentRangeFormattingParams -> IO (Either ResponseError (List TextEdit))
rangeFormatting _lf ideState (DocumentRangeFormattingParams (TextDocumentIdentifier uri) range params _mprogress)
  = doFormatting ideState (FormatRange range) uri params

doFormatting :: IdeState -> FormattingType -> Uri -> FormattingOptions -> IO (Either ResponseError (List TextEdit))
doFormatting ideState ft uri params
  = case uriToFilePath uri of
    Just (toNormalizedFilePath -> fp) -> do
      (_, mb_contents) <- runAction ideState $ getFileContents fp
      case mb_contents of
        Just contents -> provider ideState ft contents fp params
        Nothing -> return $ Left $ responseError $ T.pack $ "Ormolu plugin: could not get file contents for " ++ show uri
    Nothing -> return $ Left $ responseError $ T.pack $ "Ormolu plugin: uriToFilePath failed for: " ++ show uri

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

-- | Find the cradle wide 'ComponentOptions' that apply to a 'FilePath'
lookupBiosComponentOptions :: (Monad m) => NormalizedFilePath -> m (Maybe BIOS.ComponentOptions)
lookupBiosComponentOptions _fp = do
  -- gmc <- getModuleCache
  -- return $ lookupInCache fp gmc (const Just) (Just . compOpts) Nothing
  return Nothing

-- ---------------------------------------------------------------------

provider :: forall m. (MonadIO m) => FormattingProvider m
#if __GLASGOW_HASKELL__ >= 806
provider ideState typ contents fp _ = do
  let
    exop s =
      "-X" `isPrefixOf` s || "-fplugin=" `isPrefixOf` s || "-pgmF=" `isPrefixOf` s
  opts <- lookupBiosComponentOptions fp
  let cradleOpts =
        map DynOption
          $   filter exop
          $   join
          $   maybeToList
          $   BIOS.componentOptions
          <$> opts
  let
    fromDyn :: ParsedModule -> m [DynOption]
    fromDyn pmod =
      let
        df = ms_hspp_opts $ pm_mod_summary pmod
        pp =
          let p = D.sPgm_F $ D.settings df
          in  if null p then [] else ["-pgmF=" <> p]
        pm = map (("-fplugin=" <>) . moduleNameString) $ D.pluginModNames df
        ex = map (("-X" <>) . show) $ S.toList $ D.extensionFlags df
      in
        return $ map DynOption $ pp <> pm <> ex

  m_parsed <- liftIO $ runAction ideState $ getParsedModule fp
  fileOpts <- case m_parsed of
          Nothing -> return []
          Just pm -> fromDyn pm

  let
    conf o = Config o False False True False
    fmt :: T.Text -> [DynOption] -> m (Either OrmoluException T.Text)
    fmt cont o =
      liftIO $ try @OrmoluException (ormolu (conf o) (fromNormalizedFilePath fp) $ T.unpack cont)

  case typ of
    FormatText -> ret (fullRange contents) <$> fmt contents cradleOpts
    FormatRange r ->
      let
        txt = T.lines $ extractRange r contents
        lineRange (Range (Position sl _) (Position el _)) =
          Range (Position sl 0) $ Position el $ T.length $ last txt
        hIsSpace (h : _) = T.all isSpace h
        hIsSpace _       = True
        fixS t = if hIsSpace txt && (not $ hIsSpace t) then "" : t else t
        fixE t = if T.all isSpace $ last txt then t else T.init t
        unStrip :: T.Text -> T.Text -> T.Text
        unStrip ws new =
          fixE $ T.unlines $ map (ws `T.append`) $ fixS $ T.lines new
        mStrip :: Maybe (T.Text, T.Text)
        mStrip = case txt of
          (l : _) ->
            let ws = fst $ T.span isSpace l
            in  (,) ws . T.unlines <$> traverse (T.stripPrefix ws) txt
          _ -> Nothing
        err :: m (Either ResponseError (List TextEdit))
        err = return $ Left $ responseError
          $ T.pack "You must format a whole block of code. Ormolu does not support arbitrary ranges."
        fmt' :: (T.Text, T.Text) -> m (Either ResponseError (List TextEdit))
        fmt' (ws, striped) =
          ret (lineRange r) <$> (fmap (unStrip ws) <$> fmt striped fileOpts)
      in
        maybe err fmt' mStrip
 where
  ret :: Range -> Either OrmoluException T.Text -> Either ResponseError (List TextEdit)
  ret _ (Left err) = Left
    (responseError (T.pack $ "ormoluCmd: " ++ show err) )
  ret r (Right new) = Right (List [TextEdit r new])

#else
provider _ _ _ _ = return $ IdeResultOk [] -- NOP formatter
#endif

responseError :: T.Text -> ResponseError
responseError txt = ResponseError InvalidParams txt Nothing
