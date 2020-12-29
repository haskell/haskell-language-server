
module Development.IDE.Plugin.HLS.Formatter
  (
    formatting
  , rangeFormatting
  )
where

import qualified Data.Map  as Map
import qualified Data.Text as T
import           Development.IDE
import           Ide.PluginUtils
import           Ide.Types
import           Ide.Plugin.Config
import qualified Language.Haskell.LSP.Core as LSP
import           Language.Haskell.LSP.Types
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

formatting :: Map.Map PluginId (FormattingProvider IdeState IO)
           -> LSP.LspFuncs Config -> IdeState -> DocumentFormattingParams
           -> IO (Either ResponseError (List TextEdit))
formatting providers lf ideState
    (DocumentFormattingParams (TextDocumentIdentifier uri) params _mprogress)
  = doFormatting lf providers ideState FormatText uri params

-- ---------------------------------------------------------------------

rangeFormatting :: Map.Map PluginId (FormattingProvider IdeState IO)
                -> LSP.LspFuncs Config -> IdeState -> DocumentRangeFormattingParams
                -> IO (Either ResponseError (List TextEdit))
rangeFormatting providers lf ideState
    (DocumentRangeFormattingParams (TextDocumentIdentifier uri) range params _mprogress)
  = doFormatting lf providers ideState (FormatRange range) uri params

-- ---------------------------------------------------------------------

doFormatting :: LSP.LspFuncs Config -> Map.Map PluginId (FormattingProvider IdeState IO)
             -> IdeState -> FormattingType -> Uri -> FormattingOptions
             -> IO (Either ResponseError (List TextEdit))
doFormatting lf providers ideState ft uri params = do
  mc <- LSP.config lf
  let mf = maybe "none" formattingProvider mc
  case Map.lookup (PluginId mf) providers of
      Just provider ->
        case uriToFilePath uri of
          Just (toNormalizedFilePath -> fp) -> do
            (_, mb_contents) <- runAction "Formatter" ideState $ getFileContents fp
            case mb_contents of
              Just contents -> do
                  logDebug (ideLogger ideState) $ T.pack $
                      "Formatter.doFormatting: contents=" ++ show contents -- AZ
                  provider lf ideState ft contents fp params
              Nothing -> return $ Left $ responseError $ T.pack $ "Formatter plugin: could not get file contents for " ++ show uri
          Nothing -> return $ Left $ responseError $ T.pack $ "Formatter plugin: uriToFilePath failed for: " ++ show uri
      Nothing -> return $ Left $ responseError $ mconcat
        [ "Formatter plugin: no formatter found for:["
        , mf
        , "]"
        , if mf == "brittany"
          then T.unlines
            [ "\nThe haskell-language-server must be compiled with the agpl flag to provide Brittany."
            , "Stack users add 'agpl: true' in the flags section of the 'stack.yaml' file."
            , "The 'haskell-language-server.cabal' file already has this flag enabled by default."
            , "For more information see: https://github.com/haskell/haskell-language-server/issues/269"
            ]
          else ""
        ]

