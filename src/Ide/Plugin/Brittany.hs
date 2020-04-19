module Ide.Plugin.Brittany where

import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           Data.Coerce
import           Data.Semigroup
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Development.IDE.Types.Location
import           Language.Haskell.Brittany
import           Language.Haskell.LSP.Types            as J
import qualified Language.Haskell.LSP.Types.Lens       as J
import           Ide.Plugin.Formatter
import           Ide.Types

import           System.FilePath
import           Data.Maybe (maybeToList)

descriptor :: PluginId -> PluginDescriptor
descriptor plId = PluginDescriptor
  { pluginId = plId
  , pluginRules = mempty
  , pluginCommands = []
  , pluginCodeActionProvider = Nothing
  , pluginCodeLensProvider   = Nothing
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider      = Nothing
  , pluginSymbolsProvider    = Nothing
  , pluginFormattingProvider = Just provider
  , pluginCompletionProvider = Nothing
  }

-- | Formatter provider of Brittany.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider
  :: FormattingProvider IO
provider _ideState typ contents fp opts = do
-- text uri formatType opts = pluginGetFile "brittanyCmd: " uri $ \fp -> do
  confFile <- liftIO $ getConfFile fp
  let (range, selectedContents) = case typ of
        FormatText    -> (fullRange contents, contents)
        FormatRange r -> (normalize r, extractRange r contents)

  res <- formatText confFile opts selectedContents
  case res of
    Left err -> return $ Left $ responseError (T.pack $ "brittanyCmd: " ++ unlines (map showErr err))
    Right newText -> return $ Right $ J.List [TextEdit range newText]

-- | Primitive to format text with the given option.
-- May not throw exceptions but return a Left value.
-- Errors may be presented to the user.
formatText
  :: MonadIO m
  => Maybe FilePath -- ^ Path to configs. If Nothing, default configs will be used.
  -> FormattingOptions -- ^ Options for the formatter such as indentation.
  -> Text -- ^ Text to format
  -> m (Either [BrittanyError] Text) -- ^ Either formatted Text or a error from Brittany.
formatText confFile opts text =
  liftIO $ runBrittany tabSize confFile text
  where tabSize = opts ^. J.tabSize

-- | Extend to the line below and above to replace newline character.
normalize :: Range -> Range
normalize (Range (Position sl _) (Position el _)) =
  Range (Position sl 0) (Position (el + 1) 0)

-- | Recursively search in every directory of the given filepath for brittany.yaml.
-- If no such file has been found, return Nothing.
getConfFile :: NormalizedFilePath -> IO (Maybe FilePath)
getConfFile = findLocalConfigPath . takeDirectory . fromNormalizedFilePath

-- | Run Brittany on the given text with the given tab size and
-- a configuration path. If no configuration path is given, a
-- default configuration is chosen. The configuration may overwrite
-- tab size parameter.
--
-- Returns either a list of Brittany Errors or the reformatted text.
-- May not throw an exception.
runBrittany :: Int              -- ^ tab  size
            -> Maybe FilePath   -- ^ local config file
            -> Text             -- ^ text to format
            -> IO (Either [BrittanyError] Text)
runBrittany tabSize confPath text = do
  let cfg = mempty
              { _conf_layout =
                  mempty { _lconfig_indentAmount = opt (coerce tabSize)
                         }
              , _conf_forward =
                  (mempty :: CForwardOptions Option)
                    { _options_ghc = opt (runIdentity ( _options_ghc forwardOptionsSyntaxExtsEnabled))
                    }
              }

  config <- fromMaybeT (pure staticDefaultConfig) (readConfigsWithUserConfig cfg (maybeToList confPath))
  parsePrintModule config text

fromMaybeT :: Monad m => m a -> MaybeT m a -> m a
fromMaybeT def act = runMaybeT act >>= maybe def return

opt :: a -> Option a
opt = Option . Just

showErr :: BrittanyError -> String
showErr (ErrorInput s)          = s
showErr (ErrorMacroConfig  err input)
  = "Error: parse error in inline configuration: " ++ err ++ " in the string \"" ++ input ++ "\"."
showErr (ErrorUnusedComment s)  = s
showErr (LayoutWarning s)       = s
showErr (ErrorUnknownNode s _)  = s
showErr ErrorOutputCheck        = "Brittany error - invalid output"
