{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module Ide.Plugin.Brittany where

import           Control.Exception           (bracket_)
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe   (MaybeT, runMaybeT)
import           Data.Maybe                  (mapMaybe, maybeToList)
import           Data.Semigroup
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Development.IDE             hiding (pluginHandlers)
import           Development.IDE.GHC.Compat  (ModSummary (ms_hspp_opts), topDir)
import qualified DynFlags                    as D
import qualified EnumSet                     as S
import           GHC.LanguageExtensions.Type
import           Ide.PluginUtils
import           Ide.Types
import           Language.Haskell.Brittany
import           Language.LSP.Types          as J
import qualified Language.LSP.Types.Lens     as J
import           System.Environment          (setEnv, unsetEnv)
import           System.FilePath

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginHandlers = mkFormattingHandlers provider
  }

-- | Formatter provider of Brittany.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: FormattingHandler IdeState
provider ide typ contents nfp opts = liftIO $ do
    confFile <- getConfFile nfp
    let (range, selectedContents) = case typ of
          FormatText    -> (fullRange contents, contents)
          FormatRange r -> (normalize r, extractRange r contents)
    modsum <- fmap msrModSummary $ runAction "brittany" ide $ use_ GetModSummaryWithoutTimestamps nfp
    let dflags = ms_hspp_opts modsum
    let withRuntimeLibdir = bracket_ (setEnv key $ topDir dflags) (unsetEnv key)
          where key = "GHC_EXACTPRINT_GHC_LIBDIR"
    res <- withRuntimeLibdir $ formatText dflags confFile opts selectedContents
    case res of
      Left err -> return $ Left $ responseError (T.pack $ "brittanyCmd: " ++ unlines (map showErr err))
      Right newText -> return $ Right $ J.List [TextEdit range newText]

-- | Primitive to format text with the given option.
-- May not throw exceptions but return a Left value.
-- Errors may be presented to the user.
formatText
  :: MonadIO m
  => D.DynFlags
  -> Maybe FilePath -- ^ Path to configs. If Nothing, default configs will be used.
  -> FormattingOptions -- ^ Options for the formatter such as indentation.
  -> Text -- ^ Text to format
  -> m (Either [BrittanyError] Text) -- ^ Either formatted Text or a error from Brittany.
formatText df confFile opts text =
  liftIO $ runBrittany tabSize df confFile text
  where tabSize = opts ^. J.tabSize

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
            -> D.DynFlags
            -> Maybe FilePath   -- ^ local config file
            -> Text             -- ^ text to format
            -> IO (Either [BrittanyError] Text)
runBrittany tabSize df confPath text = do
  let cfg = mempty
              { _conf_layout =
                  mempty { _lconfig_indentAmount = opt (Last tabSize)
                         }
              , _conf_forward =
                  (mempty :: CForwardOptions Option)
                    { _options_ghc = opt (getExtensions df)
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

showExtension :: Extension -> Maybe String
showExtension Cpp              = Just "-XCPP"
-- Brittany chokes on parsing extensions that produce warnings
showExtension DatatypeContexts = Nothing
showExtension RecordPuns       = Just "-XNamedFieldPuns"
showExtension other            = Just $ "-X" ++ show other

getExtensions :: D.DynFlags -> [String]
getExtensions = mapMaybe showExtension . S.toList . D.extensionFlags
