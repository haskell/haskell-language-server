{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Ide.Plugin.StylishHaskell
  ( descriptor
  , provider
  , Log
  )
where

import           Control.Monad.Except             (throwError)
import           Control.Monad.IO.Class
import           Data.ByteString                  as B
import           Data.List                        (inits, nub)
import           Data.Maybe
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Data.Yaml
import           Debug.Trace
import           Development.IDE                  hiding (getExtensions,
                                                   pluginHandlers)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.GHC.Compat       (ModSummary (ms_hspp_opts),
                                                   extensionFlags)
import qualified Development.IDE.GHC.Compat.Util  as Util
import           GHC.LanguageExtensions.Type
import           Ide.Plugin.Error                 (PluginError (PluginInternalError))
import           Ide.PluginUtils
import           Ide.Types                        hiding (Config)
import           Language.Haskell.Stylish
import           Language.LSP.Protocol.Types      as LSP
import           System.Directory

import           System.FilePath



data Log
  = LogLanguageExtensionFromDynFlags

instance Pretty Log where
  pretty = \case
    LogLanguageExtensionFromDynFlags -> "stylish-haskell uses the language extensions from DynFlags"


descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId desc)
  { pluginHandlers = mkFormattingHandlers (provider recorder)
  }
  where
    desc = "Provides formatting of Haskell files via stylish-haskell. Built with stylish-haskell-" <> VERSION_stylish_haskell

-- | Formatter provider of stylish-haskell.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: Recorder (WithPriority Log) -> FormattingHandler IdeState
provider recorder ide _token typ contents fp _opts = do
  (msrModSummary -> ms_hspp_opts -> dyn) <- runActionE "stylish-haskell" ide $ useE GetModSummary fp
  let file = fromNormalizedFilePath fp
  config <- liftIO $ loadConfigFrom file
  mergedConfig <- liftIO $ getMergedConfig dyn config
  let (range, selectedContents) = case typ of
        FormatText    -> (fullRange contents, contents)
        FormatRange r -> (normalize r, extractTextInRange (extendToFullLines r) contents)
      result = runStylishHaskell file mergedConfig selectedContents
  case result of
    Left  err -> throwError $ PluginInternalError $ T.pack $ "stylishHaskellCmd: " ++ err
    Right new -> pure $ LSP.InL [TextEdit range new]
  where
    getMergedConfig dyn config
      | Prelude.null (configLanguageExtensions config)
      = do
          logWith recorder Info LogLanguageExtensionFromDynFlags
          pure
            $ config
              { configLanguageExtensions = getExtensions dyn }
      | otherwise
      = pure config

    getExtensions = Prelude.map showExtension . Util.toList . extensionFlags

    showExtension Cpp   = "CPP"
    showExtension other = show other

-- | taken and refactored from stylish-haskell which uses getCurrentDirectory
-- https://hackage.haskell.org/package/stylish-haskell-0.14.6.0/docs/src/Language.Haskell.Stylish.Config.html#configFilePath
-- https://github.com/haskell/haskell-language-server/issues/4234#issuecomment-2191571281
ancestors :: FilePath -> [FilePath]
ancestors = Prelude.map joinPath . Prelude.reverse . Prelude.dropWhile Prelude.null . Data.List.inits . splitPath

configFileName :: String
configFileName = ".stylish-haskell.yaml"

configFilePathMT :: Verbose -> FilePath -> IO (Maybe FilePath)
configFilePathMT verbose currentDir              = do
    configPath <- getXdgDirectory XdgConfig "stylish-haskell"
    home       <- getHomeDirectory
    search verbose $
        [d </> configFileName | d <- ancestors currentDir] ++
        [configPath </> "config.yaml", home </> configFileName]

search :: Verbose -> [FilePath] -> IO (Maybe FilePath)
search _ []             = return Nothing
search verbose (f : fs) = do
    -- TODO Maybe catch an error here, dir might be unreadable
    exists <- doesFileExist f
    verbose $ f ++ if exists then " exists" else " does not exist"
    if exists then return (Just f) else search verbose fs

loadConfigMT :: Verbose -> FilePath -> IO Config
loadConfigMT verbose currentDir = do
    mbFp <- configFilePathMT verbose currentDir
    verbose $ "Loading configuration at " ++ fromMaybe "<embedded>" mbFp
    bytes <- maybe (return defaultConfigBytes) B.readFile mbFp
    case decodeEither' bytes of
        Left exception -> error $ prettyPrintParseException exception
        Right config -> do
          -- | TODO
          cabalLanguageExtensions <- pure []

          return $ config
            { configLanguageExtensions = nub $
              configLanguageExtensions config
            }
    where toStr (ext, True)  = show ext
          toStr (ext, False) = "No" ++ show ext



-- | Recursively search in every directory of the given filepath for .stylish-haskell.yaml.
-- If no such file has been found, return default config.
loadConfigFrom :: FilePath -> IO Config
loadConfigFrom file = do
  config <- loadConfigMT (makeVerbose True) (takeDirectory file)
  pure config

-- | Run stylish-haskell on the given text with the given configuration.
runStylishHaskell :: FilePath           -- ^ Location of the file being formatted. Used for error message
                  -> Config             -- ^ Configuration for stylish-haskell
                  -> Text               -- ^ Text to format
                  -> Either String Text -- ^ Either formatted Text or an error message
runStylishHaskell file config = fmap fromLines . fmt . toLines
  where
    fromLines = T.pack . unlines
    fmt = runSteps (configLanguageExtensions config) (Just file) (configSteps config)
    toLines = lines . T.unpack
