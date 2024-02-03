{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Ide.Plugin.StylishHaskell
  ( descriptor
  , provider
  )
where

import           Control.Monad.Except             (throwError)
import           Control.Monad.IO.Class
import           Data.Text                        (Text)
import qualified Data.Text                        as T
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

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId desc)
  { pluginHandlers = mkFormattingHandlers provider
  }
  where
    desc = "Provides formatting of Haskell files via stylish-haskell. Built with stylish-haskell-" <> VERSION_stylish_haskell

-- | Formatter provider of stylish-haskell.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: FormattingHandler IdeState
provider ide _token typ contents fp _opts = do
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
      | null (configLanguageExtensions config)
      = do
          logInfo (ideLogger ide) "stylish-haskell uses the language extensions from DynFlags"
          pure
            $ config
              { configLanguageExtensions = getExtensions dyn }
      | otherwise
      = pure config

    getExtensions = map showExtension . Util.toList . extensionFlags

    showExtension Cpp   = "CPP"
    showExtension other = show other

-- | Recursively search in every directory of the given filepath for .stylish-haskell.yaml.
-- If no such file has been found, return default config.
loadConfigFrom :: FilePath -> IO Config
loadConfigFrom file = do
  currDir <- getCurrentDirectory
  setCurrentDirectory (takeDirectory file)
  config <- loadConfig (makeVerbose False) Nothing
  setCurrentDirectory currDir
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
