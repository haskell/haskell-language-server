module Ide.Plugin.StylishHaskell
  (
    descriptor
  , provider
  )
where

import           Control.Monad.IO.Class
import           Data.Text (Text)
import qualified Data.Text as T
import           Development.IDE (IdeState)
import           Ide.PluginUtils
import           Ide.Types
import           Language.Haskell.Stylish
import           Language.Haskell.LSP.Types            as J

import           System.Directory
import           System.FilePath

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId = (defaultPluginDescriptor plId)
  { pluginFormattingProvider = Just provider
  }

-- | Formatter provider of stylish-haskell.
-- Formats the given source in either a given Range or the whole Document.
-- If the provider fails an error is returned that can be displayed to the user.
provider :: FormattingProvider IdeState IO
provider _lf _ideState typ contents fp _opts = do
  let file = fromNormalizedFilePath fp
  config <- liftIO $ loadConfigFrom file
  let (range, selectedContents) = case typ of
        FormatText    -> (fullRange contents, contents)
        FormatRange r -> (normalize r, extractRange r contents)
      result = runStylishHaskell file config selectedContents
  case result of
    Left  err -> return $ Left $ responseError $ T.pack $ "stylishHaskellCmd: " ++ err
    Right new -> return $ Right $ J.List [TextEdit range new]

-- | Recursively search in every directory of the given filepath for .stylish-haskell.yaml.
-- If no such file has been found, return default config.
loadConfigFrom :: FilePath -> IO Config
loadConfigFrom file = do
  currDir <- getCurrentDirectory
  setCurrentDirectory (takeDirectory file)
  config <- loadConfig (makeVerbose False) Nothing
  setCurrentDirectory currDir
  return config

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
