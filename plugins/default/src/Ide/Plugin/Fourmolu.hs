{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PackageImports           #-}
{-# LANGUAGE TypeApplications         #-}

module Ide.Plugin.Fourmolu (
    descriptor,
    provider,
) where

import           Control.Exception
import           Data.Either.Extra
import           System.FilePath

import           Control.Lens                ((^.))
import qualified Data.Text                   as T
import           Development.IDE             as D hiding (pluginHandlers)
import qualified DynFlags                    as D
import qualified EnumSet                     as S
import           GHC                         (DynFlags, moduleNameString)
import           GHC.LanguageExtensions.Type (Extension (Cpp))
import           GhcPlugins                  (HscEnv (hsc_dflags))
import           Ide.PluginUtils             (makeDiffTextEdit)

import           Control.Monad.IO.Class
import           Ide.Types
import           Language.LSP.Server hiding (defaultConfig)
import           Language.LSP.Types
import           Language.LSP.Types.Lens
import           "fourmolu" Ormolu

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
    (defaultPluginDescriptor plId)
        { pluginHandlers = mkFormattingHandlers provider
        }

-- ---------------------------------------------------------------------

provider :: FormattingHandler IdeState
provider ideState typ contents fp fo = withIndefiniteProgress title Cancellable $ do
    ghc <- liftIO $ runAction "Fourmolu" ideState $ use GhcSession fp
    fileOpts <- case hsc_dflags . hscEnv <$> ghc of
        Nothing -> return []
        Just df -> liftIO $ convertDynFlags df

    let format printerOpts =
            mapLeft (responseError . ("Fourmolu: " <>) . T.pack . show)
                <$> try @OrmoluException (makeDiffTextEdit contents <$> ormolu config fp' (T.unpack contents))
          where
            config =
                defaultConfig
                    { cfgDynOptions = fileOpts
                    , cfgRegion = region
                    , cfgDebug = True
                    , cfgPrinterOpts =
                        fillMissingPrinterOpts
                            (printerOpts <> lspPrinterOpts)
                            defaultPrinterOpts
                    }

    liftIO (loadConfigFile fp') >>= \case
        ConfigLoaded file opts -> liftIO $ do
            putStrLn $ "Loaded Fourmolu config from: " <> file
            format opts
        ConfigNotFound searchDirs -> liftIO $ do
            putStrLn
                . unlines
                $ ("No " ++ show configFileName ++ " found in any of:") :
                map ("  " ++) searchDirs
            format mempty
        ConfigParseError f (_, err) -> do
            sendNotification SWindowShowMessage $
              ShowMessageParams
                  { _xtype = MtError
                  , _message = errorMessage
                  }
            return . Left $ responseError errorMessage
          where
            errorMessage = "Failed to load " <> T.pack f <> ": " <> T.pack err
  where
    fp' = fromNormalizedFilePath fp
    title = "Formatting " <> T.pack (takeFileName fp')
    lspPrinterOpts = mempty{poIndentation = Just $ fo ^. tabSize}
    region = case typ of
        FormatText ->
            RegionIndices Nothing Nothing
        FormatRange (Range (Position sl _) (Position el _)) ->
            RegionIndices (Just $ sl + 1) (Just $ el + 1)

convertDynFlags :: DynFlags -> IO [DynOption]
convertDynFlags df =
    let pp = ["-pgmF=" <> p | not (null p)]
        p = D.sPgm_F $ D.settings df
        pm = map (("-fplugin=" <>) . moduleNameString) $ D.pluginModNames df
        ex = map showExtension $ S.toList $ D.extensionFlags df
        showExtension = \case
            Cpp -> "-XCPP"
            x   -> "-X" ++ show x
     in return $ map DynOption $ pp <> pm <> ex
