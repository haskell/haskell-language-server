{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Ide.Plugin.Fourmolu (
    descriptor,
    provider,
) where

import Control.Exception
import Data.Either.Extra
import System.FilePath

import Control.Lens ((^.))
import qualified Data.Text as T
import Development.IDE as D
import qualified DynFlags as D
import qualified EnumSet as S
import GHC (DynFlags, moduleNameString)
import GHC.LanguageExtensions.Type (Extension (Cpp))
import GhcPlugins (HscEnv (hsc_dflags))
import Ide.Plugin.Formatter (responseError)
import Ide.PluginUtils (makeDiffTextEdit)
import Language.Haskell.LSP.Messages (FromServerMessage (ReqShowMessage))

import Ide.Types
import Language.Haskell.LSP.Core
import Language.Haskell.LSP.Types
import Language.Haskell.LSP.Types.Lens
import "fourmolu" Ormolu

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor
descriptor plId =
    (defaultPluginDescriptor plId)
        { pluginFormattingProvider = Just provider
        }

-- ---------------------------------------------------------------------

provider :: FormattingProvider IO
provider lf ideState typ contents fp fo = withIndefiniteProgress lf title Cancellable $ do
    ghc <- runAction "Fourmolu" ideState $ use GhcSession fp
    fileOpts <- case hsc_dflags . hscEnv <$> ghc of
        Nothing -> return []
        Just df -> convertDynFlags df

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
                            (lspPrinterOpts <> printerOpts)
                            defaultPrinterOpts
                    }

    loadConfigFile fp' >>= \case
        ConfigLoaded file opts -> do
            putStrLn $ "Loaded Fourmolu config from: " <> file
            format opts
        ConfigNotFound searchDirs -> do
            putStrLn
                . unlines
                $ ("No " ++ show configFileName ++ " found in any of:") :
                map ("  " ++) searchDirs
            format mempty
        ConfigParseError f (_, err) -> do
            sendFunc lf . ReqShowMessage $
                RequestMessage
                    { _jsonrpc = ""
                    , _id = IdString "fourmolu"
                    , _method = WindowShowMessageRequest
                    , _params =
                        ShowMessageRequestParams
                            { _xtype = MtError
                            , _message = errorMessage
                            , _actions = Nothing
                            }
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
    let pp = if null p then [] else ["-pgmF=" <> p]
        p = D.sPgm_F $ D.settings df
        pm = map (("-fplugin=" <>) . moduleNameString) $ D.pluginModNames df
        ex = map showExtension $ S.toList $ D.extensionFlags df
        showExtension = \case
            Cpp -> "-XCPP"
            x -> "-X" ++ show x
     in return $ map DynOption $ pp <> pm <> ex
