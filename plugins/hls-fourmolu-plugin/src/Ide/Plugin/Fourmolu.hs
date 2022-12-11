{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE NamedFieldPuns           #-}
{-# LANGUAGE OverloadedLabels         #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeApplications         #-}

module Ide.Plugin.Fourmolu (
    descriptor,
    provider,
    LogEvent,
) where

import           Control.Exception               (IOException, try)
import           Control.Lens                    ((^.))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor                  (bimap, first)
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Development.IDE                 hiding (pluginHandlers)
import           Development.IDE.GHC.Compat      as Compat hiding (Cpp, Warning,
                                                            hang, vcat)
import qualified Development.IDE.GHC.Compat.Util as S
import           GHC.LanguageExtensions.Type     (Extension (Cpp))
import           Ide.Plugin.Fourmolu.Shim
import           Ide.Plugin.Properties
import           Ide.PluginUtils                 (makeDiffTextEdit)
import           Ide.Types
import           Language.LSP.Server             hiding (defaultConfig)
import           Language.LSP.Types              hiding (line)
import           Language.LSP.Types.Lens         (HasTabSize (tabSize))
import           Ormolu
import           System.Exit
import           System.FilePath
import           System.Process.Run              (cwd, proc)
import           System.Process.Text             (readCreateProcessWithExitCode)
import           Text.Read                       (readMaybe)

descriptor :: Recorder (WithPriority LogEvent) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId)
        { pluginHandlers = mkFormattingHandlers $ provider recorder plId
        , pluginConfigDescriptor = defaultConfigDescriptor{configCustomConfig = mkCustomConfig properties}
        }

properties :: Properties '[ 'PropertyKey "external" 'TBoolean]
properties =
    emptyProperties
        & defineBooleanProperty
            #external
            "Call out to an external \"fourmolu\" executable, rather than using the bundled library"
            False

provider :: Recorder (WithPriority LogEvent) -> PluginId -> FormattingHandler IdeState
provider recorder plId ideState typ contents fp fo = withIndefiniteProgress title Cancellable $ do
    fileOpts <-
        maybe [] (convertDynFlags . hsc_dflags . hscEnv)
            <$> liftIO (runAction "Fourmolu" ideState $ use GhcSession fp)
    useCLI <- liftIO $ runAction "Fourmolu" ideState $ usePropertyAction #external plId properties
    if useCLI
        then liftIO
            . fmap (join . first (mkError . show))
            . try @IOException
            $ do
                CLIVersionInfo{noCabal} <- do -- check Fourmolu version so that we know which flags to use
                    (exitCode, out, _err) <- readCreateProcessWithExitCode ( proc "fourmolu" ["-v"] ) ""
                    let version = do
                            guard $ exitCode == ExitSuccess
                            "fourmolu" : v : _ <- pure $ T.words out
                            traverse (readMaybe @Int . T.unpack) $ T.splitOn "." v
                    case version of
                        Just v -> pure CLIVersionInfo
                            { noCabal = v >= [0, 7]
                            }
                        Nothing -> do
                            logWith recorder Warning $ NoVersion out
                            pure CLIVersionInfo
                                { noCabal = True
                                }
                (exitCode, out, err) <- -- run Fourmolu
                    readCreateProcessWithExitCode
                        ( proc "fourmolu" $
                            map ("-o" <>) fileOpts
                                <> mwhen noCabal ["--no-cabal"]
                                <> catMaybes
                                    [ ("--start-line=" <>) . show <$> regionStartLine region
                                    , ("--end-line=" <>) . show <$> regionEndLine region
                                    ]
                        ){cwd = Just $ takeDirectory fp'}
                        contents
                case exitCode of
                    ExitSuccess -> do
                        logWith recorder Debug $ StdErr err
                        pure . Right $ makeDiffTextEdit contents out
                    ExitFailure n -> do
                        logWith recorder Info $ StdErr err
                        pure . Left . responseError $ "Fourmolu failed with exit code " <> T.pack (show n)
        else do
            let format fourmoluConfig =
                    bimap (mkError . show) (makeDiffTextEdit contents)
                        <$> try @OrmoluException (ormolu config fp' (T.unpack contents))
                  where
                    printerOpts = cfgFilePrinterOpts fourmoluConfig
                    config =
                        addFixityOverrides (cfgFileFixities fourmoluConfig) $
                        defaultConfig
                            { cfgDynOptions = map DynOption fileOpts
                            , cfgRegion = region
                            , cfgDebug = False
                            , cfgPrinterOpts =
                                fillMissingPrinterOpts
                                    (printerOpts <> lspPrinterOpts)
                                    defaultPrinterOpts
                            }
             in liftIO (loadConfigFile fp') >>= \case
                    ConfigLoaded file opts -> liftIO $ do
                        logWith recorder Info $ ConfigPath file
                        format opts
                    ConfigNotFound searchDirs -> liftIO $ do
                        logWith recorder Info $ NoConfigPath searchDirs
                        format emptyConfig
                    ConfigParseError f err -> do
                        sendNotification SWindowShowMessage $
                            ShowMessageParams
                                { _xtype = MtError
                                , _message = errorMessage
                                }
                        return . Left $ responseError errorMessage
                      where
                        errorMessage = "Failed to load " <> T.pack f <> ": " <> T.pack (showParseError err)
  where
    fp' = fromNormalizedFilePath fp
    title = "Formatting " <> T.pack (takeFileName fp')
    mkError = responseError . ("Fourmolu: " <>) . T.pack
    lspPrinterOpts = mempty{poIndentation = Just $ fromIntegral $ fo ^. tabSize}
    region = case typ of
        FormatText ->
            RegionIndices Nothing Nothing
        FormatRange (Range (Position sl _) (Position el _)) ->
            RegionIndices (Just $ fromIntegral $ sl + 1) (Just $ fromIntegral $ el + 1)

data LogEvent
    = NoVersion Text
    | ConfigPath FilePath
    | NoConfigPath [FilePath]
    | StdErr Text
    deriving (Show)

instance Pretty LogEvent where
    pretty = \case
        NoVersion t -> "Couldn't get Fourmolu version:" <> line <> indent 2 (pretty t)
        ConfigPath p -> "Loaded Fourmolu config from: " <> pretty (show p)
        NoConfigPath ps -> "No " <> pretty configFileName <> " found in any of:"
            <> line <> indent 2 (vsep (map (pretty . show) ps))
        StdErr t -> "Fourmolu stderr:" <> line <> indent 2 (pretty t)

convertDynFlags :: DynFlags -> [String]
convertDynFlags df =
    let pp = ["-pgmF=" <> p | not (null p)]
        p = sPgm_F $ Compat.settings df
        pm = map (("-fplugin=" <>) . moduleNameString) $ pluginModNames df
        ex = map showExtension $ S.toList $ extensionFlags df
        showExtension = \case
            Cpp -> "-XCPP"
            x   -> "-X" ++ show x
     in pp <> pm <> ex

newtype CLIVersionInfo = CLIVersionInfo
    { noCabal :: Bool
    }

mwhen :: Monoid a => Bool -> a -> a
mwhen b x = if b then x else mempty
