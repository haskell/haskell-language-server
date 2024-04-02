{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ide.Plugin.Fourmolu.Impl (pluginDesc, handlers) where

import           Control.Exception
import           Control.Lens                    ((^.))
import           Control.Monad                   (guard)
import           Control.Monad.Error.Class       (MonadError (throwError))
import           Control.Monad.IO.Class          (MonadIO (liftIO))
import           Control.Monad.Trans.Class       (MonadTrans (lift))
import           Control.Monad.Trans.Except      (ExceptT (..), runExceptT)
import           Data.Bifunctor                  (bimap)
import           Data.Maybe                      (catMaybes)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Development.IDE                 hiding (pluginHandlers)
import           Development.IDE.GHC.Compat      as Compat hiding (Cpp, Warning,
                                                            hang, vcat)
import qualified Development.IDE.GHC.Compat.Util as S
import           GHC.LanguageExtensions.Type     (Extension (Cpp))
import           Ide.Plugin.Error
import           Ide.Plugin.Fourmolu.Common
import           Ide.PluginUtils                 (makeDiffTextEdit)
import           Ide.Types
import           Language.LSP.Protocol.Lens      (HasTabSize (tabSize))
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server             hiding (defaultConfig)
import           Ormolu
import           Ormolu.Config
import           System.Exit
import           System.FilePath
import           System.Process.Run              (cwd, proc)
import           System.Process.Text             (readCreateProcessWithExitCode)
import           Text.Read                       (readMaybe)

pluginDesc :: Text
pluginDesc = pluginDescMain <> " Built with fourmolu-" <> VERSION_fourmolu

handlers :: Recorder (WithPriority LogEvent) -> PluginId -> PluginHandlers IdeState
handlers recorder plId = mkFormattingHandlers $ provider recorder plId

provider :: Recorder (WithPriority LogEvent) -> PluginId -> FormattingHandler IdeState
provider recorder plId ideState token typ contents fp fo = ExceptT $ withIndefiniteProgress title token Cancellable $ \_updater -> runExceptT $ do
    fileOpts <-
        maybe [] (convertDynFlags . hsc_dflags . hscEnv)
            <$> liftIO (runAction "Fourmolu" ideState $ use GhcSession fp)
    useCLI <- liftIO $ runAction "Fourmolu" ideState $ usePropertyAction #external plId properties
    fourmoluExePath <- fmap T.unpack $ liftIO $ runAction "Fourmolu" ideState $ usePropertyAction #path plId properties
    if useCLI
        then ExceptT . liftIO $
                handle @IOException (pure . Left . PluginInternalError . T.pack . show) $
                    runExceptT (cliHandler fourmoluExePath fileOpts)
        else do
            logWith recorder Debug $ LogCompiledInVersion VERSION_fourmolu
            FourmoluConfig{..} <-
                liftIO (loadConfigFile fp') >>= \case
                    ConfigLoaded file opts -> do
                        logWith recorder Info $ ConfigPath file
                        pure opts
                    ConfigNotFound searchDirs -> do
                        logWith recorder Info $ NoConfigPath configFileName searchDirs
                        pure emptyConfig
                    ConfigParseError f err -> do
                        lift $ sendNotification SMethod_WindowShowMessage $
                            ShowMessageParams
                                { _type_ = MessageType_Error
                                , _message = errorMessage
                                }
                        throwError $ PluginInternalError errorMessage
                      where
                        errorMessage = "Failed to load " <> T.pack f <> ": " <> T.pack (show err)

            let config =
#if MIN_VERSION_fourmolu(0,13,0)
                    refineConfig ModuleSource Nothing Nothing Nothing
#endif
                    defaultConfig
                        { cfgDynOptions = map DynOption fileOpts
                        , cfgFixityOverrides = cfgFileFixities
                        , cfgRegion = region
                        , cfgDebug = False
                        , cfgPrinterOpts = resolvePrinterOpts [lspPrinterOpts, cfgFilePrinterOpts]
                        }
            ExceptT . liftIO $
                bimap (PluginInternalError . T.pack . show) (InL . makeDiffTextEdit contents)
                    <$> try @OrmoluException (ormolu config fp' contents)
  where
    fp' = fromNormalizedFilePath fp
    title = "Formatting " <> T.pack (takeFileName fp')
    lspPrinterOpts = mempty{poIndentation = Just $ fromIntegral $ fo ^. tabSize}
    region = case typ of
        FormatText ->
            RegionIndices Nothing Nothing
        FormatRange (Range (Position sl _) (Position el _)) ->
            RegionIndices (Just $ fromIntegral $ sl + 1) (Just $ fromIntegral $ el + 1)
    cliHandler :: FilePath -> [String] -> ExceptT PluginError IO ([TextEdit] |? Null)
    cliHandler path fileOpts = do
        CLIVersionInfo{noCabal} <- do -- check Fourmolu version so that we know which flags to use
            (exitCode, out, _err) <- liftIO $ readCreateProcessWithExitCode ( proc path ["-v"] ) ""
            let version = do
                    guard $ exitCode == ExitSuccess
                    "fourmolu" : v : _ <- pure $ T.words out
                    traverse (readMaybe @Int . T.unpack) $ T.splitOn "." v
            case version of
                Just v -> do
                    logWith recorder Debug $ LogExternalVersion v
                    pure CLIVersionInfo
                        { noCabal = v >= [0, 7]
                        }
                Nothing -> do
                    logWith recorder Debug $ LogExternalVersion []
                    logWith recorder Warning $ NoVersion out
                    pure CLIVersionInfo
                        { noCabal = True
                        }
        (exitCode, out, err) <- -- run Fourmolu
            liftIO $ readCreateProcessWithExitCode
                ( proc path $
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
                pure $ InL $ makeDiffTextEdit contents out
            ExitFailure n -> do
                logWith recorder Info $ StdErr err
                throwError $ PluginInternalError $ "Fourmolu failed with exit code " <> T.pack (show n)

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

#if !MIN_VERSION_fourmolu(0,14,0)
resolvePrinterOpts :: [PrinterOptsPartial] -> PrinterOptsTotal
resolvePrinterOpts = foldr fillMissingPrinterOpts defaultPrinterOpts
#endif
