{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE TypeApplications         #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE OverloadedLabels         #-}

module Ide.Plugin.Fourmolu (
    descriptor,
    provider,
) where

import           Control.Exception               (IOException, try)
import           Control.Lens                    ((^.))
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bifunctor                  (first)
import           Data.Maybe
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           Development.IDE                 hiding (pluginHandlers)
import           Development.IDE.GHC.Compat      as Compat hiding (Cpp)
import qualified Development.IDE.GHC.Compat.Util as S
import           GHC.LanguageExtensions.Type     (Extension (Cpp))
import           Ide.Plugin.Properties
import           Ide.PluginUtils                 (makeDiffTextEdit, usePropertyLsp)
import           Ide.Types
import           Language.LSP.Server             hiding (defaultConfig)
import           Language.LSP.Types
import           Language.LSP.Types.Lens         (HasTabSize (tabSize))
import           Ormolu
import           System.Exit
import           System.FilePath
import           System.IO                       (stderr)
import           System.Process.Run              (proc, cwd)
import           System.Process.Text             (readCreateProcessWithExitCode)

descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
    (defaultPluginDescriptor plId)
        { pluginHandlers = mkFormattingHandlers $ provider plId
        }

properties :: Properties '[ 'PropertyKey "external" 'TBoolean]
properties =
    emptyProperties
        & defineBooleanProperty
            #external
            "Call out to an external \"fourmolu\" executable, rather than using the bundled library"
            False

provider :: PluginId -> FormattingHandler IdeState
provider plId ideState typ contents fp fo = withIndefiniteProgress title Cancellable $ do
    fileOpts <-
        maybe [] (convertDynFlags . hsc_dflags . hscEnv)
            <$> liftIO (runAction "Fourmolu" ideState $ use GhcSession fp)
    useCLI <- usePropertyLsp #external plId properties
    if useCLI
        then liftIO
            . fmap (join . first (mkError . show))
            . try @IOException
            $ do
                (exitCode, out, err) <-
                    readCreateProcessWithExitCode
                        ( proc "fourmolu" $
                            ["-d"]
                                <> catMaybes
                                    [ ("--start-line=" <>) . show <$> regionStartLine region
                                    , ("--end-line=" <>) . show <$> regionEndLine region
                                    ]
                                <> map ("-o" <>) fileOpts
                        ){cwd = Just $ takeDirectory fp'}
                        contents
                T.hPutStrLn stderr err
                case exitCode of
                    ExitSuccess ->
                        pure . Right $ makeDiffTextEdit contents out
                    ExitFailure n ->
                        pure . Left . responseError $ "Fourmolu failed with exit code " <> T.pack (show n)
        else do
            let format printerOpts =
                    first (mkError . show)
                        <$> try @OrmoluException (makeDiffTextEdit contents <$> ormolu config fp' (T.unpack contents))
                  where
                    config =
                        defaultConfig
                            { cfgDynOptions = map DynOption fileOpts
                            , cfgRegion = region
                            , cfgDebug = True
                            , cfgPrinterOpts =
                                fillMissingPrinterOpts
                                    (printerOpts <> lspPrinterOpts)
                                    defaultPrinterOpts
                            }
             in liftIO (loadConfigFile fp') >>= \case
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
    mkError = responseError . ("Fourmolu: " <>) . T.pack
    lspPrinterOpts = mempty{poIndentation = Just $ fromIntegral $ fo ^. tabSize}
    region = case typ of
        FormatText ->
            RegionIndices Nothing Nothing
        FormatRange (Range (Position sl _) (Position el _)) ->
            RegionIndices (Just $ fromIntegral $ sl + 1) (Just $ fromIntegral $ el + 1)

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
