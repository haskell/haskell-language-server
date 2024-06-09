{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.Ormolu
  ( descriptor
  , provider
  , LogEvent
  )
where

import           Control.Exception               (Handler (..), IOException,
                                                  SomeException (..), catches,
                                                  handle)
import           Control.Monad.Except            (runExceptT, throwError)
import           Control.Monad.Extra
import           Control.Monad.Trans
import           Control.Monad.Trans.Except      (ExceptT (..), mapExceptT)
import           Data.Functor                    ((<&>))
import           Data.List                       (intercalate)
import           Data.Maybe                      (catMaybes)
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Development.IDE                 hiding (pluginHandlers)
import           Development.IDE.GHC.Compat      (hsc_dflags, moduleNameString)
import qualified Development.IDE.GHC.Compat      as D
import qualified Development.IDE.GHC.Compat.Util as S
import           GHC.LanguageExtensions.Type
import           Ide.Plugin.Error                (PluginError (PluginInternalError))
import           Ide.Plugin.Properties
import           Ide.PluginUtils
import           Ide.Types                       hiding (Config)
import qualified Ide.Types                       as Types
import           Language.LSP.Protocol.Types
import           Language.LSP.Server             hiding (defaultConfig)
import           Ormolu
import           System.Exit
import           System.FilePath
import           System.Process.Run              (cwd, proc)
import           System.Process.Text             (readCreateProcessWithExitCode)
import           Text.Read                       (readMaybe)

-- ---------------------------------------------------------------------

descriptor :: Recorder (WithPriority LogEvent) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId desc)
    { pluginHandlers = mkFormattingHandlers $ provider recorder plId,
      pluginConfigDescriptor = defaultConfigDescriptor {configCustomConfig = mkCustomConfig properties}
    }
  where
    desc = "Provides formatting of Haskell files via ormolu. Built with ormolu-" <> VERSION_ormolu

properties :: Properties '[ 'PropertyKey "external" 'TBoolean]
properties =
  emptyProperties
    & defineBooleanProperty
      #external
      "Call out to an external \"ormolu\" executable, rather than using the bundled library"
      False

-- ---------------------------------------------------------------------

provider :: Recorder (WithPriority LogEvent) -> PluginId -> FormattingHandler IdeState
provider recorder plId ideState token typ contents fp _ = ExceptT $ pluginWithIndefiniteProgress title token Cancellable $ \_updater -> runExceptT $ do
  fileOpts <-
      maybe [] (fromDyn . hsc_dflags . hscEnv)
          <$> liftIO (runAction "Ormolu" ideState $ use GhcSession fp)
  useCLI <- liftIO $ runAction "Ormolu" ideState $ usePropertyAction #external plId properties

  if useCLI
      then mapExceptT liftIO $ ExceptT
           $ handle @IOException
          (pure . Left . PluginInternalError . T.pack . show)
           $ runExceptT $ cliHandler fileOpts
      else do
          logWith recorder Debug $ LogCompiledInVersion VERSION_ormolu

          let
            fmt :: T.Text -> Config RegionIndices -> IO (Either SomeException T.Text)
            fmt cont conf = flip catches handlers $ do
#if MIN_VERSION_ormolu(0,5,3)
              cabalInfo <- getCabalInfoForSourceFile fp' <&> \case
                CabalNotFound                -> Nothing
                CabalDidNotMention cabalInfo -> Just cabalInfo
                CabalFound cabalInfo         -> Just cabalInfo
#if MIN_VERSION_ormolu(0,7,0)
              (fixityOverrides, moduleReexports) <- getDotOrmoluForSourceFile fp'
              let conf' = refineConfig ModuleSource cabalInfo (Just fixityOverrides) (Just moduleReexports) conf
#else
              fixityOverrides <- traverse getFixityOverridesForSourceFile cabalInfo
              let conf' = refineConfig ModuleSource cabalInfo fixityOverrides conf
#endif
              let cont' = cont
#else
              let conf' = conf
                  cont' = T.unpack cont
#endif
              Right <$> ormolu conf' fp' cont'
            handlers =
              [ Handler $ pure . Left . SomeException @OrmoluException
              , Handler $ pure . Left . SomeException @IOException
              ]

          res <- liftIO $ fmt contents defaultConfig { cfgDynOptions = map DynOption fileOpts, cfgRegion = region }
          ret res
 where
   fp' = fromNormalizedFilePath fp

   region :: RegionIndices
   region = case typ of
       FormatText ->
           RegionIndices Nothing Nothing
       FormatRange (Range (Position sl _) (Position el _)) ->
           RegionIndices (Just $ fromIntegral $ sl + 1) (Just $ fromIntegral $ el + 1)

   title = T.pack $ "Formatting " <> takeFileName (fromNormalizedFilePath fp)

   ret :: Either SomeException T.Text -> ExceptT PluginError (HandlerM Types.Config) ([TextEdit] |? Null)
   ret (Left err)  = throwError $ PluginInternalError . T.pack $ "ormoluCmd: " ++ show err
   ret (Right new) = pure $ InL $ makeDiffTextEdit contents new

   fromDyn :: D.DynFlags -> [String]
   fromDyn df =
     let
       pp =
         let p = D.sPgm_F $ D.settings df
         in  ["-pgmF=" <> p | not (null p)]
       pm = ("-fplugin=" <>) . moduleNameString <$> D.pluginModNames df
       ex = showExtension <$> S.toList (D.extensionFlags df)
     in pp <> pm <> ex

   cliHandler :: [String] -> ExceptT PluginError IO ([TextEdit] |? Null)
   cliHandler fileOpts = do
       CLIVersionInfo{noCabal} <- do -- check Ormolu version so that we know which flags to use
           (exitCode, out, _err) <- liftIO $ readCreateProcessWithExitCode ( proc "ormolu" ["--version"] ) ""
           let version = do
                   guard $ exitCode == ExitSuccess
                   "ormolu" : v : _ <- pure $ T.words out
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
       (exitCode, out, err) <- do -- run Ormolu
           let commandArgs = map ("-o" <>) fileOpts
                       -- "The --stdin-input-file option is necessary when using input from
                       -- stdin and accounting for .cabal files" as per Ormolu documentation
                       <> (if noCabal then ["--no-cabal"] else ["--stdin-input-file", fp'])
                       <> catMaybes
                           [ ("--start-line=" <>) . show <$> regionStartLine region
                           , ("--end-line=" <>) . show <$> regionEndLine region
                           ]
               cwd = takeDirectory fp'
           logWith recorder Debug $ LogOrmoluCommand commandArgs cwd
           liftIO $ readCreateProcessWithExitCode (proc "ormolu" commandArgs) {cwd = Just cwd} contents
       case exitCode of
           ExitSuccess -> do
               when (not $ T.null err) $ logWith recorder Debug $ StdErr err
               pure $ InL $ makeDiffTextEdit contents out
           ExitFailure n -> do
               logWith recorder Info $ StdErr err
               throwError $ PluginInternalError $ "Ormolu failed with exit code " <> T.pack (show n)

newtype CLIVersionInfo =   CLIVersionInfo
    { noCabal :: Bool
    }

data LogEvent
    = NoVersion Text
    | StdErr Text
    | LogCompiledInVersion String
    | LogExternalVersion [Int]
    | LogOrmoluCommand [String] FilePath
    deriving (Show)

instance Pretty LogEvent where
    pretty = \case
        NoVersion t -> "Couldn't get Ormolu version:" <> line <> indent 2 (pretty t)
        StdErr t -> "Ormolu stderr:" <> line <> indent 2 (pretty t)
        LogCompiledInVersion v -> "Using compiled in ormolu-" <> pretty v
        LogExternalVersion v ->
            "Using external ormolu"
            <> if null v then "" else "-"
            <> pretty (intercalate "." $ map show v)
        LogOrmoluCommand commandArgs cwd -> "Running: `ormolu " <> pretty (unwords commandArgs) <> "` in directory " <> pretty cwd

showExtension :: Extension -> String
showExtension Cpp   = "-XCPP"
showExtension other = "-X" ++ show other
