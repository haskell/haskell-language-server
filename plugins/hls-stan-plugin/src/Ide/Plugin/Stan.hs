{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}
module Ide.Plugin.Stan (descriptor, Log) where

import           Compat.HieTypes             (HieFile (..))
import           Control.DeepSeq             (NFData)
import           Control.Monad               (void)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Foldable               (toList)
import           Data.Hashable               (Hashable)
import qualified Data.HashMap.Strict         as HM
import           Data.Maybe                  (mapMaybe)
import qualified Data.Text                   as T
import           Development.IDE
import           Development.IDE.Core.Rules  (getHieFile)
import qualified Development.IDE.Core.Shake  as Shake
import           GHC.Generics                (Generic)
import           Ide.Plugin.Config           (PluginConfig (..))
import           Ide.Types                   (PluginDescriptor (..), PluginId,
                                              configHasDiagnostics,
                                              configInitialGenericConfig,
                                              defaultConfigDescriptor,
                                              defaultPluginDescriptor)
import qualified Language.LSP.Protocol.Types as LSP
import           Stan                        (createCabalExtensionsMap,
                                              getStanConfig)
import           Stan.Analysis               (Analysis (..), runAnalysis)
import           Stan.Category               (Category (..))
import           Stan.Cli                    (StanArgs (..))
import           Stan.Config                 (Config, ConfigP (..), applyConfig)
import           Stan.Config.Pretty          (prettyConfigCli)
import           Stan.Core.Id                (Id (..))
import           Stan.EnvVars                (EnvVars (..), envVarsToText)
import           Stan.Inspection             (Inspection (..))
import           Stan.Inspection.All         (inspectionsIds, inspectionsMap)
import           Stan.Observation            (Observation (..))
import           Stan.Report.Settings        (OutputSettings (..),
                                              ToggleSolution (..),
                                              Verbosity (..))
import           Stan.Toml                   (usedTomlFiles)
import           System.Directory            (makeRelativeToCurrentDirectory)
import           Trial                       (Fatality, Trial (..), fiasco,
                                              pattern FiascoL, pattern ResultL,
                                              prettyTrial, prettyTrialWith)

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId desc)
  { pluginRules = rules recorder plId
  , pluginConfigDescriptor = defConfigDescriptor
      { configHasDiagnostics = True
      -- We disable this plugin by default because users have been complaining about
      -- the diagnostics, see https://github.com/haskell/haskell-language-server/issues/3916
      , configInitialGenericConfig = (configInitialGenericConfig defConfigDescriptor)
        { plcGlobalOn = False
        }
      }
    }
  where
    defConfigDescriptor = defaultConfigDescriptor
    desc = "Provides stan diagnostics. Built with stan-" <> VERSION_stan

data Log = LogShake !Shake.Log
         | LogWarnConf ![(Fatality, T.Text)]
         | LogDebugStanConfigResult ![FilePath] !(Trial T.Text Config)
         | LogDebugStanEnvVars !EnvVars

-- We use this function to remove the terminal escape sequences emmited by Trial pretty printing functions.
-- See https://github.com/kowainik/trial/pull/73#issuecomment-1868233235
stripModifiers :: T.Text -> T.Text
stripModifiers = go ""
  where
    go acc txt =
      case T.findIndex (== '\x1B') txt of
        Nothing -> acc <> txt
        Just index -> let (beforeEsc, afterEsc) = T.splitAt index txt
                      in go (acc <> beforeEsc) (consumeEscapeSequence afterEsc)
    consumeEscapeSequence :: T.Text -> T.Text
    consumeEscapeSequence txt =
      case T.findIndex (== 'm') txt of
        Nothing    -> txt
        Just index -> T.drop (index + 1) txt

instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log
    LogWarnConf errs -> "Fiasco encountered when trying to load stan configuration. Using default inspections:"
                        <> line <> (pretty $ show errs)
    LogDebugStanConfigResult fps t -> "Config result using: "
                                      <> pretty fps <> line <> pretty (stripModifiers $ prettyTrialWith (T.unpack . prettyConfigCli) t)
    LogDebugStanEnvVars envVars -> "EnvVars " <>
        case envVars of
            EnvVars trial@(FiascoL _) -> pretty (stripModifiers $ prettyTrial trial)

            -- if the envVars are not set, 'envVarsToText returns an empty string'
            _ -> "found: " <> (pretty $ envVarsToText envVars)

data GetStanDiagnostics = GetStanDiagnostics
  deriving (Eq, Show, Generic)

instance Hashable GetStanDiagnostics

instance NFData GetStanDiagnostics

type instance RuleResult GetStanDiagnostics = ()

rules :: Recorder (WithPriority Log) -> PluginId -> Rules ()
rules recorder plId = do
  define (cmapWithPrio LogShake recorder) $
    \GetStanDiagnostics file -> do
      config <- getPluginConfigAction plId
      if plcGlobalOn config && plcDiagnosticsOn config then do
          maybeHie <- getHieFile file
          case maybeHie of
            Nothing -> return ([], Nothing)
            Just hie -> do
              let isLoud = False -- in Stan: notJson = not isLoud
              let stanArgs =
                      StanArgs
                          { stanArgsHiedir               = "" -- :: !FilePath  -- ^ Directory with HIE files
                          , stanArgsCabalFilePath        = [] -- :: ![FilePath]  -- ^ Path to @.cabal@ files.
                          , stanArgsOutputSettings       = OutputSettings NonVerbose ShowSolution -- :: !OutputSettings  -- ^ Settings for output terminal report
                                                                                                  -- doesnt matter, because it is silenced by isLoud
                          , stanArgsReport               = Nothing -- :: !(Maybe ReportArgs)  -- ^ @HTML@ report settings
                          , stanArgsUseDefaultConfigFile = fiasco "" -- :: !(TaggedTrial Text Bool)  -- ^ Use default @.stan.toml@ file
                          , stanArgsConfigFile           = Nothing -- :: !(Maybe FilePath)  -- ^ Path to a custom configurations file.
                          , stanArgsConfig               = ConfigP
                                                            { configChecks  = fiasco "'hls-stan-plugin' doesn't receive CLI options for: checks"
                                                            , configRemoved = fiasco "'hls-stan-plugin' doesn't receive CLI options for: remove"
                                                            , configIgnored = fiasco "'hls-stan-plugin' doesn't receive CLI options for: ignore"
                                                            }
                                                            -- if they are not fiascos, .stan.toml's aren't taken into account
                          ,stanArgsJsonOut              = not isLoud -- :: !Bool  -- ^ Output the machine-readable output in JSON format instead.
                          }

              (configTrial, useDefConfig, env) <- liftIO $ getStanConfig stanArgs isLoud
              tomlsUsedByStan <- liftIO $ usedTomlFiles useDefConfig (stanArgsConfigFile stanArgs)
              logWith recorder Debug (LogDebugStanConfigResult tomlsUsedByStan configTrial)

              -- If envVar is set to 'False', stan will ignore all local and global .stan.toml files
              logWith recorder Debug (LogDebugStanEnvVars env)

              -- Note that Stan works in terms of relative paths, but the HIE come in as absolute. Without
              -- making its path relative, the file name(s) won't line up with the associated Map keys.
              relativeHsFilePath <- liftIO $ makeRelativeToCurrentDirectory $ fromNormalizedFilePath file
              let hieRelative = hie{hie_hs_file=relativeHsFilePath}

              (checksMap, ignoredObservations) <- case configTrial of
                  FiascoL es -> do
                      logWith recorder Development.IDE.Warning (LogWarnConf es)
                      -- If we can't read the config file, default to using all inspections:
                      let allInspections = HM.singleton relativeHsFilePath inspectionsIds
                      pure (allInspections, [])
                  ResultL _warnings stanConfig -> do
                      -- HashMap of *relative* file paths to info about enabled checks for those file paths.
                      let checksMap = applyConfig [relativeHsFilePath] stanConfig
                      pure (checksMap, configIgnored stanConfig)

              -- A Map from *relative* file paths (just one, in this case) to language extension info:
              cabalExtensionsMap <- liftIO $ createCabalExtensionsMap isLoud (stanArgsCabalFilePath stanArgs) [hieRelative]
              let analysis = runAnalysis cabalExtensionsMap checksMap ignoredObservations [hieRelative]
              return (analysisToDiagnostics file analysis, Just ())
      else return ([], Nothing)

  action $ do
    files <- getFilesOfInterestUntracked
    void $ uses GetStanDiagnostics $ HM.keys files
  where
    analysisToDiagnostics :: NormalizedFilePath -> Analysis -> [FileDiagnostic]
    analysisToDiagnostics file = mapMaybe (observationToDianostic file) . toList . analysisObservations
    observationToDianostic :: NormalizedFilePath -> Observation -> Maybe FileDiagnostic
    observationToDianostic file Observation {observationSrcSpan, observationInspectionId} =
      do
        inspection <- HM.lookup observationInspectionId inspectionsMap
        let
          -- Looking similar to Stan CLI output
          -- We do not use `prettyShowInspection` cuz Id is redundant here
          -- `prettyShowSeverity` and `prettyShowCategory` would contain color
          -- codes and are replaced, too
          message :: T.Text
          message =
            T.unlines $
              [ " ✲ Name:        " <> inspectionName inspection,
                " ✲ Description: " <> inspectionDescription inspection,
                " ✲ Severity:    " <> (T.pack $ show $ inspectionSeverity inspection),
                " ✲ Category:    " <> T.intercalate " "
                  (map (("#" <>) . unCategory) $ toList $ inspectionCategory inspection),
                "Possible solutions:"
              ]
                ++ map ("  - " <>) (inspectionSolution inspection)
        return $
          ideErrorFromLspDiag
            LSP.Diagnostic
              { _range = realSrcSpanToRange observationSrcSpan,
                _severity = Just LSP.DiagnosticSeverity_Hint,
                _code = Just (LSP.InR $ unId (inspectionId inspection)),
                _source = Just "stan",
                _message = message,
                _relatedInformation = Nothing,
                _tags = Nothing,
                _codeDescription = Nothing,
                _data_ = Nothing
              }
            file
            Nothing
