{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}
module Ide.Plugin.Stan (descriptor, Log) where

import           Compat.HieTypes                (HieASTs, HieFile (..))
import           Control.DeepSeq                (NFData)
import           Control.Monad                  (void, when)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Data.Default
import           Data.Foldable                  (toList)
import           Data.Hashable                  (Hashable)
import qualified Data.HashMap.Strict            as HM
import           Data.HashSet                   (HashSet)
import qualified Data.HashSet                   as HS
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust, mapMaybe,
                                                 maybeToList)
import           Data.String                    (IsString (fromString))
import qualified Data.Text                      as T
import           Development.IDE
import           Development.IDE.Core.Rules     (getHieFile,
                                                 getSourceFileSource)
import           Development.IDE.Core.RuleTypes (HieAstResult (..))
import qualified Development.IDE.Core.Shake     as Shake
import           Development.IDE.GHC.Compat     (HieASTs (HieASTs),
                                                 HieFile (hie_hs_file),
                                                 RealSrcSpan (..), mkHieFile',
                                                 mkRealSrcLoc, mkRealSrcSpan,
                                                 runHsc, srcSpanEndCol,
                                                 srcSpanEndLine,
                                                 srcSpanStartCol,
                                                 srcSpanStartLine, tcg_exports)
import           Development.IDE.GHC.Error      (realSrcSpanToRange)
import           GHC.Generics                   (Generic)
import           Ide.Plugin.Config              (PluginConfig (..))
import           Ide.Types                      (PluginDescriptor (..),
                                                 PluginId, configHasDiagnostics,
                                                 configInitialGenericConfig,
                                                 defaultConfigDescriptor,
                                                 defaultPluginDescriptor)
import qualified Language.LSP.Protocol.Types    as LSP
import           Stan                           (createCabalExtensionsMap,
                                                 getStanConfig)
import           Stan.Analysis                  (Analysis (..), runAnalysis)
import           Stan.Category                  (Category (..))
import           Stan.Cli                       (StanArgs (..))
import           Stan.Config                    (Config, ConfigP (..),
                                                 applyConfig, defaultConfig)
import           Stan.Config.Pretty             (ConfigAction, configToTriples,
                                                 prettyConfigAction,
                                                 prettyConfigCli)
import           Stan.Core.Id                   (Id (..))
import           Stan.EnvVars                   (EnvVars (..), envVarsToText)
import           Stan.Inspection                (Inspection (..))
import           Stan.Inspection.All            (inspectionsIds, inspectionsMap)
import           Stan.Observation               (Observation (..))
import           Stan.Report.Settings           (OutputSettings (..),
                                                 ToggleSolution (..),
                                                 Verbosity (..))
import           Stan.Toml                      (usedTomlFiles)
import           System.Directory               (makeRelativeToCurrentDirectory)
import           Trial                          (Fatality, Trial (..), fiasco,
                                                 pattern FiascoL,
                                                 pattern ResultL, prettyTrial,
                                                 prettyTrialWith)
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

renderId :: Id a -> T.Text
renderId (Id t) = "Id = " <> t

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
              seTomlFiles <- liftIO $ usedTomlFiles useDefConfig (stanArgsConfigFile stanArgs)
              logWith recorder Debug (LogDebugStanConfigResult seTomlFiles configTrial)

              -- If envVar is set to 'False', stan will ignore all local and global .stan.toml files
              logWith recorder Debug (LogDebugStanEnvVars env)
              seTomlFiles <- liftIO $ usedTomlFiles useDefConfig (stanArgsConfigFile stanArgs)

              (cabalExtensionsMap, checksMap, confIgnored) <- case configTrial of
                  FiascoL es -> do
                      logWith recorder Development.IDE.Warning (LogWarnConf es)
                      pure (Map.empty,
                            HM.fromList [(LSP.fromNormalizedFilePath file, inspectionsIds)],
                            [])
                  ResultL warnings stanConfig -> do
                      let currentHSAbs = fromNormalizedFilePath file -- hie_hs_file hie
                      currentHSRel <- liftIO $ makeRelativeToCurrentDirectory currentHSAbs
                      cabalExtensionsMap <- liftIO $ createCabalExtensionsMap isLoud (stanArgsCabalFilePath stanArgs) [hie]

                      -- Files (keys) in checksMap need to have an absolute path for the analysis, but applyConfig needs to receive relative
                      -- filepaths to apply the config, because the toml config has relative paths. Stan itself seems to work only in terms of relative paths.
                      let checksMap = HM.mapKeys (const currentHSAbs) $ applyConfig [currentHSRel] stanConfig

                      let analysis = runAnalysis cabalExtensionsMap checksMap (configIgnored stanConfig) [hie]
                      pure (cabalExtensionsMap, checksMap, configIgnored stanConfig)
              let analysis = runAnalysis cabalExtensionsMap checksMap confIgnored [hie]
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
        return ( file,
          ShowDiag,
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
          )
