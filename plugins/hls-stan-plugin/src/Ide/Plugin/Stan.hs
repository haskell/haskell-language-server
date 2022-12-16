module Ide.Plugin.Stan (descriptor, Log) where

import           Control.DeepSeq                (NFData)
import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (liftIO)
import           Control.Monad.Trans.Class      (lift)
import           Control.Monad.Trans.Maybe      (MaybeT (MaybeT), runMaybeT)
import           Data.Default
import           Data.Foldable                  (toList)
import           Data.Hashable                  (Hashable)
import qualified Data.HashMap.Strict            as HM
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust, mapMaybe)
import qualified Data.Text                      as T
import           Development.IDE
import           Development.IDE.Core.Rules     (getHieFile,
                                                 getSourceFileSource)
import           Development.IDE.Core.RuleTypes (HieAstResult (..))
import qualified Development.IDE.Core.Shake     as Shake
import           Development.IDE.GHC.Compat     (HieASTs (HieASTs),
                                                 RealSrcSpan (..), mkHieFile',
                                                 mkRealSrcLoc, mkRealSrcSpan,
                                                 runHsc, srcSpanEndCol,
                                                 srcSpanEndLine,
                                                 srcSpanStartCol,
                                                 srcSpanStartLine, tcg_exports)
import           Development.IDE.GHC.Error      (realSrcSpanToRange)
import           GHC.Generics                   (Generic)
import           HieTypes                       (HieASTs, HieFile)
import           Ide.Plugin.Config
import           Ide.Types                      (PluginDescriptor (..),
                                                 PluginId, configHasDiagnostics,
                                                 defaultConfigDescriptor,
                                                 defaultPluginDescriptor,
                                                 pluginEnabledConfig)
import qualified Language.LSP.Types             as LSP
import           Stan.Analysis                  (Analysis (..), runAnalysis)
import           Stan.Category                  (Category (..))
import           Stan.Core.Id                   (Id (..))
import           Stan.Inspection                (Inspection (..))
import           Stan.Inspection.All            (inspectionsIds, inspectionsMap)
import           Stan.Observation               (Observation (..))

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId)
  { pluginRules = rules recorder plId
  , pluginConfigDescriptor = defaultConfigDescriptor
      { configHasDiagnostics = True
      }
    }

newtype Log = LogShake Shake.Log deriving (Show)

instance Pretty Log where
  pretty = \case
    LogShake log -> pretty log

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
      if pluginEnabledConfig plcDiagnosticsOn config then do
          maybeHie <- getHieFile file
          case maybeHie of
            Nothing -> return ([], Nothing)
            Just hie -> do
              let enabledInspections = HM.fromList [(LSP.fromNormalizedFilePath file, inspectionsIds)]
              -- This should use Cabal config for extensions and Stan config for inspection preferences is the future
              let analysis = runAnalysis Map.empty enabledInspections [] [hie]
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
              _severity = Just LSP.DsHint,
              _code = Just (LSP.InR $ unId (inspectionId inspection)),
              _source = Just "stan",
              _message = message,
              _relatedInformation = Nothing,
              _tags = Nothing
            }
          )
