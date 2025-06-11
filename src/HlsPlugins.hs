{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module HlsPlugins where

import           Ide.Logger                        (Pretty (pretty), Recorder,
                                                    WithPriority, cmapWithPrio)
import           Ide.PluginUtils                   (pluginDescToIdePlugins)
import           Ide.Types                         (IdePlugins,
                                                    PluginId (PluginId))

-- fixed plugins
import           Development.IDE                   (IdeState)
import qualified Development.IDE.Plugin.HLS.GhcIde as GhcIde

-- haskell-language-server optional plugins
#if hls_qualifyImportedNames
import qualified Ide.Plugin.QualifyImportedNames   as QualifyImportedNames
#endif

#if hls_callHierarchy
import qualified Ide.Plugin.CallHierarchy          as CallHierarchy
#endif
#if hls_cabal
import qualified Ide.Plugin.Cabal                  as Cabal
#endif
#if hls_class
import qualified Ide.Plugin.Class                  as Class
#endif

#if hls_eval
import qualified Ide.Plugin.Eval                   as Eval
#endif

#if hls_importLens
import qualified Ide.Plugin.ExplicitImports        as ExplicitImports
#endif



#if hls_rename
import qualified Ide.Plugin.Rename                 as Rename
#endif

#if hls_retrie
import qualified Ide.Plugin.Retrie                 as Retrie
#endif

#if hls_hlint
import qualified Ide.Plugin.Hlint                  as Hlint
#endif

#if hls_stan
import qualified Ide.Plugin.Stan                   as Stan
#endif

#if hls_moduleName
import qualified Ide.Plugin.ModuleName             as ModuleName
#endif

#if hls_pragmas
import qualified Ide.Plugin.Pragmas                as Pragmas
#endif

#if hls_splice
import qualified Ide.Plugin.Splice                 as Splice
#endif

#if hls_alternateNumberFormat
import qualified Ide.Plugin.AlternateNumberFormat  as AlternateNumberFormat
#endif

#if hls_codeRange
import qualified Ide.Plugin.CodeRange              as CodeRange
#endif

#if hls_changeTypeSignature
import qualified Ide.Plugin.ChangeTypeSignature    as ChangeTypeSignature
#endif

#if hls_gadt
import qualified Ide.Plugin.GADT                   as GADT
#endif

#if explicitFixity
import qualified Ide.Plugin.ExplicitFixity         as ExplicitFixity
#endif

#if explicitFields
import qualified Ide.Plugin.ExplicitFields         as ExplicitFields
#endif

#if hls_overloaded_record_dot
import qualified Ide.Plugin.OverloadedRecordDot    as OverloadedRecordDot
#endif

#if hls_notes
import qualified Ide.Plugin.Notes                  as Notes
#endif

-- formatters

#if hls_floskell
import qualified Ide.Plugin.Floskell               as Floskell
#endif

#if hls_fourmolu
import qualified Ide.Plugin.Fourmolu               as Fourmolu
#endif

#if hls_cabalfmt
import qualified Ide.Plugin.CabalFmt               as CabalFmt
#endif

#if hls_cabalgild
import qualified Ide.Plugin.CabalGild              as CabalGild
#endif

#if hls_ormolu
import qualified Ide.Plugin.Ormolu                 as Ormolu
#endif

#if hls_stylishHaskell
import qualified Ide.Plugin.StylishHaskell         as StylishHaskell
#endif

#if hls_refactor
import qualified Development.IDE.Plugin.CodeAction as Refactor
#endif

#if hls_semanticTokens
import qualified Ide.Plugin.SemanticTokens         as SemanticTokens
#endif


data Log = forall a. (Pretty a) => Log PluginId a

instance Pretty Log where
  pretty (Log (PluginId pId) a) = pretty pId <> ": " <> pretty a

-- ---------------------------------------------------------------------

-- | The plugins configured for use in this instance of the language
-- server.
-- These can be freely added or removed to tailor the available
-- features of the server.

idePlugins :: Recorder (WithPriority Log) -> IdePlugins IdeState
idePlugins recorder = pluginDescToIdePlugins allPlugins
  where
    pluginRecorder :: forall log. (Pretty log) => PluginId -> Recorder (WithPriority log)
    pluginRecorder pluginId = cmapWithPrio (Log pluginId) recorder
    allPlugins =
#if hls_cabal
      let pId = "cabal" in Cabal.descriptor (pluginRecorder pId) pId :
      let caId = "cabalHaskellIntegration" in Cabal.haskellInteractionDescriptor (pluginRecorder caId) caId :
#endif
#if hls_pragmas
      Pragmas.suggestPragmaDescriptor  "pragmas-suggest" :
      Pragmas.completionDescriptor  "pragmas-completion" :
      Pragmas.suggestDisableWarningDescriptor  "pragmas-disable" :
#endif
#if hls_floskell
      Floskell.descriptor "floskell" :
#endif
#if hls_fourmolu
      let pId = "fourmolu" in Fourmolu.descriptor (pluginRecorder pId) pId:
#endif
#if hls_cabalfmt
      let pId = "cabal-fmt" in CabalFmt.descriptor (pluginRecorder pId) pId:
#endif
#if hls_cabalgild
      -- this pId needs to be kept in sync with the hardcoded
      -- cabalFormattingProvider in the Default Config
      let pId = "cabal-gild" in CabalGild.descriptor (pluginRecorder pId) pId:
#endif
#if hls_ormolu
      -- this pId needs to be kept in sync with the hardcoded
      -- haskellFormattingProvider in the Default Config
      let pId = "ormolu" in Ormolu.descriptor (pluginRecorder pId) pId :
#endif
#if hls_stylishHaskell
      let pId = "stylish-haskell" in StylishHaskell.descriptor (pluginRecorder pId) pId :
#endif
#if hls_rename
      let pId = "rename" in Rename.descriptor (pluginRecorder pId) pId:
#endif
#if hls_retrie
      let pId = "retrie" in Retrie.descriptor (pluginRecorder pId) pId :
#endif
#if hls_callHierarchy
      CallHierarchy.descriptor "callHierarchy" :
#endif
#if hls_semanticTokens
      let pId = "semanticTokens" in SemanticTokens.descriptor (pluginRecorder pId) pId:
#endif
#if hls_class
      let pId = "class" in Class.descriptor (pluginRecorder pId) pId:
#endif
#if hls_eval
      let pId = "eval" in Eval.descriptor (pluginRecorder pId) pId:
#endif
#if hls_importLens
      let pId = "importLens" in ExplicitImports.descriptor (pluginRecorder pId) pId:
#endif
#if hls_qualifyImportedNames
      QualifyImportedNames.descriptor "qualifyImportedNames" :
#endif
#if hls_moduleName
      let pId = "moduleName" in ModuleName.descriptor (pluginRecorder pId) pId:
#endif
#if hls_hlint
      let pId = "hlint" in Hlint.descriptor (pluginRecorder pId) pId:
#endif
#if hls_stan
      let pId = "stan" in Stan.descriptor (pluginRecorder pId) pId :
#endif
#if hls_splice
      Splice.descriptor "splice" :
#endif
#if hls_alternateNumberFormat
      let pId = "alternateNumberFormat" in AlternateNumberFormat.descriptor (pluginRecorder pId) pId :
#endif
#if hls_codeRange
      let pId = "codeRange" in CodeRange.descriptor (pluginRecorder pId) pId:
#endif
#if hls_changeTypeSignature
      let pId = "changeTypeSignature" in ChangeTypeSignature.descriptor (pluginRecorder pId) pId :
#endif
#if hls_gadt
      GADT.descriptor "gadt" :
#endif
#if hls_refactor
      let pId = "ghcide-code-actions-imports-exports" in Refactor.iePluginDescriptor (pluginRecorder pId) pId :
      let pId = "ghcide-code-actions-type-signatures" in Refactor.typeSigsPluginDescriptor (pluginRecorder     pId) pId :
      let pId = "ghcide-code-actions-bindings" in Refactor.bindingsPluginDescriptor (pluginRecorder pId) pId :
      let pId = "ghcide-code-actions-fill-holes" in Refactor.fillHolePluginDescriptor (pluginRecorder pId) pId :
      let pId = "ghcide-extend-import-action" in Refactor.extendImportPluginDescriptor (pluginRecorder pId) pId :
#endif
#if explicitFixity
      let pId = "explicit-fixity" in ExplicitFixity.descriptor (pluginRecorder pId) pId :
#endif
#if explicitFields
      let pId = "explicit-fields" in ExplicitFields.descriptor (pluginRecorder pId) pId :
#endif
#if hls_overloaded_record_dot
      let pId = "overloaded-record-dot" in OverloadedRecordDot.descriptor (pluginRecorder pId) pId :
#endif
#if hls_notes
      let pId = "notes" in Notes.descriptor (pluginRecorder pId) pId :
#endif
      GhcIde.descriptors (pluginRecorder "ghcide")

