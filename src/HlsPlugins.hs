{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}
module HlsPlugins where

import           Development.IDE.Types.Logger      (Pretty (pretty), Recorder,
                                                    WithPriority, cmapWithPrio)
import           Ide.PluginUtils                   (pluginDescToIdePlugins)
import           Ide.Types                         (IdePlugins)

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

#if hls_class
import qualified Ide.Plugin.Class                  as Class
#endif

#if hls_haddockComments
import qualified Ide.Plugin.HaddockComments        as HaddockComments
#endif

#if hls_eval
import qualified Ide.Plugin.Eval                   as Eval
#endif

#if hls_importLens
import qualified Ide.Plugin.ExplicitImports        as ExplicitImports
#endif

#if hls_refineImports
import qualified Ide.Plugin.RefineImports          as RefineImports
#endif

#if hls_rename
import qualified Ide.Plugin.Rename                 as Rename
#endif

#if hls_retrie
import qualified Ide.Plugin.Retrie                 as Retrie
#endif

#if hls_tactic
import qualified Ide.Plugin.Tactic                 as Tactic
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
import           Ide.Plugin.ChangeTypeSignature    as ChangeTypeSignature
#endif

#if hls_gadt
import           Ide.Plugin.GADT                   as GADT
#endif

#if explicitFixity
import           Ide.Plugin.ExplicitFixity         as ExplicitFixity
#endif

-- formatters

#if hls_floskell
import qualified Ide.Plugin.Floskell               as Floskell
#endif

#if hls_fourmolu
import qualified Ide.Plugin.Fourmolu               as Fourmolu
#endif

#if hls_ormolu
import qualified Ide.Plugin.Ormolu                 as Ormolu
#endif

#if hls_stylishHaskell
import qualified Ide.Plugin.StylishHaskell         as StylishHaskell
#endif

#if hls_brittany
import qualified Ide.Plugin.Brittany               as Brittany
#endif

#if hls_refactor
import qualified Development.IDE.Plugin.CodeAction as Refactor
#endif

data Log = forall a. (Pretty a) => Log a

instance Pretty Log where
  pretty (Log a) = pretty a

-- ---------------------------------------------------------------------

-- | The plugins configured for use in this instance of the language
-- server.
-- These can be freely added or removed to tailor the available
-- features of the server.

idePlugins :: Recorder (WithPriority Log) -> IdePlugins IdeState
idePlugins recorder = pluginDescToIdePlugins allPlugins
  where
    pluginRecorder :: forall log. (Pretty log) => Recorder (WithPriority log)
    pluginRecorder = cmapWithPrio Log recorder
    allPlugins =
#if hls_pragmas
      Pragmas.descriptor  "pragmas" :
#endif
#if hls_floskell
      Floskell.descriptor "floskell" :
#endif
#if hls_fourmolu
      Fourmolu.descriptor pluginRecorder "fourmolu" :
#endif
#if hls_tactic
      Tactic.descriptor pluginRecorder "tactics" :
#endif
#if hls_ormolu
      Ormolu.descriptor   "ormolu" :
#endif
#if hls_stylishHaskell
      StylishHaskell.descriptor "stylish-haskell" :
#endif
#if hls_rename
      Rename.descriptor pluginRecorder "rename" :
#endif
#if hls_retrie
      Retrie.descriptor "retrie" :
#endif
#if hls_brittany
      Brittany.descriptor "brittany" :
#endif
#if hls_callHierarchy
      CallHierarchy.descriptor :
#endif
#if hls_class
      Class.descriptor pluginRecorder "class" :
#endif
#if hls_haddockComments
      HaddockComments.descriptor pluginRecorder "haddockComments" :
#endif
#if hls_eval
      Eval.descriptor pluginRecorder "eval" :
#endif
#if hls_importLens
      ExplicitImports.descriptor pluginRecorder "importLens" :
#endif
#if hls_qualifyImportedNames
      QualifyImportedNames.descriptor "qualifyImportedNames" :
#endif
#if hls_refineImports
      RefineImports.descriptor pluginRecorder "refineImports" :
#endif
#if hls_moduleName
      ModuleName.descriptor pluginRecorder "moduleName" :
#endif
#if hls_hlint
      Hlint.descriptor pluginRecorder "hlint" :
#endif
#if hls_stan
      Stan.descriptor pluginRecorder "stan" :
#endif
#if hls_splice
      Splice.descriptor "splice" :
#endif
#if hls_alternateNumberFormat
      AlternateNumberFormat.descriptor pluginRecorder :
#endif
#if hls_codeRange
      CodeRange.descriptor pluginRecorder "codeRange" :
#endif
#if hls_changeTypeSignature
      ChangeTypeSignature.descriptor :
#endif
#if hls_gadt
      GADT.descriptor "gadt" :
#endif
#if hls_refactor
      Refactor.iePluginDescriptor pluginRecorder "ghcide-code-actions-imports-exports" :
      Refactor.typeSigsPluginDescriptor pluginRecorder "ghcide-code-actions-type-signatures" :
      Refactor.bindingsPluginDescriptor pluginRecorder "ghcide-code-actions-bindings" :
      Refactor.fillHolePluginDescriptor pluginRecorder "ghcide-code-actions-fill-holes" :
      Refactor.extendImportPluginDescriptor pluginRecorder "ghcide-extend-import-action" :
#endif
      GhcIde.descriptors pluginRecorder
#if explicitFixity
      ++ [ExplicitFixity.descriptor pluginRecorder]
#endif

