{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins where

import           Development.IDE.Types.Logger      (Recorder, cmap)
import           Ide.PluginUtils                   (pluginDescToIdePlugins)
import           Ide.Types                         (IdePlugins)

-- fixed plugins
import           Development.IDE                   (IdeState)
import qualified Development.IDE.Plugin.HLS.GhcIde as GhcIde
import qualified Ide.Plugin.Example                as Example
import qualified Ide.Plugin.Example2               as Example2

-- haskell-language-server optional plugins
#if qualifyImportedNames
import qualified Ide.Plugin.QualifyImportedNames   as QualifyImportedNames
#endif

#if callHierarchy
import qualified Ide.Plugin.CallHierarchy          as CallHierarchy
#endif

#if class
import qualified Ide.Plugin.Class                  as Class
#endif

#if haddockComments
import qualified Ide.Plugin.HaddockComments        as HaddockComments
#endif

#if eval
import qualified Ide.Plugin.Eval                   as Eval
#endif

#if importLens
import qualified Ide.Plugin.ExplicitImports        as ExplicitImports
#endif

#if refineImports
import qualified Ide.Plugin.RefineImports          as RefineImports
#endif

#if rename
import qualified Ide.Plugin.Rename                 as Rename
#endif

#if retrie
import qualified Ide.Plugin.Retrie                 as Retrie
#endif

#if tactic
import qualified Ide.Plugin.Tactic                 as Tactic
#endif

#if hlint
import qualified Ide.Plugin.Hlint                  as Hlint
#endif

#if moduleName
import qualified Ide.Plugin.ModuleName             as ModuleName
#endif

#if pragmas
import qualified Ide.Plugin.Pragmas                as Pragmas
#endif

#if splice
import qualified Ide.Plugin.Splice                 as Splice
#endif

#if alternateNumberFormat
import qualified Ide.Plugin.AlternateNumberFormat  as AlternateNumberFormat
#endif

-- formatters

#if floskell
import qualified Ide.Plugin.Floskell               as Floskell
#endif

#if fourmolu
import qualified Ide.Plugin.Fourmolu               as Fourmolu
#endif

#if ormolu
import qualified Ide.Plugin.Ormolu                 as Ormolu
#endif

#if stylishHaskell
import qualified Ide.Plugin.StylishHaskell         as StylishHaskell
#endif

#if brittany
import qualified Ide.Plugin.Brittany               as Brittany
import           Prettyprinter                     (Pretty (pretty))
#endif

data Log
  = LogGhcIde GhcIde.Log
  | LogExample Example.Log
  | LogExample2 Example2.Log
#if tactic
  | LogTactic Tactic.Log
#endif
#if eval
  | LogEval Eval.Log
#endif
#if importLens
  | LogExplicitImports ExplicitImports.Log
#endif
#if refineImports
  | LogRefineImports RefineImports.Log
#endif
#if hlint
  | LogHlint Hlint.Log
#endif
#if alternateNumberFormat
  | LogAlternateNumberFormat AlternateNumberFormat.Log
#endif
  deriving Show

instance Pretty Log where
  pretty = \case
    LogGhcIde ghcIdeLog -> pretty ghcIdeLog
    LogExample exampleLog -> pretty exampleLog
    LogExample2 example2Log -> pretty example2Log
#if tactic
    LogTactic tacticLog -> pretty tacticLog
#endif
#if eval
    LogEval evalLog -> pretty evalLog
#endif
#if importLens
    LogExplicitImports explicitImportsLog -> pretty explicitImportsLog
#endif
#if refineImports
    LogRefineImports refineImportsLog -> pretty refineImportsLog
#endif
#if hlint
    LogHlint hlintLog -> pretty hlintLog
#endif
#if alternateNumberFormat
    LogAlternateNumberFormat alternateNumberFormatLog -> pretty alternateNumberFormatLog
#endif

-- ---------------------------------------------------------------------

-- | The plugins configured for use in this instance of the language
-- server.
-- These can be freely added or removed to tailor the available
-- features of the server.

idePlugins :: Recorder Log -> Bool -> IdePlugins IdeState
idePlugins recorder includeExamples = pluginDescToIdePlugins allPlugins
  where
    allPlugins = if includeExamples
                   then basePlugins ++ examplePlugins
                   else basePlugins
    basePlugins =
#if pragmas
      Pragmas.descriptor  "pragmas" :
#endif
#if floskell
      Floskell.descriptor "floskell" :
#endif
#if fourmolu
      Fourmolu.descriptor "fourmolu" :
#endif
#if tactic
      Tactic.descriptor (cmap LogTactic recorder) "tactics" :
#endif
#if ormolu
      Ormolu.descriptor   "ormolu" :
#endif
#if stylishHaskell
      StylishHaskell.descriptor "stylish-haskell" :
#endif
#if rename
      Rename.descriptor "rename" :
#endif
#if retrie
      Retrie.descriptor "retrie" :
#endif
#if brittany
      Brittany.descriptor "brittany" :
#endif
#if callHierarchy
      CallHierarchy.descriptor "callHierarchy":
#endif
#if class
      Class.descriptor "class" :
#endif
#if haddockComments
      HaddockComments.descriptor "haddockComments" :
#endif
#if eval
      Eval.descriptor (cmap LogEval recorder) "eval" :
#endif
#if importLens
      ExplicitImports.descriptor (cmap LogExplicitImports recorder) "importLens" :
#endif
#if qualifyImportedNames
      QualifyImportedNames.descriptor "qualifyImportedNames" :
#endif
#if refineImports
      RefineImports.descriptor (cmap LogRefineImports recorder) "refineImports" :
#endif
#if moduleName
      ModuleName.descriptor "moduleName" :
#endif
#if hlint
      Hlint.descriptor (cmap LogHlint recorder) "hlint" :
#endif
#if splice
      Splice.descriptor "splice" :
#endif
#if alternateNumberFormat
      AlternateNumberFormat.descriptor (cmap LogAlternateNumberFormat recorder) "alternateNumberFormat" :
#endif
    -- The ghcide descriptors should come last so that the notification handlers
    -- (which restart the Shake build) run after everything else
      GhcIde.descriptors (cmap LogGhcIde recorder)
    examplePlugins =
      [Example.descriptor  (cmap LogExample recorder) "eg"
      ,Example2.descriptor (cmap LogExample2 recorder) "eg2"
      ]
