{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
module Plugins where

import           Ide.Types                 (IdePlugins)
import           Ide.PluginUtils           (pluginDescToIdePlugins)

-- fixed plugins
import           Ide.Plugin.Example        as Example
import           Ide.Plugin.Example2       as Example2
import           Development.IDE           (IdeState)
import           Development.IDE.Plugin.HLS.GhcIde as GhcIde

-- haskell-language-server optional plugins

#if class
import           Ide.Plugin.Class          as Class
#endif

#if eval
import           Ide.Plugin.Eval           as Eval
#endif

#if importLens
import           Ide.Plugin.ExplicitImports as ExplicitImports
#endif

#if retrie
import           Ide.Plugin.Retrie         as Retrie
#endif

#if tactic
import           Ide.Plugin.Tactic         as Tactic
#endif

#if hlint
import           Ide.Plugin.Hlint          as Hlint
#endif

#if moduleName
import           Ide.Plugin.ModuleName     as ModuleName
#endif

#if pragmas
import           Ide.Plugin.Pragmas        as Pragmas
#endif

-- formatters

#if floskell
import           Ide.Plugin.Floskell       as Floskell
#endif

#if fourmolu
import           Ide.Plugin.Fourmolu       as Fourmolu
#endif

#if ormolu
import           Ide.Plugin.Ormolu         as Ormolu
#endif

#if stylishHaskell
import           Ide.Plugin.StylishHaskell as StylishHaskell
#endif

#if AGPL && brittany
import           Ide.Plugin.Brittany       as Brittany
#endif

-- ---------------------------------------------------------------------

-- | The plugins configured for use in this instance of the language
-- server.
-- These can be freely added or removed to tailor the available
-- features of the server.

idePlugins :: Bool -> IdePlugins IdeState
idePlugins includeExamples = pluginDescToIdePlugins allPlugins
  where
    allPlugins = if includeExamples
                   then basePlugins ++ examplePlugins
                   else basePlugins
    basePlugins =
      [ GhcIde.descriptor  "ghcide"
#if pragmas
      , Pragmas.descriptor  "pragmas"
#endif
#if floskell
      , Floskell.descriptor "floskell"
#endif
#if fourmolu
      , Fourmolu.descriptor "fourmolu"
#endif
#if tactic
      , Tactic.descriptor "tactic"
#endif
#if ormolu
      , Ormolu.descriptor   "ormolu"
#endif
#if stylishHaskell
      , StylishHaskell.descriptor "stylish-haskell"
#endif
#if retrie
      , Retrie.descriptor "retrie"
#endif
#if AGPL && brittany
      , Brittany.descriptor "brittany"
#endif
#if class
      , Class.descriptor "class"
#endif
#if eval
      , Eval.descriptor "eval"
#endif
#if importLens
      , ExplicitImports.descriptor "importLens"
#endif
#if moduleName
      , ModuleName.descriptor "moduleName"
#endif
#if hlint
      , Hlint.descriptor "hlint"
#endif
      ]
    examplePlugins =
      [Example.descriptor  "eg"
      ,Example2.descriptor "eg2"
      ]
