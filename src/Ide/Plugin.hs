{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Plugin
    (
      asGhcIdePlugin
    ) where

import           Data.Aeson                    hiding (defaultOptions)
import qualified Data.Map  as Map
import qualified Data.Set                      as S
import           Data.String
import qualified Data.Text                     as T
import           Data.Typeable
import           Development.IDE.Core.Rules
import           Development.IDE.Types.Diagnostics as D
import           Development.IDE.Types.Location
import           Language.Haskell.LSP.Types
import           Text.Regex.TDFA.Text()
import           Development.IDE.Plugin
import           Ide.Plugin.Config
import           Ide.Types

-- ---------------------------------------------------------------------

-- | Map a set of plugins to the underlying ghcide engine.  Main point is
-- IdePlugins are arranged by kind of operation, 'Plugin' is arranged by message
-- category ('Notifaction', 'Request' etc).
asGhcIdePlugin :: IdePlugins -> Plugin Config
asGhcIdePlugin _ = Plugin mempty mempty

-- First strp will be to bring the machinery from Ide.Plugin.Formatter over.

-- ---------------------------------------------------------------------
