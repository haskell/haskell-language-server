module Development.IDE.Plugin ( Plugin(..) ) where

import           Data.Default
import           Development.IDE.Graph

import           Development.IDE.LSP.Server
import qualified Language.LSP.Server        as LSP
import Development.IDE.GHC.Compat (DynFlags)
import Data.Monoid (Endo)
import Ide.Types (DynFlagsModifications)

data Plugin c = Plugin
    {pluginRules    :: Rules ()
    ,pluginHandlers :: LSP.Handlers (ServerM c)
    ,pluginModifyDynflags :: DynFlagsModifications
    }

instance Default (Plugin c) where
    def = Plugin mempty mempty mempty

instance Semigroup (Plugin c) where
    Plugin x1 h1 d1 <> Plugin x2 h2 d2 = Plugin (x1<>x2) (h1 <> h2) (d1 <> d2)

instance Monoid (Plugin c) where
    mempty = def
