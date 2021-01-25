module Development.IDE.Plugin
  ( Plugin(..)
  ) where

import Data.Default
import Development.Shake
import Development.IDE.LSP.Server


data Plugin c = Plugin
    {pluginRules :: Rules ()
    ,pluginHandler :: PartialHandlers c
    }

instance Default (Plugin c) where
    def = Plugin mempty def

instance Semigroup (Plugin c) where
    Plugin x1 y1 <> Plugin x2 y2 = Plugin (x1<>x2) (y1<>y2)

instance Monoid (Plugin c) where
    mempty = def
