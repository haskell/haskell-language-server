{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs      #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Development.IDE.Plugin where

import Data.Default
import Development.Shake

import           Language.LSP.Types
import Development.IDE.Core.Rules
import Development.IDE.LSP.Server
import qualified Language.LSP.Server as LSP

data Plugin c = Plugin
    {pluginRules :: Rules ()
    ,pluginHandlers :: LSP.Handlers (ServerM c)
    }

instance Default (Plugin c) where
    def = Plugin mempty mempty

instance Semigroup (Plugin c) where
    Plugin x1 h1 <> Plugin x2 h2 = Plugin (x1<>x2) (h1 <> h2)

instance Monoid (Plugin c) where
    mempty = def
