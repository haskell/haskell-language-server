{-# LANGUAGE TypeFamilies #-}

module Ide.Plugin.CabalProject.Completion.Completer.Types where

import           Ide.Plugin.Cabal.Completion.Completer.Types
import           Ide.Plugin.Cabal.Completion.Types

-- Cabal.project specific completer
type CabalProjectCompleter = Completer CabalProjectCompleterData

-- | Contains information to be used by completers.
data CabalProjectCompleterData = CabalProjectCompleterData
  {
    -- | Prefix info to be used for constructing completion items
    cabalProjectPrefixInfo :: CabalPrefixInfo,
    -- | The name of the stanza in which the completer is applied
    cabalProjectStanzaName :: Maybe StanzaName
  }

-- Allows CabalProjectCompleter to be used by Completers
instance HasPrefixInfo CabalProjectCompleterData where
  getPrefixInfo = cabalProjectPrefixInfo
