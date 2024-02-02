{-# LANGUAGE TypeFamilies #-}

module Ide.Plugin.Cabal.Completion.Completer.Types where

import           Development.IDE                   as D
import           Distribution.PackageDescription   (GenericPackageDescription)
import           Ide.Plugin.Cabal.Completion.Types
import           Language.LSP.Protocol.Types       (CompletionItem)

-- | Takes information needed to build possible completion items
-- and returns the list of possible completion items
type Completer = Recorder (WithPriority Log) -> CompleterData -> IO [CompletionItem]

-- | Contains information to be used by completers.
data CompleterData = CompleterData
  { -- | Access to the latest available generic package description for the handled cabal file,
    -- relevant for some completion actions which require the file's meta information
    -- such as the module completers which require access to source directories
    getLatestGPD    :: IO (Maybe GenericPackageDescription),
    -- | Prefix info to be used for constructing completion items
    cabalPrefixInfo :: CabalPrefixInfo,
    -- | The name of the stanza in which the completer is applied
    stanzaName      :: Maybe StanzaName
  }
