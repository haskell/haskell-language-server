{-# LANGUAGE TypeFamilies #-}

module Ide.Plugin.Cabal.Completion.Completer.Types where

import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Development.IDE                   as D
import qualified Distribution.Fields               as Syntax
import           Distribution.PackageDescription   (GenericPackageDescription)
import qualified Distribution.Parsec.Position      as Syntax
import           Ide.Plugin.Cabal.Completion.Types
import           Language.LSP.Protocol.Types       (CompletionItem)
import           Text.Fuzzy.Parallel

-- | Takes information needed to build possible completion items
-- and returns the list of possible completion items
type Completer = Recorder (WithPriority Log) -> CompleterData -> IO [CompletionItem]

-- | Type signature of completion functions ranking texts against a pattern.
newtype Matcher a = Matcher { runMatcher :: T.Text -> [T.Text] -> [Scored a] }

-- | Contains information to be used by completers.
data CompleterData = CompleterData
  { -- | Access to the latest available generic package description for the handled cabal file,
    -- relevant for some completion actions which require the file's meta information
    -- such as the module completers which require access to source directories
    getLatestGPD           :: IO (Maybe GenericPackageDescription),
    -- | Access to the entries of the handled cabal file as parsed by ParseCabalFields
    getCabalCommonSections :: IO (Maybe [Syntax.Field Syntax.Position]),
    -- | Prefix info to be used for constructing completion items
    cabalPrefixInfo        :: CabalPrefixInfo,
    -- | The name of the stanza in which the completer is applied
    stanzaName             :: Maybe StanzaName,
    -- | The matcher that'll be used to rank the texts against the pattern.
    matcher                :: Matcher Text
  }
