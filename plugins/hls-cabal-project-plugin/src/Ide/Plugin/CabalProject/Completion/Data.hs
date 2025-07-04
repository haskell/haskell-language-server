{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.CabalProject.Completion.Data where

import           Data.Map                                     (Map)
import qualified Data.Map                                     as Map
import qualified Data.Text                                    as T
import           Development.IDE.GHC.Compat.Core              (flagsForCompletion)
import           Distribution.CabalSpecVersion                (CabalSpecVersion (CabalSpecV2_2),
                                                               showCabalSpecVersion)
-- import           Ide.Plugin.Cabal.Completion.Completer.FilePath
-- import           Ide.Plugin.Cabal.Completion.Completer.Module
-- import           Ide.Plugin.Cabal.Completion.Completer.Paths
import           Ide.Plugin.Cabal.Completion.Completer.Simple
import           Ide.Plugin.Cabal.Completion.Completer.Types  (Completer)
import           Ide.Plugin.Cabal.Completion.Types
-- import           Ide.Plugin.Cabal.LicenseSuggest                (licenseNames)

-- ----------------------------------------------------------------
-- Completion Data
-- ----------------------------------------------------------------

-- supportedCabalVersions :: [CabalSpecVersion]
-- supportedCabalVersions = [CabalSpecV2_2 .. maxBound]

-- -- | Keyword for cabal version; required to be the top line in a cabal file
-- cabalVersionKeyword :: Map KeyWordName Completer
-- cabalVersionKeyword =
--   Map.singleton "cabal-version:" $
--     constantCompleter $
--       -- We only suggest cabal versions newer than 2.2
--       -- since we don't recommend using older ones.
--       map (T.pack . showCabalSpecVersion) supportedCabalVersions

-- | Top level keywords of a cabal file.
--
-- TODO: we could add descriptions of field values and
-- then show them when inside the field's context
cabalProjectKeywords :: Map KeyWordName Completer
cabalProjectKeywords =
  Map.fromList
    [ ("packages:", constantCompleter []),
      ("optional-packages:", constantCompleter []),
      ("extra-packages:", constantCompleter []),
      ("verbose:", constantCompleter []),
      ("build-summary:", constantCompleter []),
      ("build-log:", constantCompleter []),
      ("remote-build-reporting:", constantCompleter []),
      ("report-planning-failure:", constantCompleter []),
      ("symlink-bindir:", constantCompleter []),
      ("jobs:", constantCompleter []),
      ("semaphore:", constantCompleter []),
      ("keep-going:", constantCompleter []),
      ("offline:", constantCompleter []),
      ("haddock-keep-temp-files:", constantCompleter []),
      ("http-transport:", constantCompleter []),
      ("ignore-expiry:", constantCompleter []),
      ("remote-repo-cache:", constantCompleter []),
      ("logs-dir:", constantCompleter [])
      -- add projectConfigSharedFieldGrammar,
    ]

packageFields :: Map KeyWordName Completer
packageFields =
  Map.fromList
    [ ("haddock-all:", constantCompleter []),
      ("extra-prog-path:", noopCompleter),
      ("flags:", constantCompleter []),
      ("library-vanilla:", constantCompleter []),
      ("shared:", constantCompleter [])
      -- add rest
    ]

-- | Map, containing all stanzas in a cabal file as keys,
--  and lists of their possible nested keywords as values.
stanzaKeywordMap :: Map StanzaType (Map KeyWordName Completer)
stanzaKeywordMap =
  Map.fromList
    [ ("package", packageFields)
    ]
