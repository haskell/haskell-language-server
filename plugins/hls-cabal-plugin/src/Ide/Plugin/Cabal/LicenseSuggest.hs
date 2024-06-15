{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
module Ide.Plugin.Cabal.LicenseSuggest
( licenseErrorSuggestion
, licenseErrorAction
, licenseNames
  -- * Re-exports
, T.Text
, Diagnostic(..)
)
where

import qualified Data.Map                    as Map
import qualified Data.Text                   as T
import           Language.LSP.Protocol.Types (CodeAction (CodeAction),
                                              CodeActionKind (CodeActionKind_QuickFix),
                                              Diagnostic (..),
                                              Position (Position),
                                              Range (Range),
                                              TextEdit (TextEdit), Uri,
                                              WorkspaceEdit (WorkspaceEdit))
import           Text.Regex.TDFA

import qualified Data.List                   as List
import           Distribution.SPDX.LicenseId (licenseId)
import qualified Text.Fuzzy.Parallel         as Fuzzy

-- | Given a diagnostic returned by 'Ide.Plugin.Cabal.Diag.errorDiagnostic',
--   if it represents an "Unknown SPDX license identifier"-error along
--   with a suggestion, then return a 'CodeAction' for replacing the
--   the incorrect license identifier with the suggestion.
licenseErrorAction
  :: Int -- ^ Maximum number of suggestions to return
  -> Uri -- ^ File for which the diagnostic was generated
  -> Diagnostic -- ^ Output of 'Ide.Plugin.Cabal.Diag.errorDiagnostic'
  -> [CodeAction]
licenseErrorAction maxCompletions uri diag =
  mkCodeAction <$> licenseErrorSuggestion maxCompletions (_message diag)
  where
    mkCodeAction (original, suggestion) =
      let
        -- The Cabal parser does not output the _range_ of the incorrect license identifier,
        -- only a single source code position. Consequently, in 'Ide.Plugin.Cabal.Diag.errorDiagnostic'
        -- we define the range to be from the returned position the first column of the next line.
        -- Since the "replace" code action replaces this range, we need to modify the range to
        -- start at the first character of the invalid license identifier. We achieve this by
        -- subtracting the length of the identifier from the beginning of the range.
        adjustRange (Range (Position line col) rangeTo) =
          Range (Position line (col - fromIntegral (T.length original))) rangeTo
        title = "Replace with " <> suggestion
        -- We must also add a newline character to the replacement since the range returned by
        -- 'Ide.Plugin.Cabal.Diag.errorDiagnostic' ends at the beginning of the following line.
        tedit = [TextEdit (adjustRange $ _range diag) (suggestion <> "\n")]
        edit  = WorkspaceEdit (Just $ Map.singleton uri tedit) Nothing Nothing
      in CodeAction title (Just CodeActionKind_QuickFix) (Just []) Nothing Nothing (Just edit) Nothing Nothing

-- | License name of every license supported by cabal
licenseNames :: [T.Text]
licenseNames = map (T.pack . licenseId) [minBound .. maxBound]

-- | Given a diagnostic returned by 'Ide.Plugin.Cabal.Diag.errorDiagnostic',
--   provide possible corrections for SPDX license identifiers
--   based on the list specified in Cabal.
--   Results are sorted by best fit, and prefer solutions that have smaller
--   length distance to the original word.
--
-- >>> licenseErrorSuggestion 2 (T.pack "Unknown SPDX license identifier: 'BSD3'")
-- [("BSD3","BSD-3-Clause"),("BSD3","BSD-3-Clause-LBNL")]
licenseErrorSuggestion ::
  Int -- ^ Maximum number of suggestions to return
  -> T.Text -- ^ Output of 'Ide.Plugin.Cabal.Diag.errorDiagnostic'
  -> [(T.Text, T.Text)]
  -- ^ (Original (incorrect) license identifier, suggested replacement)
licenseErrorSuggestion maxCompletions msg  =
   (getMatch <$> msg =~~ regex) >>= \case
          [original] ->
            let matches = map Fuzzy.original $ Fuzzy.simpleFilter Fuzzy.defChunkSize maxCompletions original licenseNames
            in [(original,candidate) | candidate <- List.sortOn (lengthDistance original) matches]
          _ -> []
  where
    regex :: T.Text
    regex = "Unknown SPDX license identifier: '(.*)'"
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [T.Text]
    getMatch (_, _, _, results) = results
    lengthDistance original x = abs $ T.length original - T.length x
