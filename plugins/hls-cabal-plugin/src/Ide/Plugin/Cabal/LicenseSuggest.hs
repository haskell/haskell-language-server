{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
module Ide.Plugin.Cabal.LicenseSuggest
( licenseErrorSuggestion
, licenseErrorAction
  -- * Re-exports
, T.Text
, Diagnostic(..)
)
where

import qualified Data.HashMap.Strict as Map
import qualified Data.Text           as T
import           Language.LSP.Types  (CodeAction (CodeAction),
                                      CodeActionKind (CodeActionQuickFix),
                                      Diagnostic (..), List (List),
                                      Position (Position), Range (Range),
                                      TextEdit (TextEdit), Uri,
                                      WorkspaceEdit (WorkspaceEdit))
import           Text.Regex.TDFA

-- | Given a diagnostic returned by 'Ide.Plugin.Cabal.Diag.errorDiagnostic',
--   if it represents an "Unknown SPDX license identifier"-error along
--   with a suggestion, then return a 'CodeAction' for replacing the
--   the incorrect license identifier with the suggestion.
licenseErrorAction
  :: Uri
  -- ^ File for which the diagnostic was generated
  -> Diagnostic
  -- ^ Output of 'Ide.Plugin.Cabal.Diag.errorDiagnostic'
  -> Maybe CodeAction
licenseErrorAction uri diag =
  mkCodeAction <$> licenseErrorSuggestion diag
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
        edit  = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing Nothing
      in CodeAction title (Just CodeActionQuickFix) (Just $ List []) Nothing Nothing (Just edit) Nothing Nothing

-- | Given a diagnostic returned by 'Ide.Plugin.Cabal.Diag.errorDiagnostic',
--   if it represents an "Unknown SPDX license identifier"-error along
--   with a suggestion then return the suggestion (after the "Do you mean"-text)
--   along with the incorrect identifier.
licenseErrorSuggestion
  :: Diagnostic
  -- ^ Output of 'Ide.Plugin.Cabal.Diag.errorDiagnostic'
  -> Maybe (T.Text, T.Text)
  -- ^ (Original (incorrect) license identifier, suggested replacement)
licenseErrorSuggestion diag =
  mSuggestion (_message diag) >>= \case
    [original, suggestion] -> Just (original, suggestion)
    _                      -> Nothing
  where
    regex :: T.Text
    regex = "Unknown SPDX license identifier: '(.*)' Do you mean (.*)\\?"
    mSuggestion msg = getMatch <$> (msg :: T.Text) =~~ regex
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [T.Text]
    getMatch (_, _, _, results) = results
