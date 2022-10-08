{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
module Ide.Plugin.Cabal.FieldSuggest
( fieldErrorSuggestion
, fieldErrorAction
  -- * Re-exports
, T.Text
, Diagnostic(..)
)
where

import qualified Data.Map.Strict             as Map
import qualified Data.Text                   as T
import           Language.LSP.Protocol.Types (CodeAction (CodeAction),
                                              CodeActionKind (..),
                                              Diagnostic (..),
                                              Position (Position),
                                              Range (Range),
                                              TextEdit (TextEdit), Uri,
                                              WorkspaceEdit (WorkspaceEdit))
import           Text.Regex.TDFA

-- | Given a diagnostic returned by 'Ide.Plugin.Cabal.Diag.errorDiagnostic',
--   if it represents an "Unknown field"-error along
--   with a incorrect field, then return a 'CodeAction' for replacing the
--   the incorrect field with the suggestion.
--   It should be context sensitive, but for now it isn't
fieldErrorAction
  :: Uri
  -- ^ File for which the diagnostic was generated
  -> Diagnostic
  -- ^ Output of 'Ide.Plugin.Cabal.Diag.errorDiagnostic'
  ->  [CodeAction]
fieldErrorAction uri diag =
  mkCodeAction <$> fieldErrorSuggestion diag
  where
    mkCodeAction (original, suggestion) =
      let
        -- Range returned by cabal here represents fragment from start of
        -- offending identifier to end of line, we modify it to the end of identifier
        adjustRange (Range rangeFrom@(Position line col) _) =
          Range rangeFrom (Position line (col + fromIntegral (T.length original)))
        title = "Replace with " <> suggestion
        tedit = [TextEdit (adjustRange $ _range diag) suggestion]
        edit  = WorkspaceEdit (Just $ Map.singleton uri tedit) Nothing Nothing
      in CodeAction title (Just CodeActionKind_QuickFix) (Just []) Nothing Nothing (Just edit) Nothing Nothing

-- | Given a diagnostic returned by 'Ide.Plugin.Cabal.Diag.errorDiagnostic',
--   if it represents an "Unknown field"- error with incorrect identifier
--   then return the suggestion (for now placeholder "name")
--   along with the incorrect identifier.
--
fieldErrorSuggestion
  :: Diagnostic
  -- ^ Output of 'Ide.Plugin.Cabal.Diag.errorDiagnostic'
  -> [(T.Text, T.Text)]
  -- ^ (Original (incorrect) license identifier, suggested replacement)
fieldErrorSuggestion diag =
  mSuggestion (_message diag) >>= \case
    [original] -> [(original, "name")]
    _                      -> []
  where
    regex :: T.Text
    regex = "Unknown field: \"(.*)\""
    mSuggestion msg = getMatch <$> (msg :: T.Text) =~~ regex
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [T.Text]
    getMatch (_, _, _, results) = results
