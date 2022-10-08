{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}

module Ide.Plugin.Cabal.FieldSuggest
  ( fieldErrorName,
    fieldErrorAction,
    -- * Re-exports
    T.Text,
    Diagnostic (..),
  )
where

import qualified Data.Map.Strict             as Map
import qualified Data.Text                   as T
import           Language.LSP.Protocol.Types (CodeAction (..),
                                              CodeActionKind (..),
                                              Diagnostic (..), Position (..),
                                              Range (..), TextEdit (..), Uri,
                                              WorkspaceEdit (..))
import           Text.Regex.TDFA

-- | Generate all code action for given file, error field in position and suggestions
fieldErrorAction
  :: Uri
  -- ^ File for which the diagnostic was generated
  -> T.Text
  -- ^ Original field
  -> [T.Text]
  -- ^ Suggestions
  -> Range
  -- ^ Location of diagnostic
  -> [CodeAction]
fieldErrorAction uri original suggestions range =
  fmap mkCodeAction  suggestions
  where
    mkCodeAction suggestion =
      let
        -- Range returned by cabal here represents fragment from start of
        -- offending identifier to end of line, we modify it to the end of identifier
        adjustRange (Range rangeFrom@(Position lineNr col) _) =
          Range rangeFrom (Position lineNr (col + fromIntegral (T.length original)))
        title = "Replace with " <> suggestion'
        tedit = [TextEdit (adjustRange range ) suggestion']
        edit  = WorkspaceEdit (Just $ Map.singleton uri tedit) Nothing Nothing
      in CodeAction title (Just CodeActionKind_QuickFix) (Just []) Nothing Nothing (Just edit) Nothing Nothing
      where
        -- dropping colon from the end of suggestion
        suggestion' = T.dropEnd 1 suggestion

-- | Given a diagnostic returned by 'Ide.Plugin.Cabal.Diag.errorDiagnostic',
--   if it represents an "Unknown field"- error with incorrect identifier
--   then return the incorrect identifier together with original diagnostics.
fieldErrorName ::
  Diagnostic ->
  -- ^ Output of 'Ide.Plugin.Cabal.Diag.errorDiagnostic'
  Maybe (T.Text, Diagnostic)
  -- ^ Original (incorrect) field name with the suggested replacement
fieldErrorName diag =
  mSuggestion (_message diag) >>= \case
    [original] -> Just (original, diag)
    _ -> Nothing
  where
    regex :: T.Text
    regex = "Unknown field: \"(.*)\""
    mSuggestion msg = getMatch <$> (msg :: T.Text) =~~ regex
    getMatch :: (T.Text, T.Text, T.Text, [T.Text]) -> [T.Text]
    getMatch (_, _, _, results) = results
