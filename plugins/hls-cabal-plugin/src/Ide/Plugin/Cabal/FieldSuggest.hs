{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Cabal.FieldSuggest
  ( fieldErrorName,

    -- * Re-exports
    T.Text,
    Diagnostic (..),
  )
where

import qualified Data.Text as T
import Language.LSP.Types
  ( Diagnostic (..),
  )
import Text.Regex.TDFA

-- | Given a diagnostic returned by 'Ide.Plugin.Cabal.Diag.errorDiagnostic',
--   if it represents an "Unknown field"- error with incorrect identifier
--   then return the incorrect identifier together with original diagnostics.
fieldErrorName ::
  -- | Output of 'Ide.Plugin.Cabal.Diag.errorDiagnostic'
  Diagnostic ->
  -- | (Original (incorrect) license identifier, suggested replacement)
  Maybe (T.Text, Diagnostic)
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
