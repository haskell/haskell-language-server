{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TupleSections         #-}
module Ide.Plugin.Cabal.Diagnostics
( errorDiagnostic
, warningDiagnostic
, positionFromCabalPosition
  -- * Re-exports
, FileDiagnostic
, Diagnostic(..)
)
where

import qualified Data.Text              as T
import           Development.IDE        (FileDiagnostic,
                                         ShowDiagnostic (ShowDiag))
import           Distribution.Fields    (showPError, showPWarning)
import qualified Ide.Plugin.Cabal.Parse as Lib
import           Ide.PluginUtils        (extendNextLine)
import           Language.LSP.Types     (Diagnostic (..),
                                         DiagnosticSeverity (..),
                                         DiagnosticSource, NormalizedFilePath,
                                         Position (Position), Range (Range),
                                         fromNormalizedFilePath)

-- | Produce a diagnostic from a Cabal parser error
errorDiagnostic :: NormalizedFilePath -> Lib.PError -> FileDiagnostic
errorDiagnostic fp err@(Lib.PError pos _) =
  mkDiag fp "cabal" DsError (toBeginningOfNextLine pos) msg
  where
    msg = T.pack $ showPError (fromNormalizedFilePath fp) err

-- | Produce a diagnostic from a Cabal parser warning
warningDiagnostic :: NormalizedFilePath -> Lib.PWarning -> FileDiagnostic
warningDiagnostic fp warning@(Lib.PWarning _ pos _) =
  mkDiag fp "cabal" DsWarning (toBeginningOfNextLine pos) msg
  where
    msg = T.pack $ showPWarning (fromNormalizedFilePath fp) warning

-- | The Cabal parser does not output a _range_ for a warning/error,
-- only a single source code 'Lib.Position'.
-- We define the range to be _from_ this position
-- _to_ the first column of the next line.
toBeginningOfNextLine :: Lib.Position -> Range
toBeginningOfNextLine cabalPos = extendNextLine $ Range pos pos
   where
    pos = positionFromCabalPosition cabalPos

-- | Convert a 'Lib.Position' from Cabal to a 'Range' that LSP understands.
--
-- Prefer this function over hand-rolled unpacking/packing, since LSP is zero-based,
-- while Cabal is one-based.
--
-- >>> positionFromCabalPosition $ Lib.Position 1 1
-- Position 0 0
positionFromCabalPosition :: Lib.Position -> Position
positionFromCabalPosition (Lib.Position line column) = Position (fromIntegral line') (fromIntegral col')
  where
    -- LSP is zero-based, Cabal is one-based
    line' = line-1
    col' = column-1

-- | Create a 'FileDiagnostic'
mkDiag
  :: NormalizedFilePath
  -- ^ Cabal file path
  -> DiagnosticSource
  -- ^ Where does the diagnostic come from?
  -> DiagnosticSeverity
  -- ^ Severity
  -> Range
  -- ^ Which source code range should the editor highlight?
  -> T.Text
  -- ^ The message displayed by the editor
  -> FileDiagnostic
mkDiag file diagSource sev loc msg = (file, ShowDiag,)
    Diagnostic
    { _range    = loc
    , _severity = Just sev
    , _source   = Just diagSource
    , _message  = msg
    , _code     = Nothing
    , _tags     = Nothing
    , _relatedInformation = Nothing
    }
