{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Ide.Plugin.Cabal.Diagnostics
( errorDiagnostic
, warningDiagnostic
, positionFromCabalPosition
, fatalParseErrorDiagnostic
  -- * Re-exports
, FileDiagnostic
, Diagnostic(..)
)
where

import           Control.Lens                      ((&), (.~))
import qualified Data.Text                         as T
import           Development.IDE                   (FileDiagnostic)
import           Development.IDE.Types.Diagnostics (fdLspDiagnosticL,
                                                    ideErrorWithSource)
import           Distribution.Fields               (showPError, showPWarning)
import qualified Distribution.Parsec               as Syntax
import           Ide.PluginUtils                   (extendNextLine)
import           Language.LSP.Protocol.Lens        (range)
import           Language.LSP.Protocol.Types       (Diagnostic (..),
                                                    DiagnosticSeverity (..),
                                                    NormalizedFilePath,
                                                    Position (Position),
                                                    Range (Range),
                                                    fromNormalizedFilePath)

-- | Produce a diagnostic for a fatal Cabal parser error.
fatalParseErrorDiagnostic :: NormalizedFilePath -> T.Text -> FileDiagnostic
fatalParseErrorDiagnostic fp msg =
  mkDiag fp "cabal" DiagnosticSeverity_Error (toBeginningOfNextLine Syntax.zeroPos) msg

-- | Produce a diagnostic from a Cabal parser error
errorDiagnostic :: NormalizedFilePath -> Syntax.PError -> FileDiagnostic
errorDiagnostic fp err@(Syntax.PError pos _) =
  mkDiag fp "cabal" DiagnosticSeverity_Error (toBeginningOfNextLine pos) msg
  where
    msg = T.pack $ showPError (fromNormalizedFilePath fp) err

-- | Produce a diagnostic from a Cabal parser warning
warningDiagnostic :: NormalizedFilePath -> Syntax.PWarning -> FileDiagnostic
warningDiagnostic fp warning@(Syntax.PWarning _ pos _) =
  mkDiag fp "cabal" DiagnosticSeverity_Warning (toBeginningOfNextLine pos) msg
  where
    msg = T.pack $ showPWarning (fromNormalizedFilePath fp) warning

-- | The Cabal parser does not output a _range_ for a warning/error,
-- only a single source code 'Lib.Position'.
-- We define the range to be _from_ this position
-- _to_ the first column of the next line.
toBeginningOfNextLine :: Syntax.Position -> Range
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
positionFromCabalPosition :: Syntax.Position -> Position
positionFromCabalPosition (Syntax.Position line column) = Position (fromIntegral line') (fromIntegral col')
  where
    -- LSP is zero-based, Cabal is one-based
    -- Cabal can return line 0 for errors in the first line
    line' = if line <= 0 then 0 else line-1
    col' = if column <= 0 then 0 else column-1

-- | Create a 'FileDiagnostic'
mkDiag
  :: NormalizedFilePath
  -- ^ Cabal file path
  -> T.Text
  -- ^ Where does the diagnostic come from?
  -> DiagnosticSeverity
  -- ^ Severity
  -> Range
  -- ^ Which source code range should the editor highlight?
  -> T.Text
  -- ^ The message displayed by the editor
  -> FileDiagnostic
mkDiag file diagSource sev loc msg =
  ideErrorWithSource
    (Just diagSource)
    (Just sev)
    file
    msg
    Nothing
    & fdLspDiagnosticL . range .~ loc
