{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Ide.Plugin.CabalProject.Diagnostics
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
import           Ide.Plugin.Cabal.Diagnostics      (mkDiag,
                                                    positionFromCabalPosition,
                                                    toBeginningOfNextLine)
import           Ide.PluginUtils                   (extendNextLine)
import           Language.LSP.Protocol.Lens        (range)
import           Language.LSP.Protocol.Types       (Diagnostic (..),
                                                    DiagnosticSeverity (..),
                                                    NormalizedFilePath,
                                                    Position (Position),
                                                    Range (Range),
                                                    fromNormalizedFilePath)

-- | Produce a diagnostic for a fatal Cabal Project parser error.
fatalParseErrorDiagnostic :: NormalizedFilePath -> T.Text -> FileDiagnostic
fatalParseErrorDiagnostic fp msg =
  mkDiag fp "cabal-project" DiagnosticSeverity_Error (toBeginningOfNextLine Syntax.zeroPos) msg

-- | Produce a diagnostic from a Cabal Project parser error
errorDiagnostic :: NormalizedFilePath -> Syntax.PError -> FileDiagnostic
errorDiagnostic fp err@(Syntax.PError pos _) =
  mkDiag fp "cabal-project" DiagnosticSeverity_Error (toBeginningOfNextLine pos) msg
  where
    msg = T.pack $ showPError (fromNormalizedFilePath fp) err

-- | Produce a diagnostic from a Cabal Project parser warning
warningDiagnostic :: NormalizedFilePath -> Syntax.PWarning -> FileDiagnostic
warningDiagnostic fp warning@(Syntax.PWarning _ pos _) =
  mkDiag fp "cabal-project" DiagnosticSeverity_Warning (toBeginningOfNextLine pos) msg
  where
    msg = T.pack $ showPWarning (fromNormalizedFilePath fp) warning
