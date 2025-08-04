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

import qualified Data.Text                    as T
import           Development.IDE              (FileDiagnostic)
import qualified Distribution.Parsec          as Syntax
import           Distribution.Parsec.Error    (showPError)
import           Distribution.Parsec.Warning  (showPWarning)
import           Ide.Plugin.Cabal.Diagnostics (mkDiag,
                                               positionFromCabalPosition,
                                               toBeginningOfNextLine)
import           Language.LSP.Protocol.Types  (Diagnostic (..),
                                               DiagnosticSeverity (..),
                                               NormalizedFilePath,
                                               fromNormalizedFilePath)

-- | Produce a diagnostic for a fatal cabal.project parser error.
fatalParseErrorDiagnostic :: NormalizedFilePath -> T.Text -> FileDiagnostic
fatalParseErrorDiagnostic fp msg =
  mkDiag fp "cabal-project" DiagnosticSeverity_Error (toBeginningOfNextLine Syntax.zeroPos) msg

-- | Produce a diagnostic from a cabal.project parser error
errorDiagnostic :: NormalizedFilePath -> Syntax.PError -> FileDiagnostic
errorDiagnostic fp err@(Syntax.PError pos _) =
  mkDiag fp "cabal-project" DiagnosticSeverity_Error (toBeginningOfNextLine pos) msg
  where
    msg = T.pack $ showPError (fromNormalizedFilePath fp) err

-- | Produce a diagnostic from a cabal.project parser warning
warningDiagnostic :: NormalizedFilePath -> Syntax.PWarning -> FileDiagnostic
warningDiagnostic fp warning@(Syntax.PWarning _ pos _) =
  mkDiag fp "cabal-project" DiagnosticSeverity_Warning (toBeginningOfNextLine pos) msg
  where
    msg = T.pack $ showPWarning (fromNormalizedFilePath fp) warning
