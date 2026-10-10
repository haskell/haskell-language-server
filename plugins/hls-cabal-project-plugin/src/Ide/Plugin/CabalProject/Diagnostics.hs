{-# LANGUAGE OverloadedStrings #-}

-- | Schema validation diagnostics for @cabal.project@ files: unknown fields
-- and unknown sections.
module Ide.Plugin.CabalProject.Diagnostics
  ( checkProjectFields
  , warningDiagnostic
  , fatalParseErrorDiagnostic
  , FileDiagnostic
  ) where

import           Control.Lens                            ((&), (.~))
import qualified Data.ByteString.Char8                   as BS
import qualified Data.Map.Strict                         as Map
import           Data.Maybe                              (fromMaybe)
import qualified Data.Text                               as T
import           Development.IDE                         (FileDiagnostic,
                                                          NormalizedFilePath)
import           Development.IDE.Types.Diagnostics       (fdLspDiagnosticL,
                                                          ideErrorWithSource)
import qualified Distribution.Fields                     as Syntax
import qualified Distribution.Parsec                     as Syntax
import           Ide.Plugin.Cabal.Completion.CabalFields (getAnnotation)
import qualified Ide.Plugin.Cabal.Diagnostics            as CabalDiagnostics
import qualified Ide.Plugin.CabalProject.Data            as Data
import           Ide.PluginUtils                         (extendNextLine)
import           Language.LSP.Protocol.Lens              (range)
import           Language.LSP.Protocol.Types             (Diagnostic (..),
                                                          DiagnosticSeverity (..),
                                                          Position,
                                                          Range (Range))

-- | Diagnostic source for this plugin.
sourceName :: T.Text
sourceName = "cabal-project"

-- | Produce a diagnostic for a fatal Cabal parse error.
fatalParseErrorDiagnostic :: NormalizedFilePath -> T.Text -> FileDiagnostic
fatalParseErrorDiagnostic fp msg =
  mkDiag fp sourceName DiagnosticSeverity_Error (toBeginningOfNextLine Syntax.zeroPos) msg

-- | Produce a warning diagnostic at the given (Cabal, 1-based) position.
warningDiagnostic :: NormalizedFilePath -> Syntax.Position -> T.Text -> FileDiagnostic
warningDiagnostic fp pos msg =
  mkDiag fp sourceName DiagnosticSeverity_Warning (toBeginningOfNextLine pos) msg

-- | The Cabal parser does not output a _range_ for a warning/error,
-- only a single source code 'Syntax.Position'.
-- We define the range to be _from_ this position
-- _to_ the first column of the next line.
toBeginningOfNextLine :: Syntax.Position -> Range
toBeginningOfNextLine cabalPos = extendNextLine $ Range pos pos
  where
    pos = CabalDiagnostics.positionFromCabalPosition cabalPos

-- | Create a 'FileDiagnostic'.
mkDiag
  :: NormalizedFilePath
  -- ^ Project file path
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

-- | The stanza context a field or section appears in.
data StanzaCtx = TopLevelCtx | InStanza T.Text
  deriving (Eq)

-- | Checks the given top-level fields of a project file for unknown fields
-- and unknown sections. Emits a warning for each, positioned at the field or
-- section name.
--
-- Unknown fields or sections are only a warning, since the set of valid
-- fields varies across cabal versions; false errors are worse than missing
-- errors.
checkProjectFields :: NormalizedFilePath -> [Syntax.Field Syntax.Position] -> [FileDiagnostic]
checkProjectFields fp = go TopLevelCtx
  where
    go ctx fields = concatMap (checkField ctx) fields

    knownFields TopLevelCtx  = Just Data.projectTopLevelFields
    knownFields (InStanza s) = Map.lookup s Data.projectStanzaKeywordMap

    checkField ctx field@(Syntax.Field name _lines) =
      let fieldName = stripColon (fieldNameOf name)
       in case knownFields ctx >>= Map.lookup (fieldName <> ":") of
            Just _ -> []
            Nothing ->
              [warningDiagnostic fp (getAnnotation field) (unknownFieldMsg ctx fieldName)]
    checkField ctx section@(Syntax.Section name _ subs)
      | sectionName `elem` ["if", "elif", "else"] = go ctx subs
      | sectionName `elem` Data.projectSectionNames =
          go (InStanza sectionName) subs
      | otherwise =
          [warningDiagnostic fp (getAnnotation section) (unknownSectionMsg sectionName)]
      where
        sectionName = fieldNameOf name

    unknownFieldMsg TopLevelCtx n = "Unknown field '" <> n <> "' in cabal.project"
    unknownFieldMsg (InStanza s) n = "Unknown field '" <> n <> "' in section '" <> s <> "'"

    unknownSectionMsg n = "Unknown section '" <> n <> "' in cabal.project"

-- | Extracts the textual name of a cabal field/section name.
fieldNameOf :: Syntax.Name Syntax.Position -> T.Text
fieldNameOf (Syntax.Name _ n) = T.pack (BS.unpack n)

-- | Drops a trailing colon from a field name, if present.
stripColon :: T.Text -> T.Text
stripColon t = fromMaybe t (T.stripSuffix ":" t)
