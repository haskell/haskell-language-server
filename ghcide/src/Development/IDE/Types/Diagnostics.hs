-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0


module Development.IDE.Types.Diagnostics (
  LSP.Diagnostic(..),
  ShowDiagnostic(..),
  FileDiagnostic,
  IdeResult,
  LSP.DiagnosticSeverity(..),
  DiagnosticStore,
  List(..),
  ideErrorText,
  ideErrorWithSource,
  showDiagnostics,
  showDiagnosticsColored,
  ) where

import Control.DeepSeq
import Data.Maybe as Maybe
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Language.Haskell.LSP.Types as LSP (DiagnosticSource,
    DiagnosticSeverity(..)
  , Diagnostic(..)
  , List(..)
  )
import Language.Haskell.LSP.Diagnostics
import Data.Text.Prettyprint.Doc.Render.Text
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as Terminal
import Data.Text.Prettyprint.Doc.Render.Terminal (Color(..), color)

import Development.IDE.Types.Location

--   A rule on a file should only return diagnostics for that given file. It should
--   not propagate diagnostic errors through multiple phases.
type IdeResult v = ([FileDiagnostic], Maybe v)

ideErrorText :: NormalizedFilePath -> T.Text -> FileDiagnostic
ideErrorText = ideErrorWithSource (Just "compiler") (Just DsError)

ideErrorWithSource
  :: Maybe DiagnosticSource
  -> Maybe DiagnosticSeverity
  -> a
  -> T.Text
  -> (a, ShowDiagnostic, Diagnostic)
ideErrorWithSource source sev fp msg = (fp, ShowDiag, LSP.Diagnostic {
    _range = noRange,
    _severity = sev,
    _code = Nothing,
    _source = source,
    _message = msg,
    _relatedInformation = Nothing,
    _tags = Nothing
    })

-- | Defines whether a particular diagnostic should be reported
--   back to the user.
--
--   One important use case is "missing signature" code lenses,
--   for which we need to enable the corresponding warning during
--   type checking. However, we do not want to show the warning
--   unless the programmer asks for it (#261).
data ShowDiagnostic
    = ShowDiag  -- ^ Report back to the user
    | HideDiag  -- ^ Hide from user
    deriving (Eq, Ord, Show)

instance NFData ShowDiagnostic where
    rnf = rwhnf

-- | Human readable diagnostics for a specific file.
--
--   This type packages a pretty printed, human readable error message
--   along with the related source location so that we can display the error
--   on either the console or in the IDE at the right source location.
--
type FileDiagnostic = (NormalizedFilePath, ShowDiagnostic, Diagnostic)

prettyRange :: Range -> Doc Terminal.AnsiStyle
prettyRange Range{..} = f _start <> "-" <> f _end
    where f Position{..} = pretty (_line+1) <> colon <> pretty _character

stringParagraphs :: T.Text -> Doc a
stringParagraphs = vcat . map (fillSep . map pretty . T.words) . T.lines

showDiagnostics :: [FileDiagnostic] -> T.Text
showDiagnostics = srenderPlain . prettyDiagnostics

showDiagnosticsColored :: [FileDiagnostic] -> T.Text
showDiagnosticsColored = srenderColored . prettyDiagnostics


prettyDiagnostics :: [FileDiagnostic] -> Doc Terminal.AnsiStyle
prettyDiagnostics = vcat . map prettyDiagnostic

prettyDiagnostic :: FileDiagnostic -> Doc Terminal.AnsiStyle
prettyDiagnostic (fp, sh, LSP.Diagnostic{..}) =
    vcat
        [ slabel_ "File:    " $ pretty (fromNormalizedFilePath fp)
        , slabel_ "Hidden:  " $ if sh == ShowDiag then "no" else "yes"
        , slabel_ "Range:   " $ prettyRange _range
        , slabel_ "Source:  " $ pretty _source
        , slabel_ "Severity:" $ pretty $ show sev
        , slabel_ "Message: "
            $ case sev of
              LSP.DsError -> annotate $ color Red
              LSP.DsWarning -> annotate $ color Yellow
              LSP.DsInfo -> annotate $ color Blue
              LSP.DsHint -> annotate $ color Magenta
            $ stringParagraphs _message
        ]
    where
        sev = fromMaybe LSP.DsError _severity


-- | Label a document.
slabel_ :: String -> Doc a -> Doc a
slabel_ t d = nest 2 $ sep [pretty t, d]

-- | The layout options used for the SDK assistant.
cliLayout ::
       Int
    -- ^ Rendering width of the pretty printer.
    -> LayoutOptions
cliLayout renderWidth = LayoutOptions
    { layoutPageWidth = AvailablePerLine renderWidth 0.9
    }

-- | Render without any syntax annotations
srenderPlain :: Doc ann -> T.Text
srenderPlain = renderStrict . layoutSmart (cliLayout defaultTermWidth)

-- | Render a 'Document' as an ANSII colored string.
srenderColored :: Doc Terminal.AnsiStyle -> T.Text
srenderColored =
    Terminal.renderStrict .
    layoutSmart defaultLayoutOptions { layoutPageWidth = AvailablePerLine 100 1.0 }

defaultTermWidth :: Int
defaultTermWidth = 80
