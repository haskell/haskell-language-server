-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE CPP #-}

module Development.IDE.Types.Diagnostics (
  LSP.Diagnostic(..),
  ShowDiagnostic(..),
  FileDiagnostic(..),
  fdFilePath,
  fdShouldShowDiagnostic,
  fdLspDiagnostic,
  fdStructuredMessage,
  modifyFdLspDiagnostic,
  StructuredMessage(..),
  IdeResult,
  LSP.DiagnosticSeverity(..),
  DiagnosticStore,
  ideErrorText,
  ideErrorWithSource,
  ideErrorFromLspDiag,
  showDiagnostics,
  showDiagnosticsColored,
  IdeResultNoDiagnosticsEarlyCutoff) where

import           Control.DeepSeq
import           Data.ByteString                (ByteString)
import           Data.Maybe                     as Maybe
import qualified Data.Text                      as T
import           Development.IDE.GHC.Compat     (GhcMessage, MsgEnvelope)
import           Development.IDE.Types.Location
import           GHC.Generics
import           GHC.Types.Error                (diagnosticCode, DiagnosticCode (..), errMsgDiagnostic)
import           Language.LSP.Diagnostics
import           Language.LSP.Protocol.Types    as LSP
import           Prettyprinter
import           Prettyprinter.Render.Terminal  (Color (..), color)
import qualified Prettyprinter.Render.Terminal  as Terminal
import           Prettyprinter.Render.Text
import           Text.Printf                    (printf)


-- | The result of an IDE operation. Warnings and errors are in the Diagnostic,
--   and a value is in the Maybe. For operations that throw an error you
--   expect a non-empty list of diagnostics, at least one of which is an error,
--   and a Nothing. For operations that succeed you expect perhaps some warnings
--   and a Just. For operations that depend on other failing operations you may
--   get empty diagnostics and a Nothing, to indicate this phase throws no fresh
--   errors but still failed.
--
--   A rule on a file should only return diagnostics for that given file. It should
--   not propagate diagnostic errors through multiple phases.
type IdeResult v = ([FileDiagnostic], Maybe v)

-- | an IdeResult with a fingerprint
type IdeResultNoDiagnosticsEarlyCutoff  v = (Maybe ByteString, Maybe v)

ideErrorText :: Maybe (MsgEnvelope GhcMessage) -> NormalizedFilePath -> T.Text -> FileDiagnostic
ideErrorText origMsg fdFilePath msg =
  ideErrorWithSource (Just "compiler") (Just DiagnosticSeverity_Error) fdFilePath msg origMsg

ideErrorFromLspDiag
  :: LSP.Diagnostic
  -> NormalizedFilePath
  -> Maybe (MsgEnvelope GhcMessage)
  -> FileDiagnostic
ideErrorFromLspDiag lspDiag fdFilePath origMsg =
  let fdShouldShowDiagnostic = ShowDiag
      fdStructuredMessage =
        case origMsg of
          Nothing -> NoStructuredMessage
          Just msg -> SomeStructuredMessage msg
      fdLspDiagnostic = lspDiag
        { _code = fmap ghcCodeToLspCode . diagnosticCode . errMsgDiagnostic =<< origMsg
        }
      ghcCodeToLspCode :: DiagnosticCode -> Int32 LSP.|? T.Text
#if MIN_VERSION_ghc(9,10,1)
      ghcCodeToLspCode = InR . T.pack . show
#else
      ghcCodeToLspCode (DiagnosticCode prefix c) = InR $ T.pack $ prefix ++ "-" ++ printf "%05d" c
#endif
  in
  FileDiagnostic {..}

ideErrorWithSource
  :: Maybe T.Text
  -> Maybe DiagnosticSeverity
  -> NormalizedFilePath
  -> T.Text
  -> Maybe (MsgEnvelope GhcMessage)
  -> FileDiagnostic
ideErrorWithSource source sev fdFilePath msg origMsg =
  let lspDiagnostic =
        LSP.Diagnostic {
          _range = noRange,
          _severity = sev,
          _code = Nothing,
          _source = source,
          _message = msg,
          _relatedInformation = Nothing,
          _tags = Nothing,
          _codeDescription = Nothing,
          _data_ = Nothing
        }
  in
  ideErrorFromLspDiag lspDiagnostic fdFilePath origMsg

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
data FileDiagnostic = FileDiagnostic
  { fdFilePath :: NormalizedFilePath
  , fdShouldShowDiagnostic :: ShowDiagnostic
  , fdLspDiagnostic :: Diagnostic
  , fdStructuredMessage :: StructuredMessage
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData FileDiagnostic

data StructuredMessage
  = NoStructuredMessage
  | SomeStructuredMessage (MsgEnvelope GhcMessage)
  deriving (Generic)

instance Show StructuredMessage where
  show NoStructuredMessage = "NoStructuredMessage"
  show SomeStructuredMessage {} = "SomeStructuredMessage"

instance Eq StructuredMessage where
  (==) NoStructuredMessage NoStructuredMessage = True
  (==) SomeStructuredMessage {} SomeStructuredMessage {} = True
  (==) _ _ = False

instance Ord StructuredMessage where
  compare NoStructuredMessage NoStructuredMessage = EQ
  compare SomeStructuredMessage {} SomeStructuredMessage {} = EQ
  compare NoStructuredMessage SomeStructuredMessage {} = GT
  compare SomeStructuredMessage {} NoStructuredMessage = LT

instance NFData StructuredMessage where
  rnf NoStructuredMessage = ()
  rnf SomeStructuredMessage {} = ()

modifyFdLspDiagnostic :: (Diagnostic -> Diagnostic) -> FileDiagnostic -> FileDiagnostic
modifyFdLspDiagnostic f diag =
  diag { fdLspDiagnostic = f (fdLspDiagnostic diag) }

prettyRange :: Range -> Doc Terminal.AnsiStyle
prettyRange Range{..} = f _start <> "-" <> f _end
    where f Position{..} = pretty (show $ _line+1) <> colon <> pretty (show $ _character+1)

stringParagraphs :: T.Text -> Doc a
stringParagraphs = vcat . map (fillSep . map pretty . T.words) . T.lines

showDiagnostics :: [FileDiagnostic] -> T.Text
showDiagnostics = srenderPlain . prettyDiagnostics

showDiagnosticsColored :: [FileDiagnostic] -> T.Text
showDiagnosticsColored = srenderColored . prettyDiagnostics


prettyDiagnostics :: [FileDiagnostic] -> Doc Terminal.AnsiStyle
prettyDiagnostics = vcat . map prettyDiagnostic

prettyDiagnostic :: FileDiagnostic -> Doc Terminal.AnsiStyle
prettyDiagnostic FileDiagnostic { fdFilePath, fdShouldShowDiagnostic, fdLspDiagnostic = LSP.Diagnostic{..} } =
    vcat
        [ slabel_ "File:    " $ pretty (fromNormalizedFilePath fdFilePath)
        , slabel_ "Hidden:  " $ if fdShouldShowDiagnostic == ShowDiag then "no" else "yes"
        , slabel_ "Range:   " $ prettyRange _range
        , slabel_ "Source:  " $ pretty _source
        , slabel_ "Severity:" $ pretty $ show sev
        , slabel_ "Code:    " $ case _code of
                                  Just (InR text) -> pretty text
                                  Just (InL i) -> pretty i
                                  Nothing -> "<none>"
        , slabel_ "Message: "
            $ case sev of
              LSP.DiagnosticSeverity_Error       -> annotate $ color Red
              LSP.DiagnosticSeverity_Warning     -> annotate $ color Yellow
              LSP.DiagnosticSeverity_Information -> annotate $ color Blue
              LSP.DiagnosticSeverity_Hint        -> annotate $ color Magenta
            $ stringParagraphs _message
        ]
    where
        sev = fromMaybe LSP.DiagnosticSeverity_Error _severity


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
