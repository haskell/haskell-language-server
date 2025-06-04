-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Development.IDE.Types.Diagnostics (
  LSP.Diagnostic(..),
  ShowDiagnostic(..),
  FileDiagnostic(..),
  fdFilePathL,
  fdLspDiagnosticL,
  fdShouldShowDiagnosticL,
  fdStructuredMessageL,
  StructuredMessage(..),
  _NoStructuredMessage,
  _SomeStructuredMessage,
  IdeResult,
  LSP.DiagnosticSeverity(..),
  DiagnosticStore,
  ideErrorText,
  ideErrorWithSource,
  ideErrorFromLspDiag,
  showDiagnostics,
  showDiagnosticsColored,
  showGhcCode,
  IdeResultNoDiagnosticsEarlyCutoff,
  attachReason,
  attachedReason) where

import           Control.Applicative            ((<|>))
import           Control.DeepSeq
import           Control.Lens
import qualified Data.Aeson                     as JSON
import qualified Data.Aeson.Lens                as JSON
import           Data.ByteString                (ByteString)
import           Data.Foldable
import           Data.Maybe                     as Maybe
import qualified Data.Text                      as T
import           Development.IDE.GHC.Compat     (GhcMessage, MsgEnvelope,
                                                 WarningFlag, flagSpecFlag,
                                                 flagSpecName, wWarningFlags)
import           Development.IDE.Types.Location
import           GHC.Generics
import           GHC.Types.Error                (DiagnosticCode (..),
                                                 DiagnosticReason (..),
                                                 diagnosticCode,
                                                 diagnosticReason,
                                                 errMsgDiagnostic)
import           Language.LSP.Diagnostics
import           Language.LSP.Protocol.Lens     (data_)
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

-- | Produce a 'FileDiagnostic' for the given 'NormalizedFilePath'
-- with an error message.
ideErrorText :: NormalizedFilePath -> T.Text -> FileDiagnostic
ideErrorText nfp msg =
  ideErrorWithSource (Just "compiler") (Just DiagnosticSeverity_Error) nfp msg Nothing

-- | Create a 'FileDiagnostic' from an existing 'LSP.Diagnostic' for a
-- specific 'NormalizedFilePath'.
-- The optional 'MsgEnvelope GhcMessage' is the original error message
-- that was used for creating the 'LSP.Diagnostic'.
-- It is included here, to allow downstream consumers, such as HLS plugins,
-- to provide LSP features based on the structured error messages.
-- Additionally, if available, we insert the ghc error code into the
-- 'LSP.Diagnostic'. These error codes are used in https://errors.haskell.org/
-- to provide documentation and explanations for error messages.
ideErrorFromLspDiag
  :: LSP.Diagnostic
  -> NormalizedFilePath
  -> Maybe (MsgEnvelope GhcMessage)
  -> FileDiagnostic
ideErrorFromLspDiag lspDiag fdFilePath mbOrigMsg =
  let fdShouldShowDiagnostic = ShowDiag
      fdStructuredMessage =
        case mbOrigMsg of
          Nothing  -> NoStructuredMessage
          Just msg -> SomeStructuredMessage msg
      fdLspDiagnostic =
        lspDiag
          & attachReason (fmap (diagnosticReason . errMsgDiagnostic) mbOrigMsg)
          & setGhcCode mbOrigMsg
  in
  FileDiagnostic {..}

-- | Set the code of the 'LSP.Diagnostic' to the GHC diagnostic code which is linked
-- to https://errors.haskell.org/.
setGhcCode :: Maybe (MsgEnvelope GhcMessage) -> LSP.Diagnostic -> LSP.Diagnostic
setGhcCode mbOrigMsg diag =
  let mbGhcCode = do
          origMsg <- mbOrigMsg
          code <- diagnosticCode (errMsgDiagnostic origMsg)
          pure (InR (showGhcCode code))
  in
  diag { _code = mbGhcCode <|> _code diag }

#if MIN_VERSION_ghc(9,9,0)
-- DiagnosticCode only got a show instance in 9.10.1
showGhcCode :: DiagnosticCode -> T.Text
showGhcCode = T.pack . show
#else
showGhcCode :: DiagnosticCode -> T.Text
showGhcCode (DiagnosticCode prefix c) = T.pack $ prefix ++ "-" ++ printf "%05d" c
#endif

attachedReason :: Traversal' Diagnostic (Maybe JSON.Value)
attachedReason = data_ . non (JSON.object []) . JSON.atKey "attachedReason"

attachReason :: Maybe DiagnosticReason -> Diagnostic -> Diagnostic
attachReason Nothing = id
attachReason (Just wr) = attachedReason .~ fmap JSON.toJSON (showReason wr)
 where
  showReason = \case
    WarningWithFlag flag -> Just $ catMaybes [showFlag flag]
#if MIN_VERSION_ghc(9,7,0)
    WarningWithFlags flags -> Just $ catMaybes (fmap showFlag $ toList flags)
#endif
    _                    -> Nothing

showFlag :: WarningFlag -> Maybe T.Text
showFlag flag = ("-W" <>) . T.pack . flagSpecName <$> find ((== flag) . flagSpecFlag) wWarningFlags

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

-- | A Maybe-like wrapper for a GhcMessage that doesn't try to compare, show, or
-- force the GhcMessage inside, so that we can derive Show, Eq, Ord, NFData on
-- FileDiagnostic. FileDiagnostic only uses this as metadata so we can safely
-- ignore it in fields.
--
-- Instead of pattern matching on these constructors directly, consider 'Prism' from
-- the 'lens' package. This allows to conveniently pattern match deeply into the 'MsgEnvelope GhcMessage'
-- constructor.
-- The module 'Development.IDE.GHC.Compat.Error' implements additional 'Lens's and 'Prism's,
-- allowing you to avoid importing GHC modules directly.
--
-- For example, to pattern match on a 'TcRnMessage' you can use the lens:
--
-- @
--   message ^? _SomeStructuredMessage . msgEnvelopeErrorL . _TcRnMessage
-- @
--
-- This produces a value of type `Maybe TcRnMessage`.
--
-- Further, consider utility functions such as 'stripTcRnMessageContext', which strip
-- context from error messages which may be more convenient in certain situations.
data StructuredMessage
  = NoStructuredMessage
  | SomeStructuredMessage (MsgEnvelope GhcMessage)
  deriving (Generic)

instance Show StructuredMessage where
  show NoStructuredMessage      = "NoStructuredMessage"
  show SomeStructuredMessage {} = "SomeStructuredMessage"

instance Eq StructuredMessage where
  (==) NoStructuredMessage NoStructuredMessage           = True
  (==) SomeStructuredMessage {} SomeStructuredMessage {} = True
  (==) _ _                                               = False

instance Ord StructuredMessage where
  compare NoStructuredMessage NoStructuredMessage           = EQ
  compare SomeStructuredMessage {} SomeStructuredMessage {} = EQ
  compare NoStructuredMessage SomeStructuredMessage {}      = GT
  compare SomeStructuredMessage {} NoStructuredMessage      = LT

instance NFData StructuredMessage where
  rnf NoStructuredMessage      = ()
  rnf SomeStructuredMessage {} = ()

-- | Human readable diagnostics for a specific file.
--
--   This type packages a pretty printed, human readable error message
--   along with the related source location so that we can display the error
--   on either the console or in the IDE at the right source location.
--
--   It also optionally keeps a structured diagnostic message GhcMessage in
--   StructuredMessage.
--
data FileDiagnostic = FileDiagnostic
  { fdFilePath             :: NormalizedFilePath
  , fdShouldShowDiagnostic :: ShowDiagnostic
  , fdLspDiagnostic        :: Diagnostic
    -- | The original diagnostic that was used to produce 'fdLspDiagnostic'.
    -- We keep it here, so downstream consumers, e.g. HLS plugins, can use the
    -- the structured error messages and don't have to resort to parsing
    -- error messages via regexes or similar.
    --
    -- The optional GhcMessage inside of this StructuredMessage is ignored for
    -- Eq, Ord, Show, and NFData instances. This is fine because this field
    -- should only ever be metadata and should never be used to distinguish
    -- between FileDiagnostics.
  , fdStructuredMessage    :: StructuredMessage
  }
  deriving (Eq, Ord, Show, Generic)

instance NFData FileDiagnostic

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
                                  Just (InL i)    -> pretty i
                                  Nothing         -> "<none>"
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

makePrisms ''StructuredMessage

makeLensesWith
    (lensRules & lensField .~ mappingNamer (pure . (++ "L")))
    ''FileDiagnostic
