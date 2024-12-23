{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DisambiguateRecordFields #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
module Development.IDE.GHC.Error
  (
    -- * Producing Diagnostic values
    diagFromGhcErrorMessages
  , diagFromErrMsgs
  , diagFromErrMsg
  , diagFromSDocErrMsgs
  , diagFromSDocErrMsg
  , diagFromString
  , diagFromStrings
  , diagFromGhcException
  , catchSrcErrors

  -- * utilities working with spans
  , srcSpanToLocation
  , srcSpanToRange
  , realSrcSpanToRange
  , realSrcLocToPosition
  , realSrcSpanToLocation
  , realSrcSpanToCodePointRange
  , realSrcLocToCodePointPosition
  , srcSpanToFilename
  , rangeToSrcSpan
  , rangeToRealSrcSpan
  , positionToRealSrcLoc
  , zeroSpan
  , realSpan
  , isInsideSrcSpan
  , spanContainsRange
  , noSpan

  -- * utilities working with severities
  , toDSeverity
  ) where

import           Control.Lens
import           Data.Maybe
import           Data.String                       (fromString)
import qualified Data.Text                         as T
import           Data.Tuple.Extra                  (uncurry3)
import           Development.IDE.GHC.Compat        (GhcMessage, MsgEnvelope,
                                                    errMsgDiagnostic,
                                                    errMsgSeverity, errMsgSpan,
                                                    formatErrorWithQual,
                                                    srcErrorMessages)
import qualified Development.IDE.GHC.Compat        as Compat
import qualified Development.IDE.GHC.Compat.Util   as Compat
import           Development.IDE.GHC.Orphans       ()
import           Development.IDE.Types.Diagnostics as D
import           Development.IDE.Types.Location
import           GHC
import           Language.LSP.Protocol.Types       (isSubrangeOf)
import           Language.LSP.VFS                  (CodePointPosition (CodePointPosition),
                                                    CodePointRange (CodePointRange))


diagFromText :: T.Text -> D.DiagnosticSeverity -> SrcSpan -> T.Text -> Maybe (MsgEnvelope GhcMessage) -> FileDiagnostic
diagFromText diagSource sev loc msg origMsg =
  D.ideErrorWithSource
    (Just diagSource) (Just sev)
    (toNormalizedFilePath' $ fromMaybe noFilePath $ srcSpanToFilename loc)
    msg origMsg
    & fdLspDiagnosticL %~ \diag -> diag { D._range = fromMaybe noRange $ srcSpanToRange loc }

-- | Produce a GHC-style error from a source span and a message.
diagFromErrMsg :: T.Text -> DynFlags -> MsgEnvelope GhcMessage -> [FileDiagnostic]
diagFromErrMsg diagSource dflags origErr =
    let err = fmap (\e -> (Compat.renderDiagnosticMessageWithHints e, Just origErr)) origErr
    in
    diagFromSDocWithOptionalOrigMsg diagSource dflags err

-- | Compatibility function for creating '[FileDiagnostic]' from
-- a 'Compat.Bag' of GHC error messages.
-- The function signature changes based on the GHC version.
-- While this is not desirable, it avoids more CPP statements in code
-- that implements actual logic.
#if MIN_VERSION_ghc(9,5,0)
diagFromGhcErrorMessages :: T.Text -> DynFlags -> Compat.Bag (MsgEnvelope GhcMessage) -> [FileDiagnostic]
diagFromGhcErrorMessages sourceParser dflags errs =
    diagFromErrMsgs sourceParser dflags errs
#else
diagFromGhcErrorMessages :: T.Text -> DynFlags -> Compat.Bag (MsgEnvelope Compat.DecoratedSDoc) -> [FileDiagnostic]
diagFromGhcErrorMessages sourceParser dflags errs =
    diagFromSDocErrMsgs sourceParser dflags errs
#endif

diagFromErrMsgs :: T.Text -> DynFlags -> Compat.Bag (MsgEnvelope GhcMessage) -> [FileDiagnostic]
diagFromErrMsgs diagSource dflags = concatMap (diagFromErrMsg diagSource dflags) . Compat.bagToList

diagFromSDocErrMsg :: T.Text -> DynFlags -> MsgEnvelope Compat.DecoratedSDoc -> [FileDiagnostic]
diagFromSDocErrMsg diagSource dflags err =
    diagFromSDocWithOptionalOrigMsg diagSource dflags (fmap (,Nothing) err)

diagFromSDocErrMsgs :: T.Text -> DynFlags -> Compat.Bag (MsgEnvelope Compat.DecoratedSDoc) -> [FileDiagnostic]
diagFromSDocErrMsgs diagSource dflags = concatMap (diagFromSDocErrMsg diagSource dflags) . Compat.bagToList

diagFromSDocWithOptionalOrigMsg :: T.Text -> DynFlags -> MsgEnvelope (Compat.DecoratedSDoc, Maybe (MsgEnvelope GhcMessage)) -> [FileDiagnostic]
diagFromSDocWithOptionalOrigMsg diagSource dflags err =
    [ diagFromText diagSource sev (errMsgSpan err) (T.pack (formatErrorWithQual dflags (fmap fst err))) (snd (errMsgDiagnostic err))
    | Just sev <- [toDSeverity $ errMsgSeverity err]]

-- | Convert a GHC SrcSpan to a DAML compiler Range
srcSpanToRange :: SrcSpan -> Maybe Range
srcSpanToRange (UnhelpfulSpan _)           = Nothing
srcSpanToRange (Compat.RealSrcSpan real _) = Just $ realSrcSpanToRange real
-- srcSpanToRange = fmap realSrcSpanToRange . realSpan

realSrcSpanToRange :: RealSrcSpan -> Range
realSrcSpanToRange real =
  Range (realSrcLocToPosition $ Compat.realSrcSpanStart real)
        (realSrcLocToPosition $ Compat.realSrcSpanEnd   real)

realSrcLocToPosition :: RealSrcLoc -> Position
realSrcLocToPosition real =
  Position (fromIntegral $ srcLocLine real - 1) (fromIntegral $ srcLocCol real - 1)

-- Note [Unicode support]
-- ~~~~~~~~~~~~~~~~~~~~~~
-- the current situation is:
-- LSP Positions use UTF-16 code units(Unicode may count as variable columns);
-- GHC use Unicode code points(Unicode count as one column).
-- To support unicode, ideally range should be in lsp standard,
-- and codePoint should be in ghc standard.
-- see https://github.com/haskell/lsp/pull/407

-- | Convert a GHC SrcSpan to CodePointRange
-- see Note [Unicode support]
realSrcSpanToCodePointRange :: RealSrcSpan -> CodePointRange
realSrcSpanToCodePointRange real =
  CodePointRange
    (realSrcLocToCodePointPosition $ Compat.realSrcSpanStart real)
    (realSrcLocToCodePointPosition $ Compat.realSrcSpanEnd real)

-- | Convert a GHC RealSrcLoc to CodePointPosition
-- see Note [Unicode support]
realSrcLocToCodePointPosition :: RealSrcLoc -> CodePointPosition
realSrcLocToCodePointPosition real =
  CodePointPosition (fromIntegral $ srcLocLine real - 1) (fromIntegral $ srcLocCol real - 1)

-- | Extract a file name from a GHC SrcSpan (use message for unhelpful ones)
-- FIXME This may not be an _absolute_ file name, needs fixing.
srcSpanToFilename :: SrcSpan -> Maybe FilePath
srcSpanToFilename (UnhelpfulSpan _)  = Nothing
srcSpanToFilename (Compat.RealSrcSpan real _) = Just $ Compat.unpackFS $ srcSpanFile real
-- srcSpanToFilename = fmap (FS.unpackFS . srcSpanFile) . realSpan

realSrcSpanToLocation :: RealSrcSpan -> Location
realSrcSpanToLocation real = Location file (realSrcSpanToRange real)
  where file = fromNormalizedUri $ filePathToUri' $ toNormalizedFilePath' $ Compat.unpackFS $ srcSpanFile real

srcSpanToLocation :: SrcSpan -> Maybe Location
srcSpanToLocation src = do
  fs <- srcSpanToFilename src
  rng <- srcSpanToRange src
  -- important that the URI's we produce have been properly normalized, otherwise they point at weird places in VS Code
  pure $ Location (fromNormalizedUri $ filePathToUri' $ toNormalizedFilePath' fs) rng

rangeToSrcSpan :: NormalizedFilePath -> Range -> SrcSpan
rangeToSrcSpan = fmap (\x -> Compat.RealSrcSpan x Nothing) . rangeToRealSrcSpan

rangeToRealSrcSpan
    :: NormalizedFilePath -> Range -> RealSrcSpan
rangeToRealSrcSpan nfp =
    Compat.mkRealSrcSpan
        <$> positionToRealSrcLoc nfp . _start
        <*> positionToRealSrcLoc nfp . _end

positionToRealSrcLoc :: NormalizedFilePath -> Position -> RealSrcLoc
positionToRealSrcLoc nfp (Position l c)=
    Compat.mkRealSrcLoc (fromString $ fromNormalizedFilePath nfp) (fromIntegral $ l + 1) (fromIntegral $ c + 1)

isInsideSrcSpan :: Position -> SrcSpan -> Bool
p `isInsideSrcSpan` r = case srcSpanToRange r of
  Just (Range sp ep) -> sp <= p && p <= ep
  _                  -> False

-- Returns Nothing if the SrcSpan does not represent a valid range
spanContainsRange :: SrcSpan -> Range -> Maybe Bool
spanContainsRange srcSpan range = (range `isSubrangeOf`) <$> srcSpanToRange srcSpan

-- | Convert a GHC severity to a DAML compiler Severity. Severities below
-- "Warning" level are dropped (returning Nothing).
toDSeverity :: GHC.Severity -> Maybe D.DiagnosticSeverity
toDSeverity SevIgnore  = Nothing
toDSeverity SevWarning = Just DiagnosticSeverity_Warning
toDSeverity SevError   = Just DiagnosticSeverity_Error


-- | Produce a bag of GHC-style errors (@ErrorMessages@) from the given
--   (optional) locations and message strings.
diagFromStrings :: T.Text -> D.DiagnosticSeverity -> [(SrcSpan, String, Maybe (MsgEnvelope GhcMessage))] -> [FileDiagnostic]
diagFromStrings diagSource sev = concatMap (uncurry3 (diagFromString diagSource sev))

-- | Produce a GHC-style error from a source span and a message.
diagFromString :: T.Text -> D.DiagnosticSeverity -> SrcSpan -> String -> Maybe (MsgEnvelope GhcMessage) -> [FileDiagnostic]
diagFromString diagSource sev sp x origMsg = [diagFromText diagSource sev sp (T.pack x) origMsg]


-- | Produces an "unhelpful" source span with the given string.
noSpan :: String -> SrcSpan
noSpan = Compat.mkGeneralSrcSpan . Compat.fsLit


-- | creates a span with zero length in the filename of the argument passed
zeroSpan :: Compat.FastString -- ^ file path of span
         -> RealSrcSpan
zeroSpan file = Compat.realSrcLocSpan (Compat.mkRealSrcLoc file 1 1)

realSpan :: SrcSpan
         -> Maybe RealSrcSpan
realSpan = \case
  Compat.RealSrcSpan r _ -> Just r
  UnhelpfulSpan _        -> Nothing


-- | Catch the errors thrown by GHC (SourceErrors and
-- compiler-internal exceptions like Panic or InstallationError), and turn them into
-- diagnostics
catchSrcErrors :: DynFlags -> T.Text -> IO a -> IO (Either [FileDiagnostic] a)
catchSrcErrors dflags fromWhere ghcM = do
    Compat.handleGhcException ghcExceptionToDiagnostics $
      handleSourceError sourceErrorToDiagnostics $
      Right <$> ghcM
    where
        ghcExceptionToDiagnostics = return . Left . diagFromGhcException fromWhere dflags
        sourceErrorToDiagnostics diag = pure $ Left $
          diagFromErrMsgs fromWhere dflags (Compat.getMessages (srcErrorMessages diag))

diagFromGhcException :: T.Text -> DynFlags -> GhcException -> [FileDiagnostic]
diagFromGhcException diagSource dflags exc = diagFromString diagSource DiagnosticSeverity_Error (noSpan "<Internal>") (showGHCE dflags exc) Nothing

showGHCE :: DynFlags -> GhcException -> String
showGHCE dflags exc = case exc of
        Signal n
          -> "Signal: " <> show n

        Panic s
          -> unwords ["Compilation Issue:", s, "\n", requestReport]
        PprPanic  s sdoc
          -> unlines ["Compilation Issue", s,""
                     , Compat.showSDoc dflags sdoc
                     , requestReport ]

        Sorry s
          -> "Unsupported feature: " <> s
        PprSorry s sdoc
          -> unlines ["Unsupported feature: ", s,""
                     , Compat.showSDoc dflags sdoc]


        ---------- errors below should not happen at all --------
        InstallationError str
          -> "Installation error: " <> str

        UsageError str -- should never happen
          -> unlines ["Unexpected usage error", str]

        CmdLineError str
          -> unlines ["Unexpected usage error", str]

        ProgramError str
            -> "Program error: " <> str
        PprProgramError str  sdoc  ->
            unlines ["Program error:", str,""
                    , Compat.showSDoc dflags sdoc]
  where
    requestReport = "Please report this bug to the compiler authors."
