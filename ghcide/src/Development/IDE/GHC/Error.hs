{-# LANGUAGE CPP #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
module Development.IDE.GHC.Error
  (
    -- * Producing Diagnostic values
    diagFromErrMsgs
  , diagFromErrMsg
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

import           Data.Maybe
import           Data.String                       (fromString)
import qualified Data.Text                         as T
import           Development.IDE.GHC.Compat        (DecoratedSDoc, MsgEnvelope,
                                                    errMsgSeverity, errMsgSpan,
                                                    formatErrorWithQual,
                                                    srcErrorMessages)
import qualified Development.IDE.GHC.Compat        as Compat
import qualified Development.IDE.GHC.Compat.Util   as Compat
import           Development.IDE.GHC.Orphans       ()
import           Development.IDE.Types.Diagnostics as D
import           Development.IDE.Types.Location
import           GHC
import           Language.LSP.Types                (isSubrangeOf)


diagFromText :: T.Text -> D.DiagnosticSeverity -> SrcSpan -> T.Text -> FileDiagnostic
diagFromText diagSource sev loc msg = (toNormalizedFilePath' $ fromMaybe noFilePath $ srcSpanToFilename loc,ShowDiag,)
    Diagnostic
    { _range    = fromMaybe noRange $ srcSpanToRange loc
    , _severity = Just sev
    , _source   = Just diagSource -- not shown in the IDE, but useful for ghcide developers
    , _message  = msg
    , _code     = Nothing
    , _relatedInformation = Nothing
    , _tags     = Nothing
    }

-- | Produce a GHC-style error from a source span and a message.
diagFromErrMsg :: T.Text -> DynFlags -> MsgEnvelope DecoratedSDoc -> [FileDiagnostic]
diagFromErrMsg diagSource dflags e =
    [ diagFromText diagSource sev (errMsgSpan e)
      $ T.pack $ formatErrorWithQual dflags e
    | Just sev <- [toDSeverity $ errMsgSeverity e]]

diagFromErrMsgs :: T.Text -> DynFlags -> Compat.Bag (MsgEnvelope DecoratedSDoc) -> [FileDiagnostic]
diagFromErrMsgs diagSource dflags = concatMap (diagFromErrMsg diagSource dflags) . Compat.bagToList

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
#if !MIN_VERSION_ghc(9,3,0)
toDSeverity SevOutput      = Nothing
toDSeverity SevInteractive = Nothing
toDSeverity SevDump        = Nothing
toDSeverity SevInfo        = Just DsInfo
toDSeverity SevFatal       = Just DsError
#else
toDSeverity SevIgnore      = Nothing
#endif
toDSeverity SevWarning     = Just DsWarning
toDSeverity SevError       = Just DsError


-- | Produce a bag of GHC-style errors (@ErrorMessages@) from the given
--   (optional) locations and message strings.
diagFromStrings :: T.Text -> D.DiagnosticSeverity -> [(SrcSpan, String)] -> [FileDiagnostic]
diagFromStrings diagSource sev = concatMap (uncurry (diagFromString diagSource sev))

-- | Produce a GHC-style error from a source span and a message.
diagFromString :: T.Text -> D.DiagnosticSeverity -> SrcSpan -> String -> [FileDiagnostic]
diagFromString diagSource sev sp x = [diagFromText diagSource sev sp $ T.pack x]


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
    Compat.handleGhcException (ghcExceptionToDiagnostics dflags) $
      handleSourceError (sourceErrorToDiagnostics dflags) $
      Right <$> ghcM
    where
        ghcExceptionToDiagnostics dflags = return . Left . diagFromGhcException fromWhere dflags
        sourceErrorToDiagnostics dflags = return . Left . diagFromErrMsgs fromWhere dflags
#if MIN_VERSION_ghc(9,3,0)
                                        . fmap (fmap Compat.renderDiagnosticMessageWithHints) . Compat.getMessages
#endif
                                        . srcErrorMessages


diagFromGhcException :: T.Text -> DynFlags -> GhcException -> [FileDiagnostic]
diagFromGhcException diagSource dflags exc = diagFromString diagSource DsError (noSpan "<Internal>") (showGHCE dflags exc)

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
