{-# LANGUAGE CPP #-}

module Development.IDE.GHC.Compat.Outputable (
    SDoc,
    Outputable,
    showSDoc,
    showSDocUnsafe,
    showSDocForUser,
    ppr, pprPanic, text, vcat, (<+>), ($$), empty, hang, nest, punctuate,
    printSDocQualifiedUnsafe,
    printWithoutUniques,
    mkPrintUnqualifiedDefault,
    PrintUnqualified,
    defaultUserStyle,
    withPprStyle,
    -- * Parser errors
    PsWarning,
    PsError,
#if MIN_VERSION_ghc(9,5,0)
    defaultDiagnosticOpts,
    GhcMessage,
    DriverMessage,
    Messages,
    initDiagOpts,
    pprMessages,
#endif
#if MIN_VERSION_ghc(9,3,0)
    DiagnosticReason(..),
    renderDiagnosticMessageWithHints,
    pprMsgEnvelopeBagWithLoc,
    Error.getMessages,
    renderWithContext,
    defaultSDocContext,
    errMsgDiagnostic,
    unDecorated,
    diagnosticMessage,
#else
    pprWarning,
    pprError,
#endif
    -- * Error infrastructure
    DecoratedSDoc,
    MsgEnvelope,
    ErrMsg,
    WarnMsg,
    SourceError(..),
    errMsgSpan,
    errMsgSeverity,
    formatErrorWithQual,
    mkWarnMsg,
    mkSrcErr,
    srcErrorMessages,
    textDoc,
    ) where

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]


import           GHC.Driver.Env
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import qualified GHC.Types.Error              as Error
import           GHC.Types.Name.Ppr
import           GHC.Types.Name.Reader
import           GHC.Types.SourceError
import           GHC.Types.SrcLoc
import           GHC.Unit.State
import           GHC.Utils.Error              hiding (mkWarnMsg)
import           GHC.Utils.Outputable         as Out
import           GHC.Utils.Panic

#if !MIN_VERSION_ghc(9,3,0)
import           GHC.Parser.Errors
import qualified GHC.Parser.Errors.Ppr        as Ppr
#endif

#if MIN_VERSION_ghc(9,3,0)
import           Data.Maybe
import           GHC.Driver.Config.Diagnostic
import           GHC.Parser.Errors.Types
#endif

#if MIN_VERSION_ghc(9,5,0)
import           GHC.Driver.Errors.Types      (DriverMessage, GhcMessage)
#endif

#if MIN_VERSION_ghc(9,7,0)
import           GHC.Types.Error              (defaultDiagnosticOpts)
#endif

#if MIN_VERSION_ghc(9,5,0)
type PrintUnqualified = NamePprCtx
#endif

-- | A compatible function to print `Outputable` instances
-- without unique symbols.
--
-- It print with a user-friendly style like: `a_a4ME` as `a`.
printWithoutUniques :: Outputable a => a -> String
printWithoutUniques =
  renderWithContext (defaultSDocContext
    {
      sdocStyle = defaultUserStyle
    , sdocSuppressUniques = True
    , sdocCanUseUnicode = True
    }) . ppr

printSDocQualifiedUnsafe :: PrintUnqualified -> SDoc -> String
printSDocQualifiedUnsafe unqual doc =
  -- Taken from 'showSDocForUser'
  renderWithContext (defaultSDocContext { sdocStyle = sty }) doc'
  where
    sty  = mkUserStyle unqual AllTheWay
    doc' = pprWithUnitState emptyUnitState doc


#if !MIN_VERSION_ghc(9,3,0)
pprWarning :: PsWarning -> MsgEnvelope DecoratedSDoc
pprWarning =
  Ppr.pprWarning

pprError :: PsError -> MsgEnvelope DecoratedSDoc
pprError =
  Ppr.pprError
#endif

formatErrorWithQual :: DynFlags -> MsgEnvelope DecoratedSDoc -> String
formatErrorWithQual dflags e =
  showSDoc dflags (pprNoLocMsgEnvelope e)

#if MIN_VERSION_ghc(9,3,0)
pprNoLocMsgEnvelope :: MsgEnvelope DecoratedSDoc -> SDoc
#else
pprNoLocMsgEnvelope :: Error.RenderableDiagnostic e => MsgEnvelope e -> SDoc
#endif
pprNoLocMsgEnvelope (MsgEnvelope { errMsgDiagnostic = e
                                 , errMsgContext   = unqual })
  = sdocWithContext $ \_ctx ->
    withErrStyle unqual $
#if MIN_VERSION_ghc(9,7,0)
      formatBulleted e
#elif MIN_VERSION_ghc(9,3,0)
      formatBulleted _ctx $ e
#else
      formatBulleted _ctx $ Error.renderDiagnostic e
#endif



type ErrMsg  = MsgEnvelope DecoratedSDoc
#if MIN_VERSION_ghc(9,3,0)
type WarnMsg  = MsgEnvelope DecoratedSDoc
#endif

mkPrintUnqualifiedDefault :: HscEnv -> GlobalRdrEnv -> PrintUnqualified
#if MIN_VERSION_ghc(9,5,0)
mkPrintUnqualifiedDefault env =
  mkNamePprCtx ptc (hsc_unit_env env)
    where
      ptc = initPromotionTickContext (hsc_dflags env)
#else
mkPrintUnqualifiedDefault env =
  -- GHC 9.2 version
  -- mkPrintUnqualified :: UnitEnv -> GlobalRdrEnv -> PrintUnqualified
  mkPrintUnqualified (hsc_unit_env env)
#endif

#if MIN_VERSION_ghc(9,3,0)
renderDiagnosticMessageWithHints :: forall a. Diagnostic a => a -> DecoratedSDoc
renderDiagnosticMessageWithHints a = Error.unionDecoratedSDoc
  (diagnosticMessage
#if MIN_VERSION_ghc(9,5,0)
    (defaultDiagnosticOpts @a)
#endif
    a) (mkDecorated $ map ppr $ diagnosticHints a)
#endif

#if MIN_VERSION_ghc(9,3,0)
mkWarnMsg :: DynFlags -> Maybe DiagnosticReason -> b -> SrcSpan -> PrintUnqualified -> SDoc -> MsgEnvelope DecoratedSDoc
mkWarnMsg df reason _logFlags l st doc = fmap renderDiagnosticMessageWithHints $ mkMsgEnvelope (initDiagOpts df) l st (mkPlainDiagnostic (fromMaybe WarningWithoutFlag reason) [] doc)
#else
mkWarnMsg :: a -> b -> DynFlags -> SrcSpan -> PrintUnqualified -> SDoc -> MsgEnvelope DecoratedSDoc
mkWarnMsg _ _ =
  const Error.mkWarnMsg
#endif

textDoc :: String -> SDoc
textDoc = text
