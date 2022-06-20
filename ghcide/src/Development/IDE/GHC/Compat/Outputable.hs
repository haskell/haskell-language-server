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
    mkPrintUnqualified,
    mkPrintUnqualifiedDefault,
    PrintUnqualified(..),
    defaultUserStyle,
    withPprStyle,
    -- * Parser errors
    PsWarning,
    PsError,
    pprWarning,
    pprError,
    -- * Error infrastructure
    DecoratedSDoc,
    MsgEnvelope,
    errMsgSpan,
    errMsgSeverity,
    formatErrorWithQual,
    mkWarnMsg,
    mkSrcErr,
    srcErrorMessages,
    ) where


#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Env
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Parser.Errors
import qualified GHC.Parser.Errors.Ppr           as Ppr
import qualified GHC.Types.Error                 as Error
import           GHC.Types.Name.Ppr
import           GHC.Types.Name.Reader
import           GHC.Types.SourceError
import           GHC.Types.SrcLoc
import           GHC.Unit.State
import           GHC.Utils.Error                 hiding (mkWarnMsg)
import           GHC.Utils.Outputable            as Out hiding (defaultUserStyle)
import qualified GHC.Utils.Outputable            as Out
import           GHC.Utils.Panic
#elif MIN_VERSION_ghc(9,0,0)
import           GHC.Driver.Session
import           GHC.Driver.Types                as HscTypes
import           GHC.Types.Name.Reader           (GlobalRdrEnv)
import           GHC.Types.SrcLoc
import           GHC.Utils.Error                 as Err hiding (mkWarnMsg)
import qualified GHC.Utils.Error                 as Err
import           GHC.Utils.Outputable            as Out hiding (defaultUserStyle)
import qualified GHC.Utils.Outputable            as Out
#else
import           Development.IDE.GHC.Compat.Core (GlobalRdrEnv)
import           DynFlags
import           ErrUtils                        hiding (mkWarnMsg)
import qualified ErrUtils                        as Err
import           HscTypes
import           Outputable                      as Out hiding (defaultUserStyle)
import qualified Outputable                      as Out
import           SrcLoc
#endif

-- | A compatible function to print `Outputable` instances
-- without unique symbols.
--
-- It print with a user-friendly style like: `a_a4ME` as `a`.
printWithoutUniques :: Outputable a => a -> String
printWithoutUniques =
#if MIN_VERSION_ghc(9,2,0)
  renderWithContext (defaultSDocContext
    {
      sdocStyle = defaultUserStyle
    , sdocSuppressUniques = True
    , sdocCanUseUnicode = True
    }) . ppr
#else
  go . ppr
    where
      go sdoc = oldRenderWithStyle dflags sdoc (oldMkUserStyle dflags neverQualify AllTheWay)
      dflags = unsafeGlobalDynFlags `gopt_set` Opt_SuppressUniques
#endif

printSDocQualifiedUnsafe :: PrintUnqualified -> SDoc -> String
#if MIN_VERSION_ghc(9,2,0)
printSDocQualifiedUnsafe unqual doc =
  -- Taken from 'showSDocForUser'
  renderWithContext (defaultSDocContext { sdocStyle = sty }) doc'
  where
    sty  = mkUserStyle unqual AllTheWay
    doc' = pprWithUnitState emptyUnitState doc
#else
printSDocQualifiedUnsafe unqual doc =
    showSDocForUser unsafeGlobalDynFlags unqual doc
#endif

#if MIN_VERSION_ghc(9,0,0) && !MIN_VERSION_ghc(9,2,0)
oldRenderWithStyle dflags sdoc sty = Out.renderWithStyle (initSDocContext dflags sty) sdoc
oldMkUserStyle _ = Out.mkUserStyle
oldMkErrStyle _ = Out.mkErrStyle

oldFormatErrDoc :: DynFlags -> Err.ErrDoc -> Out.SDoc
oldFormatErrDoc dflags = Err.formatErrDoc dummySDocContext
  where dummySDocContext = initSDocContext dflags Out.defaultUserStyle
#elif !MIN_VERSION_ghc(9,0,0)
oldRenderWithStyle :: DynFlags -> Out.SDoc -> Out.PprStyle -> String
oldRenderWithStyle = Out.renderWithStyle

oldMkUserStyle :: DynFlags -> Out.PrintUnqualified -> Out.Depth -> Out.PprStyle
oldMkUserStyle = Out.mkUserStyle

oldMkErrStyle :: DynFlags -> Out.PrintUnqualified -> Out.PprStyle
oldMkErrStyle = Out.mkErrStyle

oldFormatErrDoc :: DynFlags -> Err.ErrDoc -> Out.SDoc
oldFormatErrDoc = Err.formatErrDoc
#endif

pprWarning :: PsWarning -> MsgEnvelope DecoratedSDoc
pprWarning =
#if MIN_VERSION_ghc(9,2,0)
  Ppr.pprWarning
#else
  id
#endif

pprError :: PsError -> MsgEnvelope DecoratedSDoc
pprError =
#if MIN_VERSION_ghc(9,2,0)
  Ppr.pprError
#else
  id
#endif

formatErrorWithQual :: DynFlags -> MsgEnvelope DecoratedSDoc -> String
formatErrorWithQual dflags e =
#if MIN_VERSION_ghc(9,2,0)
  showSDoc dflags (pprNoLocMsgEnvelope e)

pprNoLocMsgEnvelope :: Error.RenderableDiagnostic e => MsgEnvelope e -> SDoc
pprNoLocMsgEnvelope (MsgEnvelope { errMsgDiagnostic = e
                                 , errMsgContext   = unqual })
  = sdocWithContext $ \ctx ->
    withErrStyle unqual $
      (formatBulleted ctx $ Error.renderDiagnostic e)

#else
  Out.showSDoc dflags
  $ Out.withPprStyle (oldMkErrStyle dflags $ errMsgContext e)
  $ oldFormatErrDoc dflags
  $ Err.errMsgDoc e
#endif

#if !MIN_VERSION_ghc(9,2,0)
type DecoratedSDoc = ()
type MsgEnvelope e = ErrMsg

type PsWarning = ErrMsg
type PsError = ErrMsg
#endif

mkPrintUnqualifiedDefault :: HscEnv -> GlobalRdrEnv -> PrintUnqualified
mkPrintUnqualifiedDefault env =
#if MIN_VERSION_ghc(9,2,0)
  -- GHC 9.2 version
  -- mkPrintUnqualified :: UnitEnv -> GlobalRdrEnv -> PrintUnqualified
  mkPrintUnqualified (hsc_unit_env env)
#else
  HscTypes.mkPrintUnqualified (hsc_dflags env)
#endif

mkWarnMsg :: DynFlags -> SrcSpan -> PrintUnqualified -> SDoc -> MsgEnvelope DecoratedSDoc
mkWarnMsg =
#if MIN_VERSION_ghc(9,2,0)
  const Error.mkWarnMsg
#else
  Err.mkWarnMsg
#endif

defaultUserStyle :: PprStyle
#if MIN_VERSION_ghc(9,0,0)
defaultUserStyle = Out.defaultUserStyle
#else
defaultUserStyle = Out.defaultUserStyle unsafeGlobalDynFlags
#endif
