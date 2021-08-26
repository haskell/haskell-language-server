{-# LANGUAGE CPP #-}

module Development.IDE.GHC.Compat.Outputable (
    SDoc,
    Outputable,
    showSDoc,
    showSDocUnsafe,
    showSDocForUser,
    ppr,
    pprPanic,
    printSDocQualifiedUnsafe,
    printNameWithoutUniques,
    printSDocAllTheWay,
    mkPrintUnqualified,
    mkPrintUnqualifiedDefault,
    PrintUnqualified(..),
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
import GHC.Driver.Session
import GHC.Driver.Ppr
import qualified GHC.Types.Error as Error
import GHC.Types.SrcLoc
import GHC.Types.SourceError
import GHC.Unit.State (emptyUnitState)
import GHC.Utils.Error hiding (mkWarnMsg)
import GHC.Utils.Outputable
import GHC.Utils.Logger
import GHC.Utils.Panic
import GHC.Parser.Errors
import qualified GHC.Parser.Errors.Ppr as Ppr
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Types
import GHC.Driver.Session
import GHC.Utils.Outputable as Out
import GHC.Types.SrcLoc
import GHC.Utils.Error as Err hiding (mkWarnMsg)
import qualified GHC.Utils.Error as Err
import GHC.Types.Name.Reader (GlobalRdrEnv)
#else
import DynFlags
import Outputable as Out
import HscTypes
import qualified ErrUtils as Err
import SrcLoc
import ErrUtils hiding (mkWarnMsg)
import Development.IDE.GHC.Compat.Core (GlobalRdrEnv)
#endif


printNameWithoutUniques :: Outputable a => a -> String
printNameWithoutUniques =
#if MIN_VERSION_ghc(9,2,0)
      renderWithContext (defaultSDocContext { sdocSuppressUniques = True }) . ppr
#else
      printSDocAllTheWay dyn . ppr
  where
    dyn = unsafeGlobalDynFlags `gopt_set` Opt_SuppressUniques
#endif

printSDocQualifiedUnsafe :: PrintUnqualified -> SDoc -> String
printSDocQualifiedUnsafe unqual doc =
#if MIN_VERSION_ghc(9,2,0)
  -- Taken from 'showSDocForUser'
  renderWithContext (initSDocContext dflags sty) doc'
  where
    sty  = mkUserStyle unqual AllTheWay
    doc' = pprWithUnitState emptyUnitState doc
#else
  showSDocForUser unsafeGlobalDynFlags unqual doc
#endif

printSDocAllTheWay :: DynFlags -> SDoc -> String
#if MIN_VERSION_ghc(9,2,0)
printSDocAllTheWay dflags sdoc = renderWithContext ctxt sdoc
  where
    ctxt = initSDocContext dflags (mkUserStyle neverQualify AllTheWay)
#else
printSDocAllTheWay dflags sdoc = oldRenderWithStyle dflags sdoc (oldMkUserStyle dflags Out.neverQualify Out.AllTheWay)

#if  MIN_VERSION_ghc(9,0,0)
oldRenderWithStyle dflags sdoc sty = Out.renderWithStyle (initSDocContext dflags sty) sdoc
oldMkUserStyle _ = Out.mkUserStyle
oldMkErrStyle _ = Out.mkErrStyle

oldFormatErrDoc :: DynFlags -> Err.ErrDoc -> Out.SDoc
oldFormatErrDoc dflags = Err.formatErrDoc dummySDocContext
  where dummySDocContext = initSDocContext dflags Out.defaultUserStyle

#else
oldRenderWithStyle :: DynFlags -> Out.SDoc -> Out.PprStyle -> String
oldRenderWithStyle = Out.renderWithStyle

oldMkUserStyle :: DynFlags -> Out.PrintUnqualified -> Out.Depth -> Out.PprStyle
oldMkUserStyle = Out.mkUserStyle

oldMkErrStyle :: DynFlags -> Out.PrintUnqualified -> Out.PprStyle
oldMkErrStyle = Out.mkErrStyle

oldFormatErrDoc :: DynFlags -> Err.ErrDoc -> Out.SDoc
oldFormatErrDoc = Err.formatErrDoc
#endif
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
    showSDoc dflags (pprLocMsgEnvelope e)
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

-- | Like 'mkPrintUnqualified', but requires no additional context,
-- such as DynFlags or, in later GHC versions, UnitState by relying
-- on defaults.
mkPrintUnqualifiedDefault :: GlobalRdrEnv -> PrintUnqualified
mkPrintUnqualifiedDefault =
#if MIN_VERSION_ghc(9,2,0)
  mkPrintUnqualified emptyUnitState
#else
  mkPrintUnqualified unsafeGlobalDynFlags
#endif


mkWarnMsg :: DynFlags -> SrcSpan -> PrintUnqualified -> SDoc -> MsgEnvelope DecoratedSDoc
mkWarnMsg =
#if MIN_VERSION_ghc(9,2,0)
  const Error.mkWarnMsg
#else
  Err.mkWarnMsg
#endif
