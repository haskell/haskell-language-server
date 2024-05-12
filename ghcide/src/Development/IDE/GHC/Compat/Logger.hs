{-# LANGUAGE CPP #-}
-- | Compat module for GHC 9.2 Logger infrastructure.
module Development.IDE.GHC.Compat.Logger (
    putLogHook,
    Logger.pushLogHook,
    -- * Logging stuff
    LogActionCompat,
    logActionCompat,
    defaultLogActionHPutStrDoc,
    ) where

import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Env        as Env
import           Development.IDE.GHC.Compat.Outputable


import           GHC.Utils.Logger                      as Logger
import           GHC.Utils.Outputable

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

#if MIN_VERSION_ghc(9,3,0)
import           GHC.Types.Error
#endif

putLogHook :: Logger -> HscEnv -> HscEnv
putLogHook logger env =
  env { hsc_logger = logger }

#if MIN_VERSION_ghc(9,3,0)
type LogActionCompat = LogFlags -> Maybe DiagnosticReason -> Maybe Severity -> SrcSpan -> PrintUnqualified -> SDoc -> IO ()

-- alwaysQualify seems to still do the right thing here, according to the "unqualified warnings" test.
logActionCompat :: LogActionCompat -> LogAction
#if MIN_VERSION_ghc(9,7,0)
logActionCompat logAction logFlags (MCDiagnostic severity (ResolvedDiagnosticReason wr) _) loc = logAction logFlags (Just wr) (Just severity) loc alwaysQualify
#elif MIN_VERSION_ghc(9,5,0)
logActionCompat logAction logFlags (MCDiagnostic severity wr _) loc = logAction logFlags (Just wr) (Just severity) loc alwaysQualify
#else
logActionCompat logAction logFlags (MCDiagnostic severity wr) loc = logAction logFlags (Just wr) (Just severity) loc alwaysQualify
#endif
logActionCompat logAction logFlags _cls loc = logAction logFlags Nothing Nothing loc alwaysQualify

#else
type LogActionCompat = DynFlags -> WarnReason -> Severity -> SrcSpan -> PrintUnqualified -> SDoc -> IO ()

-- alwaysQualify seems to still do the right thing here, according to the "unqualified warnings" test.
logActionCompat :: LogActionCompat -> LogAction
logActionCompat logAction dynFlags wr severity loc = logAction dynFlags wr severity loc alwaysQualify

#endif
