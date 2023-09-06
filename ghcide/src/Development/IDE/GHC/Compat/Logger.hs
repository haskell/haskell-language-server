{-# LANGUAGE CPP #-}
-- | Compat module for GHC 9.2 Logger infrastructure.
module Development.IDE.GHC.Compat.Logger (
    putLogHook,
    Development.IDE.GHC.Compat.Logger.pushLogHook,
    -- * Logging stuff
    LogActionCompat,
    logActionCompat,
    defaultLogActionHPutStrDoc,
    ) where

import           Development.IDE.GHC.Compat.Core
import           Development.IDE.GHC.Compat.Env        as Env
import           Development.IDE.GHC.Compat.Outputable

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

import           GHC.Utils.Outputable

#if !MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Session                    as DynFlags
#endif

#if MIN_VERSION_ghc(9,2,0)
import           GHC.Driver.Env                        (hsc_logger)
import           GHC.Utils.Logger                      as Logger
#endif

#if MIN_VERSION_ghc(9,3,0)
import           GHC.Types.Error
#endif

putLogHook :: Logger -> HscEnv -> HscEnv
putLogHook logger env =
#if MIN_VERSION_ghc(9,2,0)
  env { hsc_logger = logger }
#else
  hscSetFlags ((hsc_dflags env) { DynFlags.log_action = Env.log_action logger }) env
#endif

pushLogHook :: (LogAction -> LogAction) -> Logger -> Logger
pushLogHook f logger =
#if MIN_VERSION_ghc(9,2,0)
  Logger.pushLogHook f logger
#else
  logger { Env.log_action = f (Env.log_action logger) }
#endif

#if MIN_VERSION_ghc(9,3,0)
type LogActionCompat = LogFlags -> Maybe DiagnosticReason -> Maybe Severity -> SrcSpan -> PrintUnqualified -> SDoc -> IO ()

-- alwaysQualify seems to still do the right thing here, according to the "unqualified warnings" test.
logActionCompat :: LogActionCompat -> LogAction
#if MIN_VERSION_ghc(9,5,0)
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
