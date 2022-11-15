module Development.IDE.Plugin.CodeAction.Util where

import           Data.Data                             (Data)
import           Data.Time.Clock.POSIX                 (POSIXTime,
                                                        getCurrentTime,
                                                        utcTimeToPOSIXSeconds)
import qualified Data.Unique                           as U
import           Debug.Trace
import           Development.IDE.GHC.Compat.ExactPrint as GHC
import           Development.IDE.GHC.Dump              (showAstDataHtml)
import           GHC.Stack
import           System.Environment.Blank              (getEnvDefault)
import           System.IO.Unsafe
import           Text.Printf
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Utils.Outputable
#else
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.Util
#endif
--------------------------------------------------------------------------------
-- Tracing exactprint terms

-- Should in `Development.IDE.GHC.Orphans`,
-- leave it here to prevent cyclic module dependency

{-# NOINLINE timestamp #-}
timestamp :: POSIXTime
timestamp = utcTimeToPOSIXSeconds $ unsafePerformIO getCurrentTime

debugAST :: Bool
debugAST = unsafePerformIO (getEnvDefault "GHCIDE_DEBUG_AST" "0") == "1"

-- | Prints an 'Outputable' value to stderr and to an HTML file for further inspection
traceAst :: (Data a, ExactPrint a, Outputable a, HasCallStack) => String -> a -> a
traceAst lbl x
  | debugAST = trace doTrace x
  | otherwise = x
  where
#if MIN_VERSION_ghc(9,2,0)
    renderDump = renderWithContext defaultSDocContext{sdocStyle = defaultDumpStyle, sdocPprDebug = True}
#else
    renderDump = showSDocUnsafe . ppr
#endif
    htmlDump = showAstDataHtml x
    doTrace = unsafePerformIO $ do
        u <- U.newUnique
        let htmlDumpFileName = printf "/tmp/hls/%s-%s-%d.html" (show timestamp) lbl (U.hashUnique u)
        writeFile htmlDumpFileName $ renderDump htmlDump
        return $ unlines
            [prettyCallStack callStack ++ ":"
#if MIN_VERSION_ghc(9,2,0)
            , exactPrint x
#endif
            , "file://" ++ htmlDumpFileName]

