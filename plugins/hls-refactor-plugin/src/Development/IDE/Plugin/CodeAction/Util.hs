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
import           GHC.Utils.Outputable
import           System.Directory.Extra                (createDirectoryIfMissing)
import           System.Environment.Blank              (getEnvDefault)
import           System.IO.Unsafe
import           Text.Printf
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
traceAst :: (Data a, ExactPrint a, HasCallStack) => String -> a -> a
traceAst lbl x
  | debugAST = trace doTrace x
  | otherwise = x
  where
    renderDump = renderWithContext defaultSDocContext{sdocStyle = defaultDumpStyle, sdocPprDebug = True}
    htmlDump = showAstDataHtml x
    doTrace = unsafePerformIO $ do
        u <- U.newUnique
        let htmlDumpFileName = printf "/tmp/hls/%s-%s-%d.html" (show timestamp) lbl (U.hashUnique u)
        createDirectoryIfMissing True "/tmp/hls"
        writeFile htmlDumpFileName $ renderDump htmlDump
        return $ unlines
            [prettyCallStack callStack ++ ":"
            , exactPrint x
            , "file://" ++ htmlDumpFileName]

