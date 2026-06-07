{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wwarn #-}

-- | Expression execution
module Ide.Plugin.Eval.Code (
    Statement
  , evalExprRanges
  , resultRange
  , propSetup
  , evalExprCheck
  , asStatements
  , execStmtCaptureResult
  ) where

import           Control.Lens                ((^.))
import           Control.Monad.Catch         (MonadMask, bracket)
import           Control.Monad.IO.Class
import           Data.Algorithm.Diff         (Diff, PolyDiff (..), getDiff)
import qualified Data.List.NonEmpty          as NE
import           Data.Maybe                  (listToMaybe)
import           Data.String                 (IsString)
import qualified Data.Text                   as T
import           Development.IDE.GHC.Compat
import           GHC                         (ExecOptions, ExecResult (..),
                                              execStmt)
import           Ide.Logger                  (Recorder, WithPriority, logWith)
import qualified Ide.Logger                  as Log

import           Ide.Plugin.Eval.Types       (EvalExpr (..), Language (Plain),
                                              Loc, Located (..), Log (..),
                                              Section (sectionLanguage), Txt,
                                              locate, locate0)
import           Ide.Plugin.Eval.Util        (gStrictTry)
import qualified Language.LSP.Protocol.Lens  as L
import           Language.LSP.Protocol.Types (Position (Position),
                                              Range (Range))
import           System.IO.Extra             (newTempFile, readFile')

-- | Return the ranges of the expression and result parts of the given 'EvalExpr'.
evalExprRanges :: EvalExpr -> (Range, Range)
evalExprRanges tst =
    let startLine = evalExprRange tst ^. L.start . L.line
        (fromIntegral -> exprLines, fromIntegral -> resultLines) = evalExprLengths tst
        resLine = startLine + exprLines
     in ( Range
            (Position startLine 0)
            --(Position (startLine + exprLines + resultLines) 0),
            (Position resLine 0)
        , Range (Position resLine 0) (Position (resLine + resultLines) 0)
        )

-- | The document range where the result of the 'EvalExpr' is defined.
resultRange :: EvalExpr -> Range
resultRange = snd . evalExprRanges

-- TODO: handle BLANKLINE
{-
>>> showDiffs $  getDiff ["abc","def","ghi","end"] ["abc","def","Z","ZZ","end"]
["abc","def","WAS ghi","NOW Z","NOW ZZ","end"]
-}
showDiffs :: (Semigroup a, IsString a) => [Diff a] -> [a]
showDiffs = map showDiff

showDiff :: (Semigroup a, IsString a) => Diff a -> a
showDiff (First w)  = "WAS " <> w
showDiff (Second w) = "NOW " <> w
showDiff (Both w _) = w

-- | Compare the expected output recorded in the 'EvalExpr' with the actual
-- output @out@. When diffing is enabled and there is a recorded output, return
-- a line-by-line diff (see 'showDiffs'); otherwise return @out@ unchanged.
evalExprCheck :: Bool -> (Section, EvalExpr) -> [T.Text] -> [T.Text]
evalExprCheck diff (section, evalExpr) out
    | not diff || null (evalExprOutput evalExpr) || sectionLanguage section == Plain = out
    | otherwise = showDiffs $ getDiff (map T.pack $ evalExprOutput evalExpr) out

-- | The number of (expression lines, result lines) an 'EvalExpr' occupies.
evalExprLengths :: EvalExpr -> (Int, Int)
evalExprLengths (Example e r _)  = (NE.length e, length r)
evalExprLengths (Property _ r _) = (1, length r)

-- |A one-line Haskell statement
type Statement = Loc String

-- | The Haskell statements to feed to GHCi for an 'EvalExpr', each tagged with
-- its source line so evaluation errors can be located.
asStatements :: EvalExpr -> [Statement]
asStatements lt =
  locate $ Located (fromIntegral $ evalExprRange lt ^. L.start . L.line) (asStmts lt)

-- | The raw statement lines of an 'EvalExpr'. A 'Property' is wrapped so its
-- result is evaluated through 'propEvaluation' (see 'propSetup').
asStmts :: EvalExpr -> [Txt]
asStmts (Example e _ _) = NE.toList e
asStmts (Property t _ _) =
    ["prop11 = " ++ t, "(propEvaluation prop11 :: IO String)"]

-- | A wrapper of 'InteractiveEval.execStmt', capturing the execution result
execStmtCaptureResult ::
     Recorder (WithPriority Log)
  -> String
  -> ExecOptions
  -> Ghc (Either String (Maybe String))
execStmtCaptureResult recorder stmt opts = do
    (result, (output, execResultE)) <-
      withCaptureResult recorder $
        withCaptureStdHandles opts $
           gStrictTry (execStmt stmt opts)
    case execResultE of
      Left exc ->
        pure $ Left exc
      Right (ExecComplete (Left err) _) ->
        pure $ Left $ show err
      Right (ExecComplete (Right _) _) -> do
        pure $ Right $ toMaybe (output <> result)
      Right ExecBreak{} ->
        pure $ Right $ Just "breakpoints are not supported"
  where
    toMaybe :: String -> Maybe String
    toMaybe x | null x    = Nothing
              | otherwise = Just x

-- 'System.IO.Extra.withTempFile' is specialized to 'IO'.
withTempFile :: (MonadIO m, MonadMask m) => (FilePath -> m b) -> m (String, b)
withTempFile k = do
    bracket
      (liftIO newTempFile)
      (\(_, purgeTempFile) -> liftIO purgeTempFile)
      (\(tempFile, _) -> do
        r <- k tempFile
        o <- liftIO $ readFile' tempFile
        pure (o, r))

-- | Capture the value the statement evaluates to (printed by GHCi via the
-- interactive print function) by writing it to a temporary file.
withCaptureResult :: Recorder (WithPriority Log) -> Ghc a -> Ghc (String, a)
withCaptureResult recorder action = withTempFile $ \resultTemp -> do
    mEvalPrint <-
      listToMaybe <$>
        runDecls
          ("evalPrint x = P.writeFile " <> show resultTemp <> " (P.show x)")
    case mEvalPrint of
      Nothing ->
        logWith recorder Log.Warning $ LogEvalFailedSettingInteractivePrintFunction
      Just evalPrint ->
        modifySession $
          \hsc -> hsc {hsc_IC = setInteractivePrintName (hsc_IC hsc) evalPrint}
    action

-- | Capture output written to @stdout@ and @stderr@ as a side effect of
-- evaluating the statement.
--
-- We redirect the handles from *within* the interpreted program, because the
-- statement writes to the interpreted standard handles, which are not the host
-- handles that 'System.IO.Silently' would redirect -- HLS has already
-- redirected the latter to protect the LSP channel (see
-- 'Development.IDE.Main').
--
-- NB: This redirection is process global, so output written concurrently to
-- @stdout@/@stderr@ from another thread may be captured here, or eval output
-- may leak. base provides no per-thread standard handles, so this is
-- unavoidable with this approach.
withCaptureStdHandles ::
     ExecOptions
  -> Ghc a
  -> Ghc (String, a)
withCaptureStdHandles opts action = withTempFile $ \outputTemp -> do
    bracket
      (execStmt (captureSetup outputTemp) opts)
      -- Restore the handles no matter how the statement terminated.
      (\_ -> execStmt captureTeardown opts)
      (\_ -> action)

-- Open a temporary file and redirect the interpreted @stdout@/@stderr@ to
-- it, saving the original handles in interactive bindings so 'captureTeardown'
-- can restore them. Bound to a tuple (rather than evaluated as a bare
-- expression) so GHCi does not pass it to the interactive print function.
captureSetup :: FilePath -> String
-- Squeeze into one line (executed by GHCi).
captureSetup outputTemp = unwords
    [ "(__hls_captureHandle, __hls_savedStdout, __hls_savedStderr) <- do {"
    , "  __hls_h <- System.IO.openFile", show outputTemp, "System.IO.WriteMode;"
    , "  System.IO.hSetBuffering __hls_h System.IO.LineBuffering;"
    , "  __hls_o <- GHC.IO.Handle.hDuplicate System.IO.stdout;"
    , "  __hls_e <- GHC.IO.Handle.hDuplicate System.IO.stderr;"
    , "  GHC.IO.Handle.hDuplicateTo __hls_h System.IO.stdout;"
    , "  GHC.IO.Handle.hDuplicateTo __hls_h System.IO.stderr;"
    , "  P.return (__hls_h, __hls_o, __hls_e);"
    , "  }"
    ]

-- Flush, restore the original handles and close the temporary file so the
-- host can read back the captured output.
captureTeardown :: String
-- Squeeze into one line (executed by GHCi).
captureTeardown = unwords
    [ "__hls_restored <- do {"
    , "  System.IO.hFlush System.IO.stdout;"
    , "  System.IO.hFlush System.IO.stderr;"
    , "  GHC.IO.Handle.hDuplicateTo __hls_savedStdout System.IO.stdout;"
    , "  GHC.IO.Handle.hDuplicateTo __hls_savedStderr System.IO.stderr;"
    , "  System.IO.hClose __hls_savedStdout;"
    , "  System.IO.hClose __hls_savedStderr;"
    , "  System.IO.hClose __hls_captureHandle;"
    , "  }"
    ]

{- | GHC declarations required to evaluate property 'EvalExprs'

Example:

prop> \(l::[Bool]) -> reverse (reverse l) == l
+++ OK, passed 100 evalExprs.

prop> \(l::[Bool]) -> reverse l == l
*** Failed! Falsified (after 6 evalExprs and 2 shrinks):
[True,False]
-}
propSetup :: [Loc [Char]]
propSetup =
    locate0
        [ ":set -XScopedTypeVariables -XExplicitForAll"
        , "import qualified Test.QuickCheck as Q11"
        , "propEvaluation p = Q11.quickCheckWithResult Q11.stdArgs p >>= error . Q11.output" -- uses `error` to get a multi-line display
        ]
