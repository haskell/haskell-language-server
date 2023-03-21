{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
{-# OPTIONS_GHC -Wwarn -fno-warn-orphans #-}

-- | Expression execution
module Ide.Plugin.Eval.Code (Statement, testRanges, resultRange, propSetup, testCheck, asStatements,myExecStmt) where

import           Control.Lens                   ((^.))
import           Control.Monad.IO.Class
import           Data.Algorithm.Diff            (Diff, PolyDiff (..), getDiff)
import qualified Data.List.NonEmpty             as NE
import           Data.String                    (IsString)
import qualified Data.Text                      as T
import           Development.IDE.GHC.Compat
import           Development.IDE.Types.Location (Position (..), Range (..))
import           GHC                            (ExecOptions, ExecResult (..),
                                                 execStmt)
import           Ide.Plugin.Eval.Types          (Language (Plain), Loc,
                                                 Located (..),
                                                 Section (sectionLanguage),
                                                 Test (..), Txt, locate,
                                                 locate0)
import           Language.LSP.Types.Lens        (line, start)
import           System.IO.Extra                (newTempFile, readFile')

-- | Return the ranges of the expression and result parts of the given test
testRanges :: Test -> (Range, Range)
testRanges tst =
    let startLine = testRange tst ^. start.line
        (fromIntegral -> exprLines, fromIntegral -> resultLines) = testLengths tst
        resLine = startLine + exprLines
     in ( Range
            (Position startLine 0)
            --(Position (startLine + exprLines + resultLines) 0),
            (Position resLine 0)
        , Range (Position resLine 0) (Position (resLine + resultLines) 0)
        )

{- |The document range where a test is defined
 testRange :: Loc Test -> Range
 testRange = fst . testRanges
-}

-- |The document range where the result of the test is defined
resultRange :: Test -> Range
resultRange = snd . testRanges

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

testCheck :: Bool -> (Section, Test) -> [T.Text] -> [T.Text]
testCheck diff (section, test) out
    | not diff || null (testOutput test) || sectionLanguage section == Plain = out
    | otherwise = showDiffs $ getDiff (map T.pack $ testOutput test) out

testLengths :: Test -> (Int, Int)
testLengths (Example e r _)  = (NE.length e, length r)
testLengths (Property _ r _) = (1, length r)

-- |A one-line Haskell statement
type Statement = Loc String

asStatements :: Test -> [Statement]
asStatements lt = locate $ Located (fromIntegral $ testRange lt ^. start.line) (asStmts lt)

asStmts :: Test -> [Txt]
asStmts (Example e _ _) = NE.toList e
asStmts (Property t _ _) =
    ["prop11 = " ++ t, "(propEvaluation prop11 :: IO String)"]



-- | A wrapper of 'InteractiveEval.execStmt', capturing the execution result
myExecStmt :: String -> ExecOptions -> Ghc (Either String (Maybe String))
myExecStmt stmt opts = do
    (temp, purge) <- liftIO newTempFile
    evalPrint <- head <$> runDecls ("evalPrint x = P.writeFile "<> show temp <> " (P.show x)")
    modifySession $ \hsc -> hsc {hsc_IC = setInteractivePrintName (hsc_IC hsc) evalPrint}
    result <- execStmt stmt opts >>= \case
              ExecComplete (Left err) _ -> pure $ Left $ show err
              ExecComplete (Right _) _ -> liftIO $ Right . (\x -> if null x then Nothing else Just x) <$> readFile' temp
              ExecBreak{} -> pure $ Right $ Just "breakpoints are not supported"
    liftIO purge
    pure result

{- |GHC declarations required to execute test properties

Example:

prop> \(l::[Bool]) -> reverse (reverse l) == l
+++ OK, passed 100 tests.

prop> \(l::[Bool]) -> reverse l == l
*** Failed! Falsified (after 6 tests and 2 shrinks):
[True,False]
-}
propSetup :: [Loc [Char]]
propSetup =
    locate0
        [ ":set -XScopedTypeVariables -XExplicitForAll"
        , "import qualified Test.QuickCheck as Q11"
        , "propEvaluation p = Q11.quickCheckWithResult Q11.stdArgs p >>= error . Q11.output" -- uses `error` to get a multi-line display
        ]
