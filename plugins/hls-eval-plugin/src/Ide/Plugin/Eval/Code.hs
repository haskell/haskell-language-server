{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wwarn -fno-warn-orphans #-}

-- | Expression execution
module Ide.Plugin.Eval.Code (Statement, testRanges, resultRange, evalExtensions, evalSetup, evalExpr, propSetup, testCheck, asStatements) where

import           Control.Lens                   ((^.))
import           Data.Algorithm.Diff            (Diff, PolyDiff (..), getDiff)
import qualified Data.List.NonEmpty             as NE
import           Data.String                    (IsString)
import qualified Data.Text                      as T
import           Development.IDE.Types.Location (Position (..), Range (..))
import           GHC                            (InteractiveImport (IIDecl), compileExpr)
import           GHC.LanguageExtensions.Type    (Extension (..))
import           GhcMonad                       (Ghc, GhcMonad, liftIO)
import           Ide.Plugin.Eval.Types          (Language (Plain), Loc,
                                                 Located (..),
                                                 Section (sectionLanguage),
                                                 Test (..), Txt, locate,
                                                 locate0)
import           InteractiveEval                (getContext, parseImportDecl, runDecls, setContext)
import           Language.LSP.Types.Lens        (line, start)
import           Unsafe.Coerce                  (unsafeCoerce)

-- | Return the ranges of the expression and result parts of the given test
testRanges :: Test -> (Range, Range)
testRanges tst =
    let startLine = testRange tst ^. start.line
        (exprLines, resultLines) = testLenghts tst
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

testCheck :: (Section, Test) -> [T.Text] -> [T.Text]
testCheck (section, test) out
    | null (testOutput test) || sectionLanguage section == Plain = out
    | otherwise = showDiffs $ getDiff (map T.pack $ testOutput test) out

testLenghts :: Test -> (Int, Int)
testLenghts (Example e r _)  = (NE.length e, length r)
testLenghts (Property _ r _) = (1, length r)

-- |A one-line Haskell statement
type Statement = Loc String

asStatements :: Test -> [Statement]
asStatements lt = locate $ Located (testRange lt ^. start.line) (asStmts lt)

asStmts :: Test -> [Txt]
asStmts (Example e _ _) = NE.toList e
asStmts (Property t _ _) =
    ["prop11 = " ++ t, "(propEvaluation prop11 :: IO String)"]

-- |Evaluate an expression (either a pure expression or an IO a)
evalExpr :: GhcMonad m => [Char] -> m String
evalExpr e = do
    res <- compileExpr $ "asPrint (" ++ e ++ ")"
    liftIO (unsafeCoerce res :: IO String)

-- |GHC extensions required for expression evaluation
evalExtensions :: [Extension]
evalExtensions =
    [ OverlappingInstances
    , UndecidableInstances
    , FlexibleInstances
    , IncoherentInstances
    , TupleSections
    , ExtendedDefaultRules
    ]

-- |GHC declarations required for expression evaluation
evalSetup :: Ghc ()
evalSetup = do
    preludeAsP <- parseImportDecl "import qualified Prelude as P"
    context <- getContext
    setContext (IIDecl preludeAsP : context)
    mapM_
        runDecls
        [ "class Print f where asPrint :: f -> P.IO P.String"
        , "instance P.Show a => Print (P.IO a) where asPrint io = io P.>>= P.return P.. P.show"
        , "instance P.Show a => Print a where asPrint a = P.return (P.show a)"
        ]

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
