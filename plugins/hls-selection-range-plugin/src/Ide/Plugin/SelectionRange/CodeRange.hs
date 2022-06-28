{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Ide.Plugin.SelectionRange.CodeRange
    ( CodeRange (..)
    , GetCodeRange(..)
    , codeRangeRule
    , Log
    , useExcept
    ) where

import           Control.DeepSeq                         (NFData)
import           Control.Monad.Except                    (ExceptT (..),
                                                          runExceptT)
import           Control.Monad.Reader                    (runReader)
import           Control.Monad.Trans.Maybe               (MaybeT (MaybeT),
                                                          maybeToExceptT)
import           Data.Coerce                             (coerce)
import           Data.Data                               (Typeable)
import           Data.Hashable
import qualified Data.Map.Strict                         as Map
import           Development.IDE
import           Development.IDE.Core.Rules              (toIdeResult)
import qualified Development.IDE.Core.Shake              as Shake
import           Development.IDE.GHC.Compat              (Annotated,
                                                          HieAST (..),
                                                          HieASTs (getAsts),
                                                          ParsedSource, RefMap)
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.ExactPrint          (GetAnnotatedParsedSource (GetAnnotatedParsedSource))
import           GHC.Generics                            (Generic)
import           Ide.Plugin.SelectionRange.ASTPreProcess (PreProcessEnv (..),
                                                          preProcessAST)
import           Prelude                                 hiding (log)

data Log = LogShake Shake.Log
    | LogBadDependency BadDependencyLog
    | LogNoAST

instance Pretty Log where
    pretty log = case log of
        LogShake shakeLog                 -> pretty shakeLog
        LogBadDependency badDependencyLog -> pretty badDependencyLog
        LogNoAST                          -> "no HieAst exist for file"

data BadDependencyLog = forall rule. Show rule => BadDependencyLog rule

instance Pretty BadDependencyLog where
    pretty (BadDependencyLog rule) = "can not get result from rule " <> pretty (show rule)

data CodeRange = CodeRange Range [CodeRange]
    deriving (Show, Generic)

instance NFData CodeRange

buildCodeRange :: HieAST a -> RefMap a -> Annotated ParsedSource -> CodeRange
buildCodeRange ast refMap _ =
    -- We work on 'HieAST', then convert it to 'CodeRange', so that applications such as selection range and folding
    -- range don't need to care about 'HieAST'
    -- TODO @sloorush actually use 'Annotated ParsedSource' to handle structures not in 'HieAST' properly (for example comments)
    let ast' = runReader (preProcessAST ast) (PreProcessEnv refMap)
    in simplify . astToCodeRange $ ast'

astToCodeRange :: HieAST a -> CodeRange
astToCodeRange (Node _ sp []) = CodeRange (realSrcSpanToRange sp) []
astToCodeRange (Node _ sp children) = CodeRange (realSrcSpanToRange sp) (fmap astToCodeRange children)

-- Remove redundant nodes in 'CodeRange' tree
simplify :: CodeRange -> CodeRange
simplify r@(CodeRange range1 [CodeRange range2 children])
    | range1 == range2 = CodeRange range1 children
    | otherwise = r
simplify r = r

data GetCodeRange = GetCodeRange
    deriving (Eq, Show, Typeable, Generic)

instance Hashable GetCodeRange
instance NFData   GetCodeRange

type instance RuleResult GetCodeRange = CodeRange

-- | Like use, but report absense in 'ExceptT'
useExcept :: IdeRule k v => (BadDependencyLog -> msg) -> k -> NormalizedFilePath -> ExceptT msg Action v
useExcept f rule = maybeToExceptT (f (BadDependencyLog rule)) . MaybeT . use rule

codeRangeRule :: Recorder (WithPriority Log) -> Rules ()
codeRangeRule recorder =
    define (cmapWithPrio LogShake recorder) $ \GetCodeRange file -> handleError recorder $ do
        -- We need both 'HieAST' (for basic AST) and api annotations (for comments and some keywords).
        -- See https://gitlab.haskell.org/ghc/ghc/-/wikis/api-annotations
        HAR{hieAst, refMap} <- useExcept LogBadDependency GetHieAst file
        ast <- maybeToExceptT LogNoAST . MaybeT . pure $
            getAsts hieAst Map.!? (coerce . mkFastString . fromNormalizedFilePath) file
        annPS <- useExcept LogBadDependency GetAnnotatedParsedSource file
        pure $ buildCodeRange ast refMap annPS

-- | Handle error in 'Action'. Returns an 'IdeResult' with no value and no diagnostics on error. (but writes log)
handleError :: Recorder (WithPriority msg) -> ExceptT msg Action a -> Action (IdeResult a)
handleError recorder action' = do
    valueEither <- runExceptT action'
    case valueEither of
        Left msg -> do
            logWith recorder Error msg
            pure $ toIdeResult (Left [])
        Right value -> pure $ toIdeResult (Right value)
