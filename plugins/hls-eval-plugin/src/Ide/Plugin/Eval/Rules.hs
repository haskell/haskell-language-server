{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- To avoid warning "Pattern match has inaccessible right hand side"
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Ide.Plugin.Eval.Rules (GetEvalComments(..), rules,queueForEvaluation, unqueueForEvaluation, Log) where

import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Data.HashSet                         (HashSet)
import qualified Data.HashSet                         as Set
import           Data.IORef
import qualified Data.Map.Strict                      as Map
import           Data.String                          (fromString)
import           Development.IDE                      (GetModSummaryWithoutTimestamps (GetModSummaryWithoutTimestamps),
                                                       GetParsedModuleWithComments (GetParsedModuleWithComments),
                                                       IdeState,
                                                       NeedsCompilation (NeedsCompilation),
                                                       NormalizedFilePath,
                                                       RuleBody (RuleNoDiagnostics),
                                                       Rules, defineEarlyCutoff,
                                                       encodeLinkableType,
                                                       fromNormalizedFilePath,
                                                       msrModSummary,
                                                       realSrcSpanToRange,
                                                       useWithStale_,
                                                       use_)
import           Development.IDE.Core.PositionMapping (toCurrentRange)
import           Development.IDE.Core.Rules           (computeLinkableTypeForDynFlags,
                                                       needsCompilationRule)
import           Development.IDE.Core.Shake           (IsIdeGlobal,
                                                       RuleBody (RuleWithCustomNewnessCheck),
                                                       addIdeGlobal,
                                                       getIdeGlobalAction,
                                                       getIdeGlobalState)
import qualified Development.IDE.Core.Shake           as Shake
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat           as SrcLoc
import qualified Development.IDE.GHC.Compat.Util      as FastString
import           Development.IDE.Graph                (alwaysRerun)
import           Ide.Logger         (Pretty (pretty),
                                                       Recorder, WithPriority,
                                                       cmapWithPrio)
#if MIN_VERSION_ghc(9,2,0)
import           GHC.Parser.Annotation
#endif
import           Ide.Plugin.Eval.Types

import qualified Data.ByteString                      as BS

newtype Log = LogShake Shake.Log deriving Show

instance Pretty Log where
  pretty = \case
    LogShake shakeLog -> pretty shakeLog

rules :: Recorder (WithPriority Log) -> Rules ()
rules recorder = do
    evalParsedModuleRule recorder
    redefinedNeedsCompilation recorder
    isEvaluatingRule recorder
    addIdeGlobal . EvaluatingVar =<< liftIO(newIORef mempty)

newtype EvaluatingVar = EvaluatingVar (IORef (HashSet NormalizedFilePath))
instance IsIdeGlobal EvaluatingVar

queueForEvaluation :: IdeState -> NormalizedFilePath -> IO ()
queueForEvaluation ide nfp = do
    EvaluatingVar var <- getIdeGlobalState ide
    atomicModifyIORef' var (\fs -> (Set.insert nfp fs, ()))

unqueueForEvaluation :: IdeState -> NormalizedFilePath -> IO ()
unqueueForEvaluation ide nfp = do
    EvaluatingVar var <- getIdeGlobalState ide
    -- remove the module from the Evaluating state, so that next time it won't evaluate to True
    atomicModifyIORef' var $ \fs -> (Set.delete nfp fs, ())

#if MIN_VERSION_ghc(9,2,0)
#if MIN_VERSION_ghc(9,5,0)
getAnnotations :: Development.IDE.GHC.Compat.Located (HsModule GhcPs) -> [LEpaComment]
getAnnotations (L _ m@(HsModule { hsmodExt = XModulePs {hsmodAnn = anns'}})) =
#else
getAnnotations :: Development.IDE.GHC.Compat.Located HsModule -> [LEpaComment]
getAnnotations (L _ m@(HsModule { hsmodAnn = anns'})) =
#endif
    priorComments annComments <> getFollowingComments annComments
     <> concatMap getCommentsForDecl (hsmodImports m)
     <> concatMap getCommentsForDecl (hsmodDecls m)
       where
         annComments = epAnnComments anns'

getCommentsForDecl :: GenLocated (SrcSpanAnn' (EpAnn ann)) e
                            -> [LEpaComment]
getCommentsForDecl (L (SrcSpanAnn (EpAnn _ _ cs) _) _) = priorComments cs <> getFollowingComments cs
getCommentsForDecl (L (SrcSpanAnn (EpAnnNotUsed) _) _) = []

apiAnnComments' :: ParsedModule -> [SrcLoc.RealLocated EpaCommentTok]
apiAnnComments' pm = do
  L span (EpaComment c _) <- getAnnotations $ pm_parsed_source pm
  pure (L (anchor span) c)

pattern RealSrcSpanAlready :: SrcLoc.RealSrcSpan -> SrcLoc.RealSrcSpan
pattern RealSrcSpanAlready x = x
#else
apiAnnComments' :: ParsedModule -> [SrcLoc.RealLocated AnnotationComment]
apiAnnComments' = apiAnnRogueComments . pm_annotations

pattern RealSrcSpanAlready :: SrcLoc.RealSrcSpan -> SrcLoc.RealSrcSpan
pattern RealSrcSpanAlready x = x
#endif

evalParsedModuleRule :: Recorder (WithPriority Log) -> Rules ()
evalParsedModuleRule recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleNoDiagnostics $ \GetEvalComments nfp -> do
    (pm, posMap) <- useWithStale_ GetParsedModuleWithComments nfp
    let comments = foldMap (\case
                L (RealSrcSpanAlready real) bdy
                    | FastString.unpackFS (srcSpanFile real) ==
                        fromNormalizedFilePath nfp
                    , let ran0 = realSrcSpanToRange real
                    , Just curRan <- toCurrentRange posMap ran0
                    ->

                        -- since Haddock parsing is unset explicitly in 'getParsedModuleWithComments',
                        -- we can concentrate on these two
                        case bdy of
                            EpaLineComment cmt ->
                                mempty { lineComments = Map.singleton curRan (RawLineComment cmt) }
                            EpaBlockComment cmt ->
                                mempty { blockComments = Map.singleton curRan $ RawBlockComment cmt }
                            _ -> mempty
                _ -> mempty
            )
            $ apiAnnComments' pm
        -- we only care about whether the comments are null
        -- this is valid because the only dependent is NeedsCompilation
        fingerPrint = fromString $ if nullComments comments then "" else "1"
    return (Just fingerPrint, Just comments)

isEvaluatingRule :: Recorder (WithPriority Log) -> Rules ()
isEvaluatingRule recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleNoDiagnostics $ \IsEvaluating f -> do
    alwaysRerun
    EvaluatingVar var <- getIdeGlobalAction
    b <- liftIO $ (f `Set.member`) <$> readIORef var
    return (Just (if b then BS.singleton 1 else BS.empty), Just b)

-- Redefine the NeedsCompilation rule to set the linkable type to Just _
-- whenever the module is being evaluated
-- This will ensure that the modules are loaded with linkables
-- and the interactive session won't try to compile them on the fly,
-- leading to much better performance of the evaluate code lens
redefinedNeedsCompilation :: Recorder (WithPriority Log) -> Rules ()
redefinedNeedsCompilation recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleWithCustomNewnessCheck (<=) $ \NeedsCompilation f -> do
    isEvaluating <- use_ IsEvaluating f

    if not isEvaluating then needsCompilationRule f else do
        ms <- msrModSummary . fst <$> useWithStale_ GetModSummaryWithoutTimestamps f
        let df' = ms_hspp_opts ms
            linkableType = computeLinkableTypeForDynFlags df'
            fp = encodeLinkableType $ Just linkableType

        pure (Just fp, Just (Just linkableType))
