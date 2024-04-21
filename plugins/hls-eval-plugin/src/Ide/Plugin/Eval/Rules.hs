{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module Ide.Plugin.Eval.Rules (GetEvalComments(..), rules,queueForEvaluation, unqueueForEvaluation, Log) where

import           Control.Lens                         (toListOf)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import qualified Data.ByteString                      as BS
import           Data.Data.Lens                       (biplate)
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
                                                       useWithStale_, use_)
import           Development.IDE.Core.PositionMapping (toCurrentRange)
import           Development.IDE.Core.Rules           (computeLinkableTypeForDynFlags,
                                                       needsCompilationRule)
import           Development.IDE.Core.Shake           (IsIdeGlobal,
                                                       RuleBody (RuleWithCustomNewnessCheck),
                                                       addIdeGlobal,
                                                       getIdeGlobalAction,
                                                       getIdeGlobalState)
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat           as SrcLoc
import qualified Development.IDE.GHC.Compat.Util      as FastString
import           Development.IDE.Graph                (alwaysRerun)
import           GHC.Parser.Annotation
import           Ide.Logger                           (Recorder, WithPriority,
                                                       cmapWithPrio)
import           Ide.Plugin.Eval.Types
import Debug.Trace

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
    atomicModifyIORef' var (\fs -> (trace ("TRACE: queueForEvaluation: " <> show nfp ) $ Set.insert nfp fs, ()))

unqueueForEvaluation :: IdeState -> NormalizedFilePath -> IO ()
unqueueForEvaluation ide nfp = do
    EvaluatingVar var <- getIdeGlobalState ide
    -- remove the module from the Evaluating state, so that next time it won't evaluate to True
    atomicModifyIORef' var $ \fs -> (trace ("TRACE: unqueueForEvaluation: " <> show nfp ) $ Set.delete nfp fs, ())

apiAnnComments' :: ParsedModule -> [SrcLoc.RealLocated EpaCommentTok]
apiAnnComments' pm = do
  L span (EpaComment c _) <- getEpaComments $ pm_parsed_source pm
  pure (L (anchor span) c)
  where
#if MIN_VERSION_ghc(9,5,0)
    getEpaComments :: Development.IDE.GHC.Compat.Located (HsModule GhcPs) -> [LEpaComment]
#else
    getEpaComments :: Development.IDE.GHC.Compat.Located HsModule -> [LEpaComment]
#endif
    getEpaComments = toListOf biplate

pattern RealSrcSpanAlready :: SrcLoc.RealSrcSpan -> SrcLoc.RealSrcSpan
pattern RealSrcSpanAlready x = x

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
    b <- fmap (ts2 "isMemberEvaluatingVar" f) . liftIO $ (f `Set.member`) <$> readIORef var
    return (Just (if b then BS.singleton 1 else BS.empty), Just b)

-- Redefine the NeedsCompilation rule to set the linkable type to Just _
-- whenever the module is being evaluated
-- This will ensure that the modules are loaded with linkables
-- and the interactive session won't try to compile them on the fly,
-- leading to much better performance of the evaluate code lens
redefinedNeedsCompilation :: Recorder (WithPriority Log) -> Rules ()
redefinedNeedsCompilation recorder = defineEarlyCutoff (cmapWithPrio LogShake recorder) $ RuleWithCustomNewnessCheck (<=) $ \NeedsCompilation f -> do
    isEvaluating <- ts2 "isEvaluating" f  <$> use_ IsEvaluating f

    if not isEvaluating then needsCompilationRule f else do
        ms <- msrModSummary . fst <$> useWithStale_ GetModSummaryWithoutTimestamps f
        let df' = ms_hspp_opts ms
            linkableType = computeLinkableTypeForDynFlags df'
            fp = encodeLinkableType $ Just linkableType

        pure (Just fp, ts2 "redefinedNeedsCompilation" f $ Just (Just linkableType))

ts2 :: Show a => String -> NormalizedFilePath -> a -> a
ts2 label nfp x = trace ("TRACE: " <> label <> "=" <> show x <> "(" <> show nfp <> ")") x
