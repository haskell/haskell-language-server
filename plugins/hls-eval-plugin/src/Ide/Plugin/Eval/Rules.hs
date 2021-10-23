{-# LANGUAGE CPP             #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Eval.Rules (GetEvalComments(..), rules) where

import qualified Data.Map.Strict                      as Map
import           Development.IDE                      (GetParsedModuleWithComments (GetParsedModuleWithComments),
                                                       Rules,
                                                       defineNoDiagnostics,
                                                       fromNormalizedFilePath,
                                                       realSrcSpanToRange,
                                                       useWithStale_)
import           Development.IDE.Core.PositionMapping (toCurrentRange)
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat           as SrcLoc
import qualified Development.IDE.GHC.Compat.Util      as FastString
import           Ide.Plugin.Eval.Types


rules :: Rules ()
rules = do
    evalParsedModuleRule

#if MIN_VERSION_ghc(9,0,0)
pattern RealSrcSpanAlready :: SrcLoc.RealSrcSpan -> SrcLoc.RealSrcSpan
pattern RealSrcSpanAlready x = x
apiAnnComments' :: SrcLoc.ApiAnns -> [SrcLoc.RealLocated AnnotationComment]
apiAnnComments' = apiAnnRogueComments
#else
apiAnnComments' :: SrcLoc.ApiAnns -> [SrcLoc.Located AnnotationComment]
apiAnnComments' = concat . Map.elems . snd

pattern RealSrcSpanAlready :: SrcLoc.RealSrcSpan -> SrcSpan
pattern RealSrcSpanAlready x = SrcLoc.RealSrcSpan x Nothing
#endif

evalParsedModuleRule :: Rules ()
evalParsedModuleRule = defineNoDiagnostics $ \GetEvalComments nfp -> do
    (ParsedModule{..}, posMap) <- useWithStale_ GetParsedModuleWithComments nfp
    return $ Just $
                foldMap (\case
                L (RealSrcSpanAlready real) bdy
                    | FastString.unpackFS (srcSpanFile real) ==
                        fromNormalizedFilePath nfp
                    , let ran0 = realSrcSpanToRange real
                    , Just curRan <- toCurrentRange posMap ran0
                    ->

                        -- since Haddock parsing is unset explicitly in 'getParsedModuleWithComments',
                        -- we can concentrate on these two
                        case bdy of
                            AnnLineComment cmt ->
                                mempty { lineComments = Map.singleton curRan (RawLineComment cmt) }
                            AnnBlockComment cmt ->
                                mempty { blockComments = Map.singleton curRan $ RawBlockComment cmt }
                            _ -> mempty
                _ -> mempty
            )
            $ apiAnnComments' pm_annotations
