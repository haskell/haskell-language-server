{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ViewPatterns              #-}

module Ide.Plugin.HaddockComments (descriptor, E.Log) where

import           Control.Monad                         (join, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class             (lift)
import qualified Data.HashMap.Strict                   as HashMap
import qualified Data.Map                              as Map
import qualified Data.Text                             as T
import           Development.IDE                       hiding (pluginHandlers)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.ExactPrint        (GetAnnotatedParsedSource (..))
import qualified Development.IDE.GHC.ExactPrint        as E
import           Development.IDE.Plugin.CodeAction
import           Ide.Plugin.HaddockComments.Data       (genForDataDecl)
import           Ide.Plugin.HaddockComments.Prelude
import           Ide.Types
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Types hiding (GhcPs)
import           Language.Haskell.GHC.ExactPrint.Utils
import           Language.LSP.Types

-----------------------------------------------------------------------------
descriptor :: Recorder (WithPriority E.Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = mkExactprintPluginDescriptor recorder $
  (defaultPluginDescriptor plId)
    { pluginHandlers = mkPluginHandler STextDocumentCodeAction codeActionProvider
    }

codeActionProvider :: PluginMethodHandler IdeState TextDocumentCodeAction
codeActionProvider ideState _pId (CodeActionParams _ _ (TextDocumentIdentifier uri) range CodeActionContext {_diagnostics = List diags}) =
  do
    let noErr = and $ (/= Just DsError) . _severity <$> diags
        nfp = uriToNormalizedFilePath $ toNormalizedUri uri
    (join -> pm) <- liftIO $ runAction "HaddockComments.GetAnnotatedParsedSource" ideState $ use GetAnnotatedParsedSource `traverse` nfp
    let locDecls = hsmodDecls . unLoc . astA <$> pm
        anns = annsA <$> pm
        edits = [gen locDecls anns range | noErr, gen <- genList]
    return $ Right $ List [InR $ toAction title uri edit | (Just (title, edit)) <- edits]

genList :: [Maybe [LHsDecl GhcPs] -> Maybe Anns -> Range -> Maybe (T.Text, TextEdit)]
genList =
  [ runGenCommentsSimple genForSig,
    runGenComments genForDataDecl
  ]

-----------------------------------------------------------------------------

runGenComments :: GenComments -> Maybe [LHsDecl GhcPs] -> Maybe Anns -> Range -> Maybe (T.Text, TextEdit)
runGenComments GenComments{..} mLocDecls mAnns range
  | Just locDecls <- mLocDecls,
    Just anns <- mAnns,
    [(locDecl, src)] <- [(locDecl, l) | locDecl@(L l _) <- locDecls, range `isIntersectWith` l],
    Just range' <- toRange src,
    Just (_, (anns', _), _) <- runTransformT anns (updateAnns locDecl),
    result <- T.strip . T.pack $ exactPrint locDecl anns'
    = Just (title, TextEdit range' result)
  | otherwise = Nothing

runGenCommentsSimple :: GenCommentsSimple -> Maybe [LHsDecl GhcPs] -> Maybe Anns -> Range -> Maybe (T.Text, TextEdit)
runGenCommentsSimple GenCommentsSimple {..} = runGenComments GenComments {
        title = title,
        updateAnns = updateAnns
    }
  where
    updateAnns :: LHsDecl GhcPs -> TransformT Maybe ()
    updateAnns locDecl@(L _ decl) = do
        x <- lift $ fromDecl decl
        let annKeys = collectKeys x
        anns <- getAnnsT
        when (null annKeys || not (and $ maybe False isFresh . flip Map.lookup anns <$> annKeys)) $
            lift Nothing
        let declKey = mkAnnKey locDecl
            anns' = Map.adjust updateDeclAnn declKey $ foldr (Map.adjust updateAnn) anns annKeys
        putAnnsT anns'

-----------------------------------------------------------------------------

genForSig :: GenCommentsSimple
genForSig = GenCommentsSimple {..}
  where
    title = "Generate signature comments"

    fromDecl (SigD _ (TypeSig _ _ (HsWC _ (HsIB _ x)))) = Just x
    fromDecl _                                          = Nothing

    updateAnn x = x {annEntryDelta = DP (0, 1), annsDP = dp}
    updateDeclAnn = cleanPriorComments

    isFresh Ann {annsDP} = null [() | (AnnComment _, _) <- annsDP]
    collectKeys = keyFromTyVar 0

#if MIN_VERSION_ghc(9,2,0)
    comment = mkComment "-- ^ " (spanAsAnchor noSrcSpan)
#elif MIN_VERSION_ghc(9,0,0)
    comment = mkComment "-- ^ " badRealSrcSpan
#else
    comment = mkComment "-- ^ " noSrcSpan
#endif
    dp = [(AnnComment comment, DP (0, 1)), (G AnnRarrow, DP (1, 2))]

-----------------------------------------------------------------------------

toAction :: T.Text -> Uri -> TextEdit -> CodeAction
toAction title uri edit = CodeAction {..}
  where
    _title = title
    _kind = Just CodeActionQuickFix
    _diagnostics = Nothing
    _command = Nothing
    _changes = Just $ HashMap.singleton uri $ List [edit]
    _documentChanges = Nothing
    _edit = Just WorkspaceEdit {..}
    _isPreferred = Nothing
    _disabled = Nothing
    _xdata = Nothing
    _changeAnnotations = Nothing


toRange :: SrcSpan -> Maybe Range
toRange src
  | (RealSrcSpan s _) <- src,
    range' <- realSrcSpanToRange s =
    Just range'
  | otherwise = Nothing

isIntersectWith :: Range -> SrcSpan -> Bool
isIntersectWith Range {_start, _end} x = isInsideSrcSpan _start x || isInsideSrcSpan _end x

-- clean prior comments, since src span we get from 'LHsDecl' does not include them
cleanPriorComments :: Annotation -> Annotation
cleanPriorComments x = x {annPriorComments = []}

-----------------------------------------------------------------------------

keyFromTyVar :: Int -> LHsType GhcPs -> [AnnKey]
#if MIN_VERSION_ghc(9,0,0)
-- GHC9 HsFunTy has 4 arguments, we could extract this
keyFromTyVar dep c@(L _ (HsFunTy _ _ x y))
#else
keyFromTyVar dep c@(L _ (HsFunTy _ x y))
#endif
  | dep < 1 = mkAnnKey c : keyFromTyVar dep x ++ keyFromTyVar dep y
  | otherwise = []
keyFromTyVar dep (L _ t@HsForAllTy {}) = keyFromTyVar dep (hst_body t)
keyFromTyVar dep (L _ t@HsQualTy {}) = keyFromTyVar dep (hst_body t)
keyFromTyVar dep (L _ (HsKindSig _ x _)) = keyFromTyVar dep x
keyFromTyVar dep (L _ (HsParTy _ x)) = keyFromTyVar (succ dep) x
keyFromTyVar dep (L _ (HsBangTy _ _ x)) = keyFromTyVar dep x
keyFromTyVar _ _ = []

-----------------------------------------------------------------------------
