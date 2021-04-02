{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ViewPatterns              #-}

module Ide.Plugin.HaddockComments (descriptor) where

import           Control.Monad                         (join)
import           Control.Monad.IO.Class
import qualified Data.HashMap.Strict                   as HashMap
import qualified Data.Map                              as Map
import qualified Data.Text                             as T
import           Development.IDE                       hiding (pluginHandlers)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint        (GetAnnotatedParsedSource (..),
                                                        annsA, astA)
import           Ide.Types
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Types hiding (GhcPs)
import           Language.Haskell.GHC.ExactPrint.Utils
import           Language.LSP.Types

-----------------------------------------------------------------------------
descriptor :: PluginId -> PluginDescriptor IdeState
descriptor plId =
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
        edits = [runGenComments gen locDecls anns range | noErr, gen <- genList]
    return $ Right $ List [InR $ toAction title uri edit | (Just (title, edit)) <- edits]

genList :: [GenComments]
genList =
  [ genForSig,
    genForRecord
  ]

-----------------------------------------------------------------------------

-- | Defines how to generate haddock comments by tweaking annotations of AST
data GenComments = forall a.
  GenComments
  { title         :: T.Text,
    fromDecl      :: HsDecl GhcPs -> Maybe a,
    collectKeys   :: a -> [AnnKey],
    isFresh       :: Annotation -> Bool,
    updateAnn     :: Annotation -> Annotation,
    updateDeclAnn :: Annotation -> Annotation
  }

runGenComments :: GenComments -> Maybe [LHsDecl GhcPs] -> Maybe Anns -> Range -> Maybe (T.Text, TextEdit)
runGenComments GenComments {..} mLocDecls mAnns range
  | Just locDecls <- mLocDecls,
    Just anns <- mAnns,
    [(locDecl, src, x)] <- [(locDecl, l, x) | locDecl@(L l (fromDecl -> Just x)) <- locDecls, range `isIntersectWith` l],
    annKeys <- collectKeys x,
    not $ null annKeys,
    and $ maybe False isFresh . flip Map.lookup anns <$> annKeys,
    declKey <- mkAnnKey locDecl,
    anns' <- Map.adjust updateDeclAnn declKey $ foldr (Map.adjust updateAnn) anns annKeys,
    Just range' <- toRange src,
    result <- T.strip . T.pack $ exactPrint locDecl anns' =
    Just (title, TextEdit range' result)
  | otherwise = Nothing

-----------------------------------------------------------------------------

genForSig :: GenComments
genForSig = GenComments {..}
  where
    title = "Generate signature comments"

    fromDecl (SigD _ (TypeSig _ _ (HsWC _ (HsIB _ x)))) = Just x
    fromDecl _                                          = Nothing

    updateAnn x = x {annEntryDelta = DP (0, 1), annsDP = dp}
    updateDeclAnn = cleanPriorComments

    isFresh Ann {annsDP} = null [() | (AnnComment _, _) <- annsDP]
    collectKeys = keyFromTyVar 0

    comment = mkComment "-- ^ " noSrcSpan
    dp = [(AnnComment comment, DP (0, 1)), (G AnnRarrow, DP (1, 2))]

genForRecord :: GenComments
genForRecord = GenComments {..}
  where
    title = "Generate fields comments"

    fromDecl (TyClD _ DataDecl {tcdDataDefn = HsDataDefn {dd_cons = cons}}) =
      Just [x | (L _ ConDeclH98 {con_args = x}) <- cons]
    fromDecl _ = Nothing

    updateAnn x = x {annEntryDelta = DP (1, 2), annPriorComments = [(comment, DP (1, 2))]}
    updateDeclAnn = cleanPriorComments

    isFresh Ann {annPriorComments} = null annPriorComments

    collectKeys = keyFromCon

    comment = mkComment "-- | " noSrcSpan

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
  | (RealSrcSpan s) <- src,
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
keyFromTyVar dep c@(L _ (HsFunTy _ x y))
  | dep < 1 = mkAnnKey c : keyFromTyVar dep x ++ keyFromTyVar dep y
  | otherwise = []
keyFromTyVar dep (L _ t@HsForAllTy {}) = keyFromTyVar dep (hst_body t)
keyFromTyVar dep (L _ t@HsQualTy {}) = keyFromTyVar dep (hst_body t)
keyFromTyVar dep (L _ (HsKindSig _ x _)) = keyFromTyVar dep x
keyFromTyVar dep (L _ (HsParTy _ x)) = keyFromTyVar (succ dep) x
keyFromTyVar dep (L _ (HsBangTy _ _ x)) = keyFromTyVar dep x
keyFromTyVar _ _ = []

keyFromCon :: [HsConDeclDetails GhcPs] -> [AnnKey]
keyFromCon cons = mconcat [mkAnnKey <$> xs | (RecCon (L _ xs)) <- cons]

-----------------------------------------------------------------------------
