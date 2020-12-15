{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

#include "ghc-api-version.h"

module Ide.Plugin.HaddockComments where

import Control.Monad (join)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map
import qualified Data.Text as T
import Development.IDE
import Development.IDE.GHC.Compat
import Ide.Types
import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types hiding (GhcPs)
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.LSP.Types

-----------------------------------------------------------------------------
descriptor :: PluginId -> PluginDescriptor
descriptor plId =
  (defaultPluginDescriptor plId)
    { pluginCodeActionProvider = Just codeActionProvider
    }

haddockCommentsId :: CommandId
haddockCommentsId = "HaddockCommentsCommand"

codeActionProvider :: CodeActionProvider
codeActionProvider _lspFuncs ideState _pId (TextDocumentIdentifier uri) range CodeActionContext {_diagnostics = List diags} =
  do
    let noErr = and $ (/= Just DsError) . _severity <$> diags
        nfp = uriToNormalizedFilePath $ toNormalizedUri uri
    (join -> pm) <- runAction "HaddockComments.GetParsedModule" ideState $ use GetParsedModule `traverse` nfp
    let locDecls = hsmodDecls . unLoc . pm_parsed_source <$> pm
        anns = relativiseApiAnns <$> (pm_parsed_source <$> pm) <*> (pm_annotations <$> pm)
        edits = [runGenComments gen locDecls anns range | noErr, gen <- genList]
    return $ Right $ List [CACodeAction $ toAction title uri edit | (Just (title, edit)) <- edits]

genList :: [GenComments]
genList =
  [ genForSig,
    genForRecord
  ]

-----------------------------------------------------------------------------
data GenComments = forall a.
  GenComments
  { title :: T.Text,
    fromDecl :: HsDecl GhcPs -> Maybe a,
    collectKeys :: a -> [AnnKey],
    isFresh :: Annotation -> Bool,
    updateAnn :: Annotation -> Annotation
  }

runGenComments :: GenComments -> Maybe [LHsDecl GhcPs] -> Maybe Anns -> Range -> Maybe (T.Text, TextEdit)
runGenComments GenComments {..} mLocDecls mAnns range
  | Just locDecls <- mLocDecls,
    Just anns <- mAnns,
    [(locDecl, src, x)] <- [(locDecl, l, x) | locDecl@(L l (fromDecl -> Just x)) <- locDecls, inRange range l],
    annKeys <- collectKeys x,
    not $ null annKeys,
    and $ maybe False isFresh . flip Map.lookup anns <$> annKeys,
    anns' <- foldr (Map.adjust updateAnn) anns annKeys,
    Just range' <- calcRange src range,
    result <- T.strip . T.pack $ exactPrint locDecl anns' =
    Just (title, TextEdit range' result)
  | otherwise = Nothing

-----------------------------------------------------------------------------

genForSig :: GenComments
genForSig = GenComments {..}
  where
    title = "Generate signature comments"

    fromDecl (SigD _ (TypeSig _ _ (HsWC _ (HsIB _ x)))) = Just x
    fromDecl _ = Nothing
    updateAnn x = x {annEntryDelta = DP (0, 1), annsDP = dp}

    isFresh Ann {annsDP}
      | null [() | (AnnComment _, _) <- annsDP] = True
      | otherwise = False

    collectKeys = keyFromTyVar 0

    comment = mkComment "-- ^ " noSrcSpan
    dp = [(AnnComment comment, DP (0, 1)), (G AnnRarrow, DP (1, 2))]

genForRecord :: GenComments
genForRecord = GenComments {..}
  where
    title = "Generate fields comments"

    fromDecl (TyClD _ DataDecl {tcdDataDefn = HsDataDefn {dd_cons = cons}}) = Just [x | (L _ ConDeclH98 {con_args = x}) <- cons]
    fromDecl _ = Nothing

    updateAnn x = x {annEntryDelta = DP (1, -7), annPriorComments = [(comment, DP (1, -7))]}

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

calcRange :: SrcSpan -> Range -> Maybe Range
calcRange src range
  | inRange range src,
    (RealSrcSpan span) <- src,
    range' <- realSrcSpanToRange span =
    Just range'
  | otherwise = Nothing

inRange :: Range -> SrcSpan -> Bool
inRange range x = isInsideSrcSpan (_start range) x || isInsideSrcSpan (_end range) x

-----------------------------------------------------------------------------

keyFromTyVar :: Int -> LHsType GhcPs -> [AnnKey]
keyFromTyVar dep c@(L _ (HsFunTy _ x y))
  | dep < 1 = mkAnnKey c : keyFromTyVar dep x ++ keyFromTyVar dep y
  | otherwise = []
#if MIN_GHC_API_VERSION(8,10,0)
keyFromTyVar dep (L _ (HsForAllTy _ _ _ x)) = keyFromTyVar dep x
#else
keyFromTyVar dep (L _ (HsForAllTy _ _ x)) = keyFromTyVar dep x
#endif
keyFromTyVar dep (L _ (HsQualTy _ _ x)) = keyFromTyVar dep x
keyFromTyVar dep (L _ (HsParTy _ x)) = keyFromTyVar (succ dep) x
keyFromTyVar dep (L _ (HsBangTy _ _ x)) = keyFromTyVar dep x
keyFromTyVar _ _ = []

keyFromCon :: [HsConDeclDetails GhcPs] -> [AnnKey]
keyFromCon cons = mconcat [mkAnnKey <$> xs | (RecCon (L _ xs)) <- cons]

-----------------------------------------------------------------------------
