{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}

module Ide.Plugin.Class.ExactPrint where

import           Control.Lens                            (Identity)
import           Control.Monad.Trans.Maybe
import qualified Data.Text                               as T
import           Development.IDE.GHC.Compat
import           Ide.Plugin.Class.Types
import           Ide.Plugin.Class.Utils
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers
import           Language.LSP.Types

#if MIN_VERSION_ghc(9,2,0)
import           Data.Either.Extra                       (eitherToMaybe)
import           GHC.Parser.Annotation
#else
import           Control.Monad                           (foldM)
import qualified Data.Map.Strict                         as Map
import           Language.Haskell.GHC.ExactPrint.Types   hiding (GhcPs)
import           Language.Haskell.GHC.ExactPrint.Utils   (rs)
#endif

makeEditText :: Monad m => ParsedModule -> DynFlags -> AddMinimalMethodsParams -> MaybeT m (T.Text, T.Text)
-- addMethodDecls :: ParsedSource -> [(LHsDecl GhcPs, LHsDecl GhcPs)] -> Range -> Bool -> TransformT Identity (Located HsModule)
#if MIN_VERSION_ghc(9,2,0)
makeEditText pm df AddMinimalMethodsParams{..} = do
    List mDecls <- MaybeT . pure $ traverse (makeMethodDecl df) methodGroup
    let ps = makeDeltaAst $ pm_parsed_source pm
        old = T.pack $ exactPrint ps
        (ps', _, _) = runTransform (addMethodDecls ps mDecls range withSig)
        new = T.pack $ exactPrint ps'
    pure (old, new)

makeMethodDecl :: DynFlags -> (T.Text, T.Text) -> Maybe (LHsDecl GhcPs, LHsDecl GhcPs)
makeMethodDecl df (mName, sig) = do
    name <- eitherToMaybe $ parseDecl df (T.unpack mName) . T.unpack $ toMethodName mName <> " = _"
    sig' <- eitherToMaybe $ parseDecl df (T.unpack sig) $ T.unpack sig
    pure (name, sig')

addMethodDecls ps mDecls range withSig
    | withSig = go (concatMap (\(decl, sig) -> [sig, decl]) mDecls)
    | otherwise = go (map fst mDecls)
    where
    go inserting = do
        allDecls <- hsDecls ps
        let (before, ((L l inst): after)) = break (inRange range . getLoc) allDecls
        replaceDecls ps (before ++ (L l (addWhere inst)): (map newLine inserting ++ after))
    -- Add `where` keyword for `instance X where` if `where` is missing.
    --
    -- The `where` in ghc-9.2 is now stored in the instance declaration
    --   directly. More precisely, giving an `HsDecl GhcPs`, we have:
    --   InstD --> ClsInstD --> ClsInstDecl --> XCClsInstDecl --> (EpAnn [AddEpAnn], AnnSortKey),
    --   here `AnnEpAnn` keeps the track of Anns.
    --
    -- See the link for the original definition:
    --   https://hackage.haskell.org/package/ghc-9.2.1/docs/Language-Haskell-Syntax-Extension.html#t:XCClsInstDecl
    addWhere (InstD xInstD (ClsInstD ext decl@ClsInstDecl{..})) =
        let (EpAnn entry anns comments, key) = cid_ext
        in InstD xInstD (ClsInstD ext decl {
        cid_ext = (EpAnn
                    entry
                    (AddEpAnn AnnWhere (EpaDelta (SameLine 1) []) : anns)
                    comments
                    , key)
        })
    addWhere decl = decl

    newLine (L l e) =
        let dp = deltaPos 1 defaultIndent
        in L (noAnnSrcSpanDP (getLoc l) dp <> l) e

#else

makeEditText pm df AddMinimalMethodsParams{..} = do
    List (unzip -> (mAnns, mDecls)) <- MaybeT . pure $ traverse (makeMethodDecl df) methodGroup
    let ps = pm_parsed_source pm
        anns = relativiseApiAnns ps (pm_annotations pm)
        old = T.pack $ exactPrint ps anns
        (ps', (anns', _), _) = runTransform (mergeAnns (mergeAnnList mAnns) anns) (addMethodDecls ps mDecls range withSig)
        new = T.pack $ exactPrint ps' anns'
    pure (old, new)

makeMethodDecl :: DynFlags -> (T.Text, T.Text) -> Maybe (Anns, (LHsDecl GhcPs, LHsDecl GhcPs))
makeMethodDecl df (mName, sig) = do
    (nameAnn, name) <- case parseDecl df (T.unpack mName) . T.unpack $ toMethodName mName <> " = _" of
        Right (ann, d) -> Just (setPrecedingLines d 1 defaultIndent ann, d)
        Left _         -> Nothing
    (sigAnn, sig) <- case parseDecl df (T.unpack sig) $ T.unpack sig of
        Right (ann, d) -> Just (setPrecedingLines d 1 defaultIndent ann, d)
        Left _         -> Nothing
    pure (mergeAnnList [nameAnn, sigAnn], (name, sig))

addMethodDecls ps mDecls range withSig = do
    d <- findInstDecl ps range
    newSpan <- uniqueSrcSpanT
    let decls = if withSig then concatMap (\(decl, sig) -> [sig, decl]) mDecls else map fst mDecls
        annKey = mkAnnKey d
        newAnnKey = AnnKey (rs newSpan) (CN "HsValBinds")
        addWhere mkds@(Map.lookup annKey -> Just ann) = Map.insert newAnnKey ann2 mkds2
            where
                ann1 = ann
                        { annsDP = annsDP ann ++ [(G AnnWhere, DP (0, 1))]
                        , annCapturedSpan = Just newAnnKey
                        , annSortKey = Just (fmap (rs . getLoc) decls)
                        }
                mkds2 = Map.insert annKey ann1 mkds
                ann2 = annNone
                        { annEntryDelta = DP (1, defaultIndent)
                        }
        addWhere _ = panic "Ide.Plugin.Class.addMethodPlaceholder"
    modifyAnnsT addWhere
    modifyAnnsT (captureOrderAnnKey newAnnKey decls)
    foldM (insertAfter d) ps (reverse decls)

findInstDecl :: ParsedSource -> Range -> Transform (LHsDecl GhcPs)
findInstDecl ps range = head . filter (inRange range . getLoc) <$> hsDecls ps
#endif
