{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE RecordWildCards #-}

module Ide.Plugin.Class.ExactPrint where

import           Control.Monad.Trans.Maybe
import           Data.Either.Extra                       (eitherToMaybe)
import           Data.Functor.Identity                   (Identity)
import qualified Data.Text                               as T
import           Development.IDE.GHC.Compat
import           GHC.Parser.Annotation
import           Ide.Plugin.Class.Types
import           Ide.Plugin.Class.Utils
import           Language.Haskell.GHC.ExactPrint
import           Language.Haskell.GHC.ExactPrint.Parsers
import           Language.LSP.Protocol.Types             (Range)

#if MIN_VERSION_ghc(9,9,0)
import           Control.Lens                            (_head, over)
#endif

makeEditText :: Monad m => ParsedModule -> DynFlags -> AddMinimalMethodsParams -> MaybeT m (T.Text, T.Text)
makeEditText pm df AddMinimalMethodsParams{..} = do
    mDecls <- MaybeT . pure $ traverse (makeMethodDecl df) methodGroup
    let ps =
#if !MIN_VERSION_ghc(9,9,0)
            makeDeltaAst $
#endif
                pm_parsed_source pm

        old = T.pack $ exactPrint ps
#if MIN_VERSION_ghc_exactprint(1,10,0)
        ps' = addMethodDecls ps mDecls range withSig
#else
        (ps', _, _) = runTransform (addMethodDecls ps mDecls range withSig)
#endif
        new = T.pack $ exactPrint ps'
    pure (old, new)

makeMethodDecl :: DynFlags -> (T.Text, T.Text) -> Maybe (LHsDecl GhcPs, LHsDecl GhcPs)
makeMethodDecl df (mName, sig) = do
    name <- eitherToMaybe $ parseDecl df (T.unpack mName) . T.unpack $ toMethodName mName <> " = _"
    sig' <- eitherToMaybe $ parseDecl df (T.unpack sig) $ T.unpack sig
    pure (name, sig')

#if MIN_VERSION_ghc_exactprint(1,10,0)
addMethodDecls :: ParsedSource -> [(LHsDecl GhcPs, LHsDecl GhcPs)] -> Range -> Bool -> Located (HsModule GhcPs)
#else
addMethodDecls :: ParsedSource -> [(LHsDecl GhcPs, LHsDecl GhcPs)] -> Range -> Bool -> TransformT Identity (Located (HsModule GhcPs))
#endif
addMethodDecls ps mDecls range withSig
    | withSig = go (concatMap (\(decl, sig) -> [sig, decl]) mDecls)
    | otherwise = go (map fst mDecls)
    where
    go inserting = do
#if MIN_VERSION_ghc_exactprint(1,10,0)
        let allDecls = hsDecls ps
#else
        allDecls <- hsDecls ps
#endif
        case break (inRange range . getLoc) allDecls of
            (before, L l inst : after) ->
                let
                    instSpan = realSrcSpan $ getLoc l
#if MIN_VERSION_ghc(9,11,0)
                    instCol = srcSpanStartCol instSpan - 1
#else
                    instCol = srcSpanStartCol instSpan
#endif
#if MIN_VERSION_ghc(9,9,0)
                    instRow = srcSpanEndLine instSpan
                    methodEpAnn = noAnnSrcSpanDP $ deltaPos 1 (instCol + defaultIndent)
                    -- Put each TyCl method/type signature on separate line, indented by 2 spaces relative to instance decl
                    newLine (L _ e) = L methodEpAnn e

                    -- Set DeltaPos for following declarations so they don't move undesirably
                    resetFollowing =
                        over _head (\followingDecl ->
                            let followingDeclRow = srcSpanStartLine $ realSrcSpan $ getLoc followingDecl
                                delta = DifferentLine (followingDeclRow - instRow) instCol
                            in setEntryDP followingDecl delta)
#else
                    newLine (L l e) =
                        let dp = deltaPos 1 (instCol + defaultIndent - 1)
                        in L (noAnnSrcSpanDP (getLoc l) dp <> l) e

                    resetFollowing = id
#endif
                in replaceDecls ps (before ++ L l (addWhere inst):(map newLine inserting ++ resetFollowing after))
            (before, []) ->
                replaceDecls ps before

    -- Add `where` keyword for `instance X where` if `where` is missing.
    --
    -- The `where` in ghc-9.2 is now stored in the instance declaration
    --   directly. More precisely, giving an `HsDecl GhcPs`, we have:
    --   InstD --> ClsInstD --> ClsInstDecl --> XCClsInstDecl --> (EpAnn [AddEpAnn], AnnSortKey),
    --   here `AnnEpAnn` keeps the track of Anns.
    --
    -- See the link for the original definition:
    --   https://hackage.haskell.org/package/ghc-9.2.1/docs/Language-Haskell-Syntax-Extension.html#t:XCClsInstDecl
    addWhere :: HsDecl GhcPs -> HsDecl GhcPs
    addWhere instd@(InstD xInstD (ClsInstD ext decl@ClsInstDecl{..})) =
        case cid_ext of
#if MIN_VERSION_ghc(9,11,0)
            (warnings, anns, key)
                | EpTok _ <- acid_where anns -> instd
                | otherwise ->
                    InstD xInstD (ClsInstD ext decl {
                    cid_ext = ( warnings
                              , anns { acid_where = EpTok d1 }
                              , key
                              )
                    })
#elif MIN_VERSION_ghc(9,9,0)
            (warnings, anns, key)
                | any (\(AddEpAnn kw _ )-> kw == AnnWhere) anns -> instd
                | otherwise ->
                    InstD xInstD (ClsInstD ext decl {
                    cid_ext = ( warnings
                              , AddEpAnn AnnWhere d1 : anns
                              , key
                              )
                    })
#else
            (EpAnn entry anns comments, key) ->
                InstD xInstD (ClsInstD ext decl {
                cid_ext = (EpAnn
                            entry
                            (AddEpAnn AnnWhere d1 : anns)
                            comments
                          , key
                          )
                })
            _ -> instd
#endif
    addWhere decl = decl
