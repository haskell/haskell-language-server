{-# LANGUAGE CPP             #-}
{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Ide.Plugin.GHC where

#if !MIN_VERSION_ghc(9,11,0)
import           Data.Functor                            ((<&>))
#endif
import           Data.List.Extra                         (stripInfix)
import qualified Data.Text                               as T
import           Development.IDE
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint
import           GHC.Parser.Annotation                   (DeltaPos (..),
                                                          EpAnn (..),
                                                          EpAnnComments (EpaComments))
#if MIN_VERSION_ghc(9,11,0)
import           GHC.Parser.Annotation                   (EpToken (..))
#endif
import           Ide.PluginUtils                         (subRange)
import           Language.Haskell.GHC.ExactPrint.Parsers (parseDecl)

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

import qualified Data.List.NonEmpty                      as NE

#if !MIN_VERSION_ghc(9,9,0)
import           GHC.Parser.Annotation                   (Anchor (Anchor),
                                                          AnchorOperation (MovedAnchor),
                                                          SrcSpanAnn' (SrcSpanAnn),
                                                          TokenLocation (..),
                                                          spanAsAnchor)
#endif

#if MIN_VERSION_ghc(9,9,0)
import           GHC.Parser.Annotation                   (EpUniToken (..),
                                                          EpaLocation' (..),
                                                          noAnn)
import           Language.Haskell.GHC.ExactPrint.Utils   (showAst)
#endif

#if MIN_VERSION_ghc(9,11,0)
import           GHC.Types.SrcLoc                        (UnhelpfulSpanReason (..))
#else
import           GHC.Parser.Annotation                   (AddEpAnn (..))
#endif

type GP = GhcPass Parsed

-- | Check if a given range is in the range of located item
inRange :: HasSrcSpan a => Range -> a -> Bool
inRange range s = maybe False (subRange range) (srcSpanToRange (getLoc s))

-- | Get data decl and its location
getDataDecl :: LHsDecl GP -> Maybe (LTyClDecl GP)
getDataDecl (L l (TyClD _ d@DataDecl{})) = Just (L l d)
getDataDecl _                            = Nothing

isConDeclH98 :: ConDecl GP -> Bool
isConDeclH98 ConDeclH98{} = True
isConDeclH98 _            = False

isH98DataDecl :: LTyClDecl GP -> Bool
isH98DataDecl (L _ decl@DataDecl{}) =
    any (isConDeclH98 . (\(L _ r) -> r)) (dd_cons $ tcdDataDefn decl)
isH98DataDecl _ = False

-- | Convert H98 data type definition to GADT's
h98ToGADTDecl :: TyClDecl GP -> TyClDecl GP
h98ToGADTDecl = \case
    DataDecl{..} -> DataDecl
        { tcdDataDefn = updateDefn tcdLName tcdTyVars tcdDataDefn
        , ..
        }
    x -> x
    where
        updateDefn dataName tyVars = \case
            HsDataDefn{..} -> HsDataDefn
                { dd_cons =
                    mapX (h98ToGADTConDecl dataName tyVars (wrapCtxt dd_ctxt)) <$> dd_cons
                , dd_ctxt = emptyCtxt -- Context can't appear at the data name in GADT
                , ..
                }
            x -> x

-- | Convert H98 data constructor to GADT data constructor
h98ToGADTConDecl ::
    LIdP GP -- ^Type constructor name,
            -- used for constructing final result type in GADT
    -> LHsQTyVars GP
            -- ^Type variable names
            -- used for constructing final result type in GADT
    -> Maybe (LHsContext GP)
            -- ^Data type context
    -> ConDecl GP
    -> ConDecl GP
h98ToGADTConDecl dataName tyVars ctxt = \case
    ConDeclH98{..} ->
        ConDeclGADT

#if MIN_VERSION_ghc(9,11,0)
            (AnnConDeclGADT [] [] NoEpUniTok)
#elif MIN_VERSION_ghc(9,9,0)
            (NoEpUniTok, con_ext)
#else
            con_ext
#endif

            (NE.singleton con_name)

#if !MIN_VERSION_ghc(9,9,0)
            (L NoTokenLoc HsNormalTok)
#endif
            -- Ignore all existential type variable since GADT not needed
            implicitTyVars
            (mergeContext ctxt con_mb_cxt)
            (renderDetails con_args)
            renderResultTy
            con_doc
    x -> x
    where
        -- Parameters in the data constructor
        renderDetails :: HsConDeclH98Details GP -> HsConDeclGADTDetails GP
#if MIN_VERSION_ghc(9,9,0)
        renderDetails (PrefixCon _ args)   = PrefixConGADT noExtField args
#else
        renderDetails (PrefixCon _ args)   = PrefixConGADT args
#endif
#if MIN_VERSION_ghc(9,9,0)
        renderDetails (InfixCon arg1 arg2) = PrefixConGADT noExtField [arg1, arg2]
#else
        renderDetails (InfixCon arg1 arg2) = PrefixConGADT [arg1, arg2]
#endif
#if MIN_VERSION_ghc(9,9,0)
        renderDetails (RecCon recs)        = RecConGADT NoEpUniTok recs
#else
        renderDetails (RecCon recs)        = RecConGADT recs noHsUniTok
#endif


        -- | Construct GADT result type
        renderResultTy :: LHsType GP
        renderResultTy = case tyVars of
            -- Without type variable
            HsQTvs _ []   -> wrappedDataName
            -- With type variable
            HsQTvs _ vars -> foldl go wrappedDataName vars
            _             -> wrappedDataName
            where
                wrappedDataName = wrap (HsTyVar noUsed NotPromoted dataName)

                -- Bundle data name with type vars by `HsAppTy`
                go acc (L _(UserTyVar' var)) =
                    wrap
                        (HsAppTy noExtField acc
                            (wrap (HsTyVar noUsed NotPromoted var)))
                go acc _ = acc

        -- Merge data type context and constructor type context
        mergeContext :: Maybe (LHsContext GP) -> Maybe (LHsContext GP) -> Maybe (LHsContext GP)
        mergeContext ctxt1 ctxt2 =
            (wrap . map wrap) . map unParTy
                <$> (getContextType ctxt1 <> getContextType ctxt2)
            where
                getContextType :: Maybe (LHsContext GP) -> Maybe [HsType GP]
                getContextType ctxt = map unWrap . unWrap <$> ctxt

                -- Unparen the outmost, it only occurs at the outmost
                -- for a valid type.
                --
                -- Note for context paren rule:
                --
                -- If only one element, it __can__ have a paren type.
                -- If not, there can't have a parent type.
                unParTy :: HsType GP -> HsType GP
                unParTy (HsParTy _ ty) = unWrap ty
                unParTy x              = x
{- |
We use `printOutputable` to print H98 data decl as GADT syntax,
this print is not perfect, it will:

1. Make data name and the `where` key word in different lines.
2. Make the whole data decl prints in one line if there is only one data constructor.
3. The ident size of every data constructor depends on its origin
   format, and may have different ident size between constructors.

Hence, we first use `printOutputable` to get an initial GADT syntax,
then use `ghc-exactprint` to parse the initial result, and finally
adjust the details that mentioned above.

The adjustment includes:

1. Make the `where` key word at the same line of data name.
2. Remove the extra blank line caused by adjustment of `where`.
3. Make every data constructor start with a new line and 2 spaces
-}
prettyGADTDecl :: DynFlags -> TyClDecl GP -> Either String String
prettyGADTDecl df decl =
    let old = printOutputable decl
        hsDecl = parseDecl df "unused" (T.unpack old)
        tycld = adjustTyClD hsDecl
    in removeExtraEmptyLine . exactPrint <$> tycld
    where
        adjustTyClD = \case
                Right (L _ (TyClD _ tycld)) -> Right $ adjustDataDecl tycld
                Right x -> Left $ "Expect TyClD but got " <> showAst x
                Left err -> Left $ printWithoutUniques err

        adjustDataDecl DataDecl{..} = DataDecl
            { tcdDExt = adjustWhere tcdDExt
            , tcdDataDefn = tcdDataDefn
                {
#if MIN_VERSION_ghc(9,11,0)
                dd_ext = adjustDefnWhere (dd_ext tcdDataDefn),
#endif
                dd_cons =
                      fmap adjustCon (dd_cons tcdDataDefn)
                }
            , ..
            }
        adjustDataDecl x = x

        -- Make every data constructor start with a new line and 2 spaces
        adjustCon :: LConDecl GP -> LConDecl GP
#if MIN_VERSION_ghc(9,11,0)
        adjustCon (L _ r) =
            let delta = EpaDelta (UnhelpfulSpan UnhelpfulNoLocationInfo) (DifferentLine 1 2) []
            in L (EpAnn delta (AnnListItem []) (EpaComments [])) r
#elif MIN_VERSION_ghc(9,9,0)
        adjustCon (L _ r) =
            let delta = EpaDelta (DifferentLine 1 3) []
            in L (EpAnn delta (AnnListItem []) (EpaComments [])) r
#else
        adjustCon (L (SrcSpanAnn _ loc) r) =
            let go (Anchor a _) = Anchor a (MovedAnchor (DifferentLine 1 2))
            in L (SrcSpanAnn (EpAnn (go (spanAsAnchor loc)) (AnnListItem []) (EpaComments [])) loc) r
#endif

        -- Adjust where annotation to the same line of the type constructor
#if MIN_VERSION_ghc(9,11,0)
        -- tcdDext is just a placeholder in ghc-9.12
        adjustWhere = id
#else
        adjustWhere tcdDExt = tcdDExt <&>
#if !MIN_VERSION_ghc(9,9,0)
            map
#endif
            (\(AddEpAnn ann l) ->
            if ann == AnnWhere
                then AddEpAnn AnnWhere d1
                else AddEpAnn ann l
            )
#endif

#if MIN_VERSION_ghc(9,11,0)
        adjustDefnWhere annDataDefn
          | andd_where annDataDefn == NoEpTok = annDataDefn
          | otherwise = annDataDefn {andd_where = andd_where'}
          where
            (EpTok (EpaSpan aw)) = andd_where annDataDefn
            andd_where' = EpTok (EpaDelta aw (SameLine 1) [])
#endif
        -- Remove the first extra line if exist
        removeExtraEmptyLine s = case stripInfix "\n\n" s of
            Just (x, xs) -> x <> "\n" <> xs
            Nothing      -> s

wrap :: forall a. WrapXRec GP a => a -> XRec GP a
wrap = wrapXRec @GP
wrapCtxt = id
emptyCtxt = Nothing
unWrap = unXRec @GP
mapX = mapXRec @GP
#if MIN_VERSION_ghc(9,9,0)
noUsed = noAnn
#else
noUsed = EpAnnNotUsed
#endif

pattern UserTyVar' :: LIdP pass -> HsTyVarBndr flag pass
#if MIN_VERSION_ghc(9,11,0)
pattern UserTyVar' s <- HsTvb _ _ (HsBndrVar _ s) _
#else
pattern UserTyVar' s <- UserTyVar _ _ s
#endif

implicitTyVars = wrapXRec @GP mkHsOuterImplicit
