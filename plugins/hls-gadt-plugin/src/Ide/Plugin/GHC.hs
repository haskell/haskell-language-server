{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Ide.Plugin.GHC where

import           Data.Functor                            ((<&>))
import           Data.List.Extra                         (stripInfix)
import qualified Data.Text                               as T
import           Development.IDE
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint
import           Ide.PluginUtils                         (subRange)
import           Language.Haskell.GHC.ExactPrint.Parsers (parseDecl)

#if MIN_VERSION_ghc(9,2,1)
import           GHC.Parser.Annotation                   (AddEpAnn (..),
                                                          Anchor (Anchor),
                                                          AnchorOperation (MovedAnchor),
                                                          DeltaPos (..),
                                                          EpAnn (..),
                                                          EpAnnComments (EpaComments),
                                                          EpaLocation (EpaDelta),
                                                          SrcSpanAnn' (SrcSpanAnn),
                                                          spanAsAnchor)
import           Language.Haskell.GHC.ExactPrint         (showAst)
#else
import qualified Data.Map.Lazy                           as Map
import           Language.Haskell.GHC.ExactPrint.Types   (AnnConName (CN),
                                                          AnnKey (AnnKey),
                                                          Annotation (..),
                                                          DeltaPos (DP),
                                                          KeywordId (G),
                                                          deltaColumn)
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
            con_ext
            [con_name]
#if !MIN_VERSION_ghc(9,2,1)
            con_forall
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
#if MIN_VERSION_ghc(9,2,1)
        renderDetails :: HsConDeclH98Details GP -> HsConDeclGADTDetails GP
        renderDetails (PrefixCon _ args)   = PrefixConGADT args
        renderDetails (InfixCon arg1 arg2) = PrefixConGADT [arg1, arg2]
#if MIN_VERSION_ghc(9,3,0)
        renderDetails (RecCon recs)        = RecConGADT recs noHsUniTok
#else
        renderDetails (RecCon recs)        = RecConGADT recs
#endif

#else
        renderDetails (PrefixCon args)     = PrefixCon args
        renderDetails (InfixCon arg1 arg2) = PrefixCon [arg1, arg2]
        renderDetails (RecCon recs)        = RecCon recs
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
#if MIN_VERSION_ghc(9,2,1)
prettyGADTDecl df decl =
    let old = printOutputable decl
        hsDecl = parseDecl df "unused" (T.unpack old)
        tycld = adjustTyClD hsDecl
    in removeExtraEmptyLine . exactPrint <$> tycld
    where
        adjustTyClD = \case
                Right (L _ (TyClD _ tycld)) -> Right $ adjustDataDecl tycld
                Right x -> Left $ "Expect TyClD but got " <> showAst x
#if MIN_VERSION_ghc(9,3,0)
                Left err -> Left $ printWithoutUniques err
#else
                Left err -> Left $ show err
#endif

        adjustDataDecl DataDecl{..} = DataDecl
            { tcdDExt = adjustWhere tcdDExt
            , tcdDataDefn = tcdDataDefn
                { dd_cons = map adjustCon (dd_cons tcdDataDefn)
                }
            , ..
            }
        adjustDataDecl x = x

        -- Make every data constructor start with a new line and 2 spaces
        adjustCon :: LConDecl GP -> LConDecl GP
        adjustCon (L (SrcSpanAnn _ loc) r) =
            L (SrcSpanAnn (EpAnn (go (spanAsAnchor loc)) (AnnListItem []) (EpaComments [])) loc) r
            where
                go (Anchor a _) = Anchor a (MovedAnchor (DifferentLine 1 2))

        -- Adjust where annotation to the same line of the type constructor
        adjustWhere tcdDExt = tcdDExt <&> map
            (\(AddEpAnn ann l) ->
            if ann == AnnWhere
                then AddEpAnn AnnWhere (EpaDelta (SameLine 1) [])
                else AddEpAnn ann l
            )

        -- Remove the first extra line if exist
        removeExtraEmptyLine s = case stripInfix "\n\n" s of
            Just (x, xs) -> x <> "\n" <> xs
            Nothing      -> s
#else
prettyGADTDecl df decl =
    let old = printOutputable decl
        hsDecl = parseDecl df "unused" (T.unpack old)
        tycld = adjustTyClD hsDecl
    in removeExtraEmptyLine . uncurry (flip exactPrint) <$> tycld
    where
        adjustTyClD = \case
                Right (anns, t@(L _ (TyClD _ _))) -> Right (adjustDataDeclAnns anns, t)
                Right _ -> Left "Expect TyClD"
                Left err -> Left $ show err

        adjustDataDeclAnns = Map.mapWithKey go
            where
                isDataDeclAnn (AnnKey _ (CN name)) = name == "DataDecl"
                isConDeclGADTAnn (AnnKey _ (CN name)) = name == "ConDeclGADT"

                go key ann
                    | isDataDeclAnn key = adjustWhere ann
                    | isConDeclGADTAnn key = adjustCon ann
                    | otherwise = ann

                -- Adjust where annotation to the same line of the type constructor
                adjustWhere Ann{..} = Ann
                    { annsDP = annsDP <&>
                        (\(keyword, dp) ->
                            if keyword == G AnnWhere
                                then (keyword, DP (0, 1))
                                else (keyword, dp))
                    , ..
                    }

                -- Make every data constructor start with a new line and 2 spaces
                --
                -- Here we can't force every GADT constructor has (1, 2)
                -- delta. For the first constructor with (1, 2), it prints
                -- a new line with 2 spaces, but for other constructors
                -- with (1, 2), it will print a new line with 4 spaces.
                --
                -- The original ann parsed with `praseDecl` shows the first
                -- constructor has (1, 4) delta, but others have (1, 0).
                -- Hence, the following code only deal with the first
                -- constructor.
                adjustCon Ann{..} = let c = deltaColumn annEntryDelta
                    in Ann
                    { annEntryDelta = DP $ (1,) $ if c > 0 then 2 else 0
                    , ..
                    }

        -- Remove the first extra line if exist
        removeExtraEmptyLine s = case stripInfix "\n\n" s of
            Just (x, xs) -> x <> "\n" <> xs
            Nothing      -> s

#endif

#if MIN_VERSION_ghc(9,2,1)
wrap :: forall a. WrapXRec GP a => a -> XRec GP a
wrap = wrapXRec @GP
wrapCtxt = id
emptyCtxt = Nothing
unWrap = unXRec @GP
mapX = mapXRec @GP
noUsed = EpAnnNotUsed
#else
wrapCtxt = Just
wrap = L noSrcSpan
emptyCtxt = wrap []
unWrap (L _ r) = r
mapX = fmap
noUsed = noExtField
#endif

#if MIN_VERSION_ghc(9,0,1)
pattern UserTyVar' :: LIdP pass -> HsTyVarBndr flag pass
pattern UserTyVar' s <- UserTyVar _ _ s
#else
pattern UserTyVar' :: LIdP pass -> HsTyVarBndr pass
pattern UserTyVar' s <- UserTyVar _ s
#endif

#if MIN_VERSION_ghc(9,2,1)
implicitTyVars = (wrapXRec @GP mkHsOuterImplicit)
#elif MIN_VERSION_ghc(9,0,1)
implicitTyVars = []
#else
implicitTyVars = HsQTvs noExtField []
#endif
