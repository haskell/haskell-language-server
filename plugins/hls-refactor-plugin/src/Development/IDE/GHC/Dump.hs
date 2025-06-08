{-# LANGUAGE CPP #-}
module Development.IDE.GHC.Dump(showAstDataHtml) where
import qualified Data.ByteString                       as B
import           Data.Data                             hiding (Fixity)
import           Development.IDE.GHC.Compat            hiding (LocatedA,
                                                        NameAnn)
import           Development.IDE.GHC.Compat.ExactPrint (ExactPrint, exactPrint)
import           Development.IDE.GHC.Compat.Util
import           Generics.SYB                          (ext1Q, ext2Q, extQ)
import           GHC.Hs                                hiding (AnnLet)
import           GHC.Hs.Dump
import           GHC.Plugins                           hiding (AnnLet)
import           Prelude                               hiding ((<>))

-- | Show a GHC syntax tree in HTML.
showAstDataHtml :: (Data a, ExactPrint a) => a -> SDoc
showAstDataHtml a0 = html $
    header $$
    body (tag' [("id",text (show @String "myUL"))] "ul" $ vcat
        [
            li (pre $ text (exactPrint a0)),
            li (showAstDataHtml' a0),
            li (nested "Raw" $ pre $ showAstData NoBlankSrcSpan NoBlankEpAnnotations a0)
        ])
  where
    tag = tag' []
    tag' attrs t cont =
        angleBrackets (text t <+> hcat [text a<>char '=' <>v | (a,v) <- attrs])
        <> cont
        <> angleBrackets (char '/' <> text t)
    ul = tag' [("class", text (show @String "nested"))] "ul"
    li = tag "li"
    caret x = tag' [("class", text "caret")] "span" "" <+> x
    nested foo cts
      | otherwise = foo $$ (caret $ ul cts)
    body cts = tag "body" $ cts $$ tag "script" (text js)
    header = tag "head" $ tag "style" $ text css
    html = tag "html"
    pre = tag "pre"
    showAstDataHtml' :: Data a => a -> SDoc
    showAstDataHtml' =
      generic
              `ext1Q` list
              `extQ` string `extQ` fastString `extQ` srcSpan `extQ` realSrcSpan
#if !MIN_VERSION_ghc(9,11,0)
              `extQ` annotation
#endif
              `extQ` annotationModule
#if !MIN_VERSION_ghc(9,11,0)
              `extQ` annotationAddEpAnn
#endif
              `extQ` annotationGrhsAnn
              `extQ` annotationEpAnnHsCase
              `extQ` annotationEpAnnHsLet
              `extQ` annotationAnnList
              `extQ` annotationEpAnnImportDecl
              `extQ` annotationAnnParen
              `extQ` annotationTrailingAnn
              `extQ` annotationEpaLocation
#if !MIN_VERSION_ghc(9,11,0)
              `extQ` addEpAnn
#endif
              `extQ` lit `extQ` litr `extQ` litt
              `extQ` sourceText
              `extQ` deltaPos
              `extQ` epaAnchor
#if !MIN_VERSION_ghc(9,9,0)
              `extQ` anchorOp
#endif
              `extQ` bytestring
              `extQ` name `extQ` occName `extQ` moduleName `extQ` var
              `extQ` dataCon
              `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
              `extQ` fixity
              `ext2Q` located
              `extQ` srcSpanAnnA
              `extQ` srcSpanAnnL
              `extQ` srcSpanAnnP
              `extQ` srcSpanAnnC
              `extQ` srcSpanAnnN

      where generic :: Data a => a -> SDoc
            generic t = nested (text $ showConstr (toConstr t))
                     (vcat (gmapQ (li . showAstDataHtml') t))

            string :: String -> SDoc
            string = text . normalize_newlines . show

            fastString :: FastString -> SDoc
            fastString s = braces $
                            text "FastString:"
                        <+> text (normalize_newlines . show $ s)

            bytestring :: B.ByteString -> SDoc
            bytestring = text . normalize_newlines . show

            list []  = brackets empty
            list [x] = "[]" $$ showAstDataHtml' x
            list xs  = nested "[]" (vcat $ map (li . showAstDataHtml') xs)

            -- Eliminate word-size dependence
            lit :: HsLit GhcPs -> SDoc
            lit (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            lit (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            lit (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            lit (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            lit l                  = generic l

            litr :: HsLit GhcRn -> SDoc
            litr (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            litr (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            litr (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            litr (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            litr l                  = generic l

            litt :: HsLit GhcTc -> SDoc
            litt (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            litt (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            litt (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            litt (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            litt l                  = generic l

            numericLit :: String -> Integer -> SourceText -> SDoc
            numericLit tag x s = braces $ hsep [ text tag
                                               , generic x
                                               , generic s ]

            sourceText :: SourceText -> SDoc
            sourceText NoSourceText     = text "NoSourceText"

#if MIN_VERSION_ghc(9,7,0)
            sourceText (SourceText src) = text "SourceText" <+> ftext src
#else
            sourceText (SourceText src) = text "SourceText" <+> text src
#endif

            epaAnchor :: EpaLocation -> SDoc

#if MIN_VERSION_ghc(9,9,0)
            epaAnchor (EpaSpan s) = parens $ text "EpaSpan" <+> srcSpan s
#else
            epaAnchor (EpaSpan r _)  = text "EpaSpan" <+> realSrcSpan r
#endif

#if MIN_VERSION_ghc(9,11,0)
            epaAnchor (EpaDelta s d cs) = text "EpaDelta" <+> srcSpan s <+> deltaPos d <+> showAstDataHtml' cs
#else
            epaAnchor (EpaDelta d cs) = text "EpaDelta" <+> deltaPos d <+> showAstDataHtml' cs
#endif

#if !MIN_VERSION_ghc(9,9,0)
            anchorOp :: AnchorOperation -> SDoc
            anchorOp UnchangedAnchor  = "UnchangedAnchor"
            anchorOp (MovedAnchor dp) = "MovedAnchor " <> deltaPos dp
#endif

            deltaPos :: DeltaPos -> SDoc
            deltaPos (SameLine c) = text "SameLine" <+> ppr c
            deltaPos (DifferentLine l c) = text "DifferentLine" <+> ppr l <+> ppr c

            name :: Name -> SDoc
            name nm    = braces $ text "Name:" <+> ppr nm

            occName n  =  braces $
                          text "OccName:"
                      <+> text (occNameString n)

            moduleName :: ModuleName -> SDoc
            moduleName m = braces $ text "ModuleName:" <+> ppr m

            srcSpan :: SrcSpan -> SDoc
            srcSpan ss = char ' ' <>
                             hang (ppr ss) 1
                                   -- TODO: show annotations here
                                   (text "")

            realSrcSpan :: RealSrcSpan -> SDoc
            realSrcSpan ss = braces $ char ' ' <>
                             hang (ppr ss) 1
                                   -- TODO: show annotations here
                                   (text "")

#if !MIN_VERSION_ghc(9,11,0)
            addEpAnn :: AddEpAnn -> SDoc
            addEpAnn (AddEpAnn a s) = text "AddEpAnn" <+> ppr a <+> epaAnchor s
#endif

            var  :: Var -> SDoc
            var v      = braces $ text "Var:" <+> ppr v

            dataCon :: DataCon -> SDoc
            dataCon c  = braces $ text "DataCon:" <+> ppr c

            bagRdrName:: Bag (LocatedA (HsBind GhcPs)) -> SDoc
            bagRdrName bg =  braces $
                             text "Bag(LocatedA (HsBind GhcPs)):"
                          $$ (list . bagToList $ bg)

            bagName   :: Bag (LocatedA (HsBind GhcRn)) -> SDoc
            bagName bg  =  braces $
                           text "Bag(LocatedA (HsBind Name)):"
                        $$ (list . bagToList $ bg)

            bagVar    :: Bag (LocatedA (HsBind GhcTc)) -> SDoc
            bagVar bg  =  braces $
                          text "Bag(LocatedA (HsBind Var)):"
                       $$ (list . bagToList $ bg)

            nameSet ns =  braces $
                          text "NameSet:"
                       $$ (list . nameSetElemsStable $ ns)

            fixity :: Fixity -> SDoc
            fixity fx =  braces $
                         text "Fixity:"
                     <+> ppr fx

            located :: (Data a, Data b) => GenLocated a b -> SDoc
            located (L ss a)
              = nested "L" (li (showAstDataHtml' ss) $$ li (showAstDataHtml' a))

            -- -------------------------

#if !MIN_VERSION_ghc(9,11,0)
            annotation :: EpAnn [AddEpAnn] -> SDoc
            annotation = annotation' (text "EpAnn [AddEpAnn]")
#endif

            annotationModule :: EpAnn AnnsModule -> SDoc
            annotationModule = annotation' (text "EpAnn AnnsModule")

#if !MIN_VERSION_ghc(9,11,0)
            annotationAddEpAnn :: EpAnn AddEpAnn -> SDoc
            annotationAddEpAnn = annotation' (text "EpAnn AddEpAnn")
#endif

            annotationGrhsAnn :: EpAnn GrhsAnn -> SDoc
            annotationGrhsAnn = annotation' (text "EpAnn GrhsAnn")

            annotationEpAnnHsCase :: EpAnn EpAnnHsCase -> SDoc
            annotationEpAnnHsCase = annotation' (text "EpAnn EpAnnHsCase")

            annotationEpAnnHsLet :: EpAnn NoEpAnns -> SDoc
            annotationEpAnnHsLet = annotation' (text "EpAnn NoEpAnns")

#if MIN_VERSION_ghc(9,11,0)
            annotationAnnList :: EpAnn (AnnList ()) -> SDoc
#else
            annotationAnnList :: EpAnn AnnList -> SDoc
#endif
            annotationAnnList = annotation' (text "EpAnn AnnList")

            annotationEpAnnImportDecl :: EpAnn EpAnnImportDecl -> SDoc
            annotationEpAnnImportDecl = annotation' (text "EpAnn EpAnnImportDecl")

            annotationAnnParen :: EpAnn AnnParen -> SDoc
            annotationAnnParen = annotation' (text "EpAnn AnnParen")

            annotationTrailingAnn :: EpAnn TrailingAnn -> SDoc
            annotationTrailingAnn = annotation' (text "EpAnn TrailingAnn")

            annotationEpaLocation :: EpAnn EpaLocation -> SDoc
            annotationEpaLocation = annotation' (text "EpAnn EpaLocation")

            annotation' :: forall a. Data a => SDoc -> EpAnn a -> SDoc
            annotation' _tag anns = nested (text $ showConstr (toConstr anns))
              (vcat (map li $ gmapQ showAstDataHtml' anns))

            -- -------------------------

#if MIN_VERSION_ghc(9,9,0)
            srcSpanAnnA :: EpAnn AnnListItem -> SDoc
            srcSpanAnnA = locatedAnn'' (text "SrcSpanAnnA")

#if MIN_VERSION_ghc(9,11,0)
            srcSpanAnnL :: EpAnn (AnnList ()) -> SDoc
#else
            srcSpanAnnL :: EpAnn AnnList -> SDoc
#endif
            srcSpanAnnL = locatedAnn'' (text "SrcSpanAnnL")

            srcSpanAnnP :: EpAnn AnnPragma -> SDoc
            srcSpanAnnP = locatedAnn'' (text "SrcSpanAnnP")

            srcSpanAnnC :: EpAnn AnnContext -> SDoc
            srcSpanAnnC = locatedAnn'' (text "SrcSpanAnnC")

            srcSpanAnnN :: EpAnn NameAnn -> SDoc
            srcSpanAnnN = locatedAnn'' (text "SrcSpanAnnN")

            locatedAnn'' :: forall a. Data a => SDoc -> EpAnn a -> SDoc
            locatedAnn'' tag ss = parens $
              case cast ss of
                Just (ann :: EpAnn a) ->
                      text (showConstr (toConstr ann))
                                          $$ vcat (gmapQ showAstDataHtml' ann)
                Nothing -> text "locatedAnn:unmatched" <+> tag
                           <+> (parens $ text (showConstr (toConstr ss)))
#else
            srcSpanAnnA :: SrcSpanAnn' (EpAnn AnnListItem) -> SDoc
            srcSpanAnnA = locatedAnn'' (text "SrcSpanAnnA")

            srcSpanAnnL :: SrcSpanAnn' (EpAnn AnnList) -> SDoc
            srcSpanAnnL = locatedAnn'' (text "SrcSpanAnnL")

            srcSpanAnnP :: SrcSpanAnn' (EpAnn AnnPragma) -> SDoc
            srcSpanAnnP = locatedAnn'' (text "SrcSpanAnnP")

            srcSpanAnnC :: SrcSpanAnn' (EpAnn AnnContext) -> SDoc
            srcSpanAnnC = locatedAnn'' (text "SrcSpanAnnC")

            srcSpanAnnN :: SrcSpanAnn' (EpAnn NameAnn) -> SDoc
            srcSpanAnnN = locatedAnn'' (text "SrcSpanAnnN")

            locatedAnn'' :: forall a. Data a
              => SDoc -> SrcSpanAnn' a -> SDoc
            locatedAnn'' tag ss =
              case cast ss of
                Just ((SrcSpanAnn ann s) :: SrcSpanAnn' a) ->
                      nested "SrcSpanAnn" (
                                 li(showAstDataHtml' ann)
                              $$ li(srcSpan s))
                Nothing -> text "locatedAnn:unmatched" <+> tag
                           <+> text (showConstr (toConstr ss))
#endif


normalize_newlines :: String -> String
normalize_newlines ('\\':'r':'\\':'n':xs) = '\\':'n':normalize_newlines xs
normalize_newlines (x:xs)                 = x:normalize_newlines xs
normalize_newlines []                     = []

css :: String
css = unlines
  [ "body {background-color: black; color: white ;}"
  , "/* Remove default bullets */"
  , "ul, #myUL {"
  , "  list-style-type: none;"
  , "}"
  , "/* Remove margins and padding from the parent ul */"
  , "#myUL {"
  , "  margin: 0;                       "
  , "  padding: 0;                      "
  , "}                                  "
  , "/* Style the caret/arrow */        "
  , ".caret {                           "
  , "  cursor: pointer;                 "
  , "  user-select: none; /* Prevent text selection */"
  , "}                                  "
  , "/* Create the caret/arrow with a unicode, and style it */"
  , ".caret::before {                   "
  , "  content: \"\\25B6 \";                "
  , "  color: white;                    "
  , "  display: inline-block;           "
  , "  margin-right: 6px;               "
  , "}                                  "
  , "/* Rotate the caret/arrow icon when clicked on (using JavaScript) */"
  , ".caret-down::before {              "
  , "  transform: rotate(90deg);        "
  , "}                                  "
  , "/* Hide the nested list */         "
  , ".nested {                          "
  , "  display: none;                   "
  , "}                                  "
  , "/* Show the nested list when the user clicks on the caret/arrow (with JavaScript) */"
  , ".active {                          "
  , "  display: block;}"
  ]

js :: String
js = unlines
  [ "var toggler = document.getElementsByClassName(\"caret\");"
  , "var i;"
  , "for (i = 0; i < toggler.length; i++) {"
  , "  toggler[i].addEventListener(\"click\", function() {"
  , "    this.parentElement.querySelector(\".nested\").classList.toggle(\"active\");"
  , "    this.classList.toggle(\"caret-down\");"
  , "  }); }"
  ]
