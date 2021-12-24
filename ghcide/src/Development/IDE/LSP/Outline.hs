{-# LANGUAGE CPP                   #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}

module Development.IDE.LSP.Outline
  ( moduleOutline
  )
where

import           Control.Monad.IO.Class
import           Data.Functor
import           Data.Generics
import           Data.Maybe
import           Data.Text                      (Text, pack)
import qualified Data.Text                      as T
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error      (rangeToRealSrcSpan,
                                                 realSrcSpanToRange)
import           Development.IDE.Types.Location
import           Language.LSP.Server            (LspM)
import           Language.LSP.Types             (DocumentSymbol (..),
                                                 DocumentSymbolParams (DocumentSymbolParams, _textDocument),
                                                 List (..), ResponseError,
                                                 SymbolInformation,
                                                 SymbolKind (SkConstructor, SkField, SkFile, SkFunction, SkInterface, SkMethod, SkModule, SkObject, SkStruct, SkTypeParameter, SkUnknown),
                                                 TextDocumentIdentifier (TextDocumentIdentifier),
                                                 type (|?) (InL), uriToFilePath)

moduleOutline
  :: IdeState -> DocumentSymbolParams -> LspM c (Either ResponseError (List DocumentSymbol |? List SymbolInformation))
moduleOutline ideState DocumentSymbolParams{ _textDocument = TextDocumentIdentifier uri }
  = liftIO $ case uriToFilePath uri of
    Just (toNormalizedFilePath' -> fp) -> do
      mb_decls <- fmap fst <$> runAction "Outline" ideState (useWithStale GetParsedModule fp)
      pure $ Right $ case mb_decls of
        Nothing -> InL (List [])
        Just ParsedModule { pm_parsed_source = L _ltop HsModule { hsmodName, hsmodDecls, hsmodImports } }
          -> let
               declSymbols  = mapMaybe documentSymbolForDecl hsmodDecls
               moduleSymbol = hsmodName >>= \case
                 (L (locA -> (RealSrcSpan l _)) m) -> Just $
                   (defDocumentSymbol l :: DocumentSymbol)
                     { _name  = pprText m
                     , _kind  = SkFile
                     , _range = Range (Position 0 0) (Position 2147483647 0) -- _ltop is 0 0 0 0
                     -- In the lsp spec from 3.16 Position takes a uinteger,
                     -- where uinteger is 0 - 2^31 - 1. lsp-types currently has the type of line
                     -- as Int. So instead of using `maxBound :: Int` we hardcode the maxBound of
                     -- uinteger. 2 ^ 31 - 1 == 2147483647
                     -- Check this issue for tracking https://github.com/haskell/lsp/issues/354
                     -- the change in lsp-types.
                     }
                 _ -> Nothing
               importSymbols = maybe [] pure $
                  documentSymbolForImportSummary
                    (mapMaybe documentSymbolForImport hsmodImports)
               allSymbols    = case moduleSymbol of
                 Nothing -> importSymbols <> declSymbols
                 Just x ->
                   [ x { _children = Just (List (importSymbols <> declSymbols))
                       }
                   ]
             in
               InL (List allSymbols)


    Nothing -> pure $ Right $ InL (List [])

documentSymbolForDecl :: LHsDecl GhcPs -> Maybe DocumentSymbol
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (TyClD _ FamDecl { tcdFam = FamilyDecl { fdLName = L _ n, fdInfo, fdTyVars } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name   = showRdrName n
                  <> (case pprText fdTyVars of
                       "" -> ""
                       t  -> " " <> t
                     )
    , _detail = Just $ pprText fdInfo
    , _kind   = SkFunction
    }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (TyClD _ ClassDecl { tcdLName = L _ name, tcdSigs, tcdTyVars }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name     = showRdrName name
                    <> (case pprText tcdTyVars of
                         "" -> ""
                         t  -> " " <> t
                       )
    , _kind     = SkInterface
    , _detail   = Just "class"
    , _children =
      Just $ List
        [ (defDocumentSymbol l :: DocumentSymbol)
            { _name           = showRdrName n
            , _kind           = SkMethod
            , _selectionRange = realSrcSpanToRange l'
            }
        | L (locA -> (RealSrcSpan l _))  (ClassOpSig _ False names _) <- tcdSigs
        , L (locA -> (RealSrcSpan l' _)) n                            <- names
        ]
    }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (TyClD _ DataDecl { tcdLName = L _ name, tcdDataDefn = HsDataDefn { dd_cons } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name     = showRdrName name
    , _kind     = SkStruct
    , _children =
      Just $ List
        [ (defDocumentSymbol l :: DocumentSymbol)
            { _name           = showRdrName n
            , _kind           = SkConstructor
            , _selectionRange = realSrcSpanToRange l'
            , _children       = Just $ List $ childs
            }
        | con <- dd_cons
        , let (cs, flds) = hsConDeclsBinders con
        , let childs = mapMaybe cvtFld flds
        , L (RealSrcSpan l' _) n <- cs
        ]
    }
  where
    cvtFld :: LFieldOcc GhcPs -> Maybe DocumentSymbol
    cvtFld (L (RealSrcSpan l _) n) = Just $ (defDocumentSymbol l :: DocumentSymbol)
                { _name = showRdrName (unLoc (rdrNameFieldOcc n))
                , _kind = SkField
                }
    cvtFld _  = Nothing
    -- | Extract the record fields of a constructor
    conArgRecordFields (RecCon (L _ lcdfs)) = Just $ List
      [ (defDocumentSymbol l :: DocumentSymbol)
          { _name = showRdrName n
          , _kind = SkField
          }
      | L _ cdf <- lcdfs
      , L (locA -> (RealSrcSpan l _)) n <- rdrNameFieldOcc . unLoc <$> cd_fld_names cdf
      ]
    conArgRecordFields _ = Nothing
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (TyClD _ SynDecl { tcdLName = L (locA -> (RealSrcSpan l' _)) n })) = Just
  (defDocumentSymbol l :: DocumentSymbol) { _name           = showRdrName n
                                          , _kind           = SkTypeParameter
                                          , _selectionRange = realSrcSpanToRange l'
                                          }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (InstD _ ClsInstD { cid_inst = ClsInstDecl { cid_poly_ty } }))
  = Just (defDocumentSymbol l :: DocumentSymbol) { _name = pprText cid_poly_ty
                                                 , _kind = SkInterface
                                                 }
#if MIN_VERSION_ghc(9,2,0)
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (InstD _ DataFamInstD { dfid_inst = DataFamInstDecl FamEqn { feqn_tycon, feqn_pats } }))
#else
documentSymbolForDecl (L (RealSrcSpan l _) (InstD _ DataFamInstD { dfid_inst = DataFamInstDecl HsIB { hsib_body = FamEqn { feqn_tycon, feqn_pats } } }))
#endif
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name = showRdrName (unLoc feqn_tycon) <> " " <> T.unwords
                (map pprText feqn_pats)
    , _kind = SkInterface
    }
#if MIN_VERSION_ghc(9,2,0)
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (InstD _ TyFamInstD { tfid_inst = TyFamInstDecl _ FamEqn { feqn_tycon, feqn_pats } }))
#else
documentSymbolForDecl (L (RealSrcSpan l _) (InstD _ TyFamInstD { tfid_inst = TyFamInstDecl HsIB { hsib_body = FamEqn { feqn_tycon, feqn_pats } } }))
#endif
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name = showRdrName (unLoc feqn_tycon) <> " " <> T.unwords
                (map pprText feqn_pats)
    , _kind = SkInterface
    }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (DerivD _ DerivDecl { deriv_type })) =
  gfindtype deriv_type <&> \(L (_ :: SrcSpan) name) ->
    (defDocumentSymbol l :: DocumentSymbol) { _name = pprText @(HsType GhcPs)
                                              name
                                            , _kind = SkInterface
                                            }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (ValD _ FunBind{fun_id = L _ name})) = Just
    (defDocumentSymbol l :: DocumentSymbol)
      { _name   = showRdrName name
      , _kind   = SkFunction
      }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (ValD _ PatBind{pat_lhs})) = Just
    (defDocumentSymbol l :: DocumentSymbol)
      { _name   = pprText pat_lhs
      , _kind   = SkFunction
      }

documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (ForD _ x)) = Just
  (defDocumentSymbol l :: DocumentSymbol)
    { _name   = case x of
                  ForeignImport{} -> name
                  ForeignExport{} -> name
                  XForeignDecl{}  -> "?"
    , _kind   = SkObject
    , _detail = case x of
                  ForeignImport{} -> Just "import"
                  ForeignExport{} -> Just "export"
                  XForeignDecl{}  -> Nothing
    }
  where name = showRdrName $ unLoc $ fd_name x

documentSymbolForDecl _ = Nothing

-- | Wrap the Document imports into a hierarchical outline for
-- a better overview of symbols in scope.
-- If there are no imports, then no hierarchy will be created.
documentSymbolForImportSummary :: [DocumentSymbol] -> Maybe DocumentSymbol
documentSymbolForImportSummary [] = Nothing
documentSymbolForImportSummary importSymbols =
    let
      -- safe because if we have no ranges then we don't take this branch
      mergeRanges xs = Range (minimum $ map _start xs) (maximum $ map _end xs)
      importRange = mergeRanges $ map (_range :: DocumentSymbol -> Range) importSymbols
    in
      Just (defDocumentSymbol (rangeToRealSrcSpan "" importRange))
          { _name = "imports"
          , _kind = SkModule
          , _children = Just (List importSymbols)
          }

documentSymbolForImport :: LImportDecl GhcPs -> Maybe DocumentSymbol
documentSymbolForImport (L (locA -> (RealSrcSpan l _)) ImportDecl { ideclName, ideclQualified }) = Just
  (defDocumentSymbol l :: DocumentSymbol)
    { _name   = "import " <> pprText ideclName
    , _kind   = SkModule
#if MIN_VERSION_ghc(8,10,0)
    , _detail = case ideclQualified of { NotQualified -> Nothing; _ -> Just "qualified" }
#else
    , _detail = if ideclQualified then Just "qualified" else Nothing
#endif
    }
documentSymbolForImport _ = Nothing

defDocumentSymbol :: RealSrcSpan -> DocumentSymbol
defDocumentSymbol l = DocumentSymbol { .. } where
  _detail         = Nothing
  _deprecated     = Nothing
  _name           = ""
  _kind           = SkUnknown 0
  _range          = realSrcSpanToRange l
  _selectionRange = realSrcSpanToRange l
  _children       = Nothing
  _tags           = Nothing

showRdrName :: RdrName -> Text
showRdrName = pprText

pprText :: Outputable a => a -> Text
pprText = pack . showSDocUnsafe . ppr

-- the version of getConNames for ghc9 is restricted to only the renaming phase
#if !MIN_VERSION_ghc(9,2,0)
getConNames' :: ConDecl GhcPs -> [Located (IdP GhcPs)]
#else
getConNames' :: ConDecl GhcPs -> [XRec GhcPs (IdP GhcPs)]
#endif
getConNames' ConDeclH98  {con_name  = name}  = [name]
getConNames' ConDeclGADT {con_names = names} = names
#if !MIN_VERSION_ghc(8,10,0)
getConNames' (XConDecl NoExt)                = []
#elif !MIN_VERSION_ghc(9,0,0)
getConNames' (XConDecl x)                    = noExtCon x
#endif

hsConDeclsBinders :: LConDecl GhcPs
                  -> ([Located (IdP GhcPs)], [LFieldOcc GhcPs])
   -- See hsLTyClDeclBinders for what this does
   -- The function is boringly complicated because of the records
   -- And since we only have equality, we have to be a little careful
hsConDeclsBinders cons
  = go cons
  where
    go :: LConDecl GhcPs
       -> ([Located (IdP GhcPs)], [LFieldOcc GhcPs])
    go r
      -- Don't re-mangle the location of field names, because we don't
      -- have a record of the full location of the field declaration anyway
      = let loc = getLoc (reLoc r)
        in case unLoc r of
           -- remove only the first occurrence of any seen field in order to
           -- avoid circumventing detection of duplicate fields (#9156)
           ConDeclGADT { con_names = names, con_g_args = args }
             -> (map (L loc . unLoc) names, flds)
             where
                (flds) = get_flds_gadt args

           ConDeclH98 { con_name = name, con_args = args }
             -> ([L loc (unLoc name)], flds)
             where
                flds = get_flds_h98 args

    get_flds_h98 :: HsConDeclH98Details GhcPs
                 -> [LFieldOcc GhcPs]
    get_flds_h98 (RecCon flds) = get_flds (reLoc flds)
    get_flds_h98 _ = []

    get_flds_gadt :: HsConDeclGADTDetails GhcPs
                  -> ([LFieldOcc GhcPs])
    get_flds_gadt (RecConGADT flds) = get_flds (reLoc flds)
    get_flds_gadt _ = []

    get_flds :: Located [LConDeclField GhcPs]
             -> ([LFieldOcc GhcPs])
    get_flds flds = concatMap (cd_fld_names . unLoc) (unLoc flds)

