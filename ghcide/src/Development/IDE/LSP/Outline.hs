{-# LANGUAGE CPP                   #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}

module Development.IDE.LSP.Outline
  ( moduleOutline
  )
where

import           Control.Monad.IO.Class
import           Data.Functor
import           Data.Generics                  hiding (Prefix)
import           Data.Maybe
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error      (rangeToRealSrcSpan,
                                                 realSrcSpanToRange)
import           Development.IDE.Types.Location
import           Development.IDE.GHC.Util       (printOutputable)
import           Ide.Types
import           Language.LSP.Protocol.Types             (DocumentSymbol (..),
                                                 DocumentSymbolParams (DocumentSymbolParams, _textDocument),
                                                 SymbolKind (..),
                                                 TextDocumentIdentifier (TextDocumentIdentifier),
                                                 type (|?) (InL, InR), uriToFilePath)
import          Language.LSP.Protocol.Message

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

import           Data.List.NonEmpty             (nonEmpty)
import           Data.Foldable                  (toList)

#if !MIN_VERSION_ghc(9,3,0)
import qualified Data.Text                      as T
#endif

moduleOutline
  :: PluginMethodHandler IdeState Method_TextDocumentDocumentSymbol
moduleOutline ideState _ DocumentSymbolParams{ _textDocument = TextDocumentIdentifier uri }
  = liftIO $ case uriToFilePath uri of
    Just (toNormalizedFilePath' -> fp) -> do
      mb_decls <- fmap fst <$> runAction "Outline" ideState (useWithStale GetParsedModule fp)
      pure $ case mb_decls of
        Nothing -> InL []
        Just ParsedModule { pm_parsed_source = L _ltop HsModule { hsmodName, hsmodDecls, hsmodImports } }
          -> let
               declSymbols  = mapMaybe documentSymbolForDecl hsmodDecls
               moduleSymbol = hsmodName >>= \case
                 (L (locA -> (RealSrcSpan l _)) m) -> Just $
                   (defDocumentSymbol l :: DocumentSymbol)
                     { _name  = printOutputable m
                     , _kind  = SymbolKind_File
                     , _range = Range (Position 0 0) (Position maxBound 0) -- _ltop is 0 0 0 0
                     }
                 _ -> Nothing
               importSymbols = maybe [] pure $
                  documentSymbolForImportSummary
                    (mapMaybe documentSymbolForImport hsmodImports)
               allSymbols    = case moduleSymbol of
                 Nothing -> importSymbols <> declSymbols
                 Just x ->
                   [ x { _children = Just (importSymbols <> declSymbols)
                       }
                   ]
             in
               InR (InL allSymbols)


    Nothing -> pure $ InL []

documentSymbolForDecl :: LHsDecl GhcPs -> Maybe DocumentSymbol
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (TyClD _ FamDecl { tcdFam = FamilyDecl { fdLName = L _ n, fdInfo, fdTyVars } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name   = printOutputable n
                  <> (case printOutputable fdTyVars of
                       "" -> ""
                       t  -> " " <> t
                     )
    , _detail = Just $ printOutputable fdInfo
    , _kind   = SymbolKind_Function
    }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (TyClD _ ClassDecl { tcdLName = L _ name, tcdSigs, tcdTyVars }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name     = printOutputable name
                    <> (case printOutputable tcdTyVars of
                         "" -> ""
                         t  -> " " <> t
                       )
    , _kind     = SymbolKind_Interface
    , _detail   = Just "class"
    , _children =
      Just $
        [ (defDocumentSymbol l' :: DocumentSymbol)
            { _name           = printOutputable n
            , _kind           = SymbolKind_Method
            , _selectionRange = realSrcSpanToRange l''
            }
        | L (locA -> (RealSrcSpan l' _))  (ClassOpSig _ False names _) <- tcdSigs
        , L (locA -> (RealSrcSpan l'' _)) n                            <- names
        ]
    }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (TyClD _ DataDecl { tcdLName = L _ name, tcdDataDefn = HsDataDefn { dd_cons } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name     = printOutputable name
    , _kind     = SymbolKind_Struct
    , _children =
      Just $
          [ (defDocumentSymbol l'' :: DocumentSymbol)
            { _name           = printOutputable n
            , _kind           = SymbolKind_Constructor
            , _selectionRange = realSrcSpanToRange l'
            , _children       = toList <$> nonEmpty childs
            }
        | con <- extract_cons dd_cons
        , let (cs, flds) = hsConDeclsBinders con
        , let childs = mapMaybe cvtFld flds
        , L (locA -> RealSrcSpan l' _) n <- cs
        , let l'' = case con of
                L (locA -> RealSrcSpan l''' _) _ -> l'''
                _ -> l'
        ]
    }
  where
    cvtFld :: LFieldOcc GhcPs -> Maybe DocumentSymbol
#if MIN_VERSION_ghc(9,3,0)
    cvtFld (L (locA -> RealSrcSpan l' _) n) = Just $ (defDocumentSymbol l' :: DocumentSymbol)
#else
    cvtFld (L (RealSrcSpan l' _) n) = Just $ (defDocumentSymbol l' :: DocumentSymbol)
#endif
#if MIN_VERSION_ghc(9,3,0)
                { _name = printOutputable (unLoc (foLabel n))
#else
                { _name = printOutputable (unLoc (rdrNameFieldOcc n))
#endif
                , _kind = SymbolKind_Field
                }
    cvtFld _  = Nothing
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (TyClD _ SynDecl { tcdLName = L (locA -> (RealSrcSpan l' _)) n })) = Just
  (defDocumentSymbol l :: DocumentSymbol) { _name           = printOutputable n
                                          , _kind           = SymbolKind_TypeParameter
                                          , _selectionRange = realSrcSpanToRange l'
                                          }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (InstD _ ClsInstD { cid_inst = ClsInstDecl { cid_poly_ty } }))
  = Just (defDocumentSymbol l :: DocumentSymbol) { _name = printOutputable cid_poly_ty
                                                 , _kind = SymbolKind_Interface
                                                 }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (InstD _ DataFamInstD { dfid_inst = DataFamInstDecl FamEqn { feqn_tycon, feqn_pats } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name =
#if MIN_VERSION_ghc(9,3,0)
        printOutputable $ pprHsArgsApp (unLoc feqn_tycon) Prefix (feqn_pats)
#else
        printOutputable (unLoc feqn_tycon) <> " " <> T.unwords
                (map printOutputable feqn_pats)
#endif
    , _kind = SymbolKind_Interface
    }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (InstD _ TyFamInstD { tfid_inst = TyFamInstDecl _ FamEqn { feqn_tycon, feqn_pats } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name =
#if MIN_VERSION_ghc(9,3,0)
        printOutputable $ pprHsArgsApp (unLoc feqn_tycon) Prefix (feqn_pats)
#else
        printOutputable (unLoc feqn_tycon) <> " " <> T.unwords
                (map printOutputable feqn_pats)
#endif
    , _kind = SymbolKind_Interface
    }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (DerivD _ DerivDecl { deriv_type })) =
  gfindtype deriv_type <&> \(L (_ :: SrcSpan) name) ->
    (defDocumentSymbol l :: DocumentSymbol) { _name = printOutputable @(HsType GhcPs)
                                              name
                                            , _kind = SymbolKind_Interface
                                            }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (ValD _ FunBind{fun_id = L _ name})) = Just
    (defDocumentSymbol l :: DocumentSymbol)
      { _name   = printOutputable name
      , _kind   = SymbolKind_Function
      }
documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (ValD _ PatBind{pat_lhs})) = Just
    (defDocumentSymbol l :: DocumentSymbol)
      { _name   = printOutputable pat_lhs
      , _kind   = SymbolKind_Function
      }

documentSymbolForDecl (L (locA -> (RealSrcSpan l _)) (ForD _ x)) = Just
  (defDocumentSymbol l :: DocumentSymbol)
    { _name   = case x of
                  ForeignImport{} -> name
                  ForeignExport{} -> name
    , _kind   = SymbolKind_Object
    , _detail = case x of
                  ForeignImport{} -> Just "import"
                  ForeignExport{} -> Just "export"
    }
  where name = printOutputable $ unLoc $ fd_name x

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
      importRange = mergeRanges $ map (\DocumentSymbol{_range} -> _range) importSymbols
    in
      Just (defDocumentSymbol (rangeToRealSrcSpan "" importRange))
          { _name = "imports"
          , _kind = SymbolKind_Module
          , _children = Just importSymbols
          }

documentSymbolForImport :: LImportDecl GhcPs -> Maybe DocumentSymbol
documentSymbolForImport (L (locA -> (RealSrcSpan l _)) ImportDecl { ideclName, ideclQualified }) = Just
  (defDocumentSymbol l :: DocumentSymbol)
    { _name   = "import " <> printOutputable ideclName
    , _kind   = SymbolKind_Module
    , _detail = case ideclQualified of { NotQualified -> Nothing; _ -> Just "qualified" }
    }
documentSymbolForImport _ = Nothing

defDocumentSymbol :: RealSrcSpan -> DocumentSymbol
defDocumentSymbol l = DocumentSymbol { .. } where
  _detail         = Nothing
  _deprecated     = Nothing
  _name           = ""
  -- This used to be SkUnknown 0, which is invalid, as SymbolKinds start at 1,
  -- therefore, I am replacing it with SymbolKind_File, which is the type for 1
  _kind           = SymbolKind_File
  _range          = realSrcSpanToRange l
  _selectionRange = realSrcSpanToRange l
  _children       = Nothing
  _tags           = Nothing

-- the version of getConNames for ghc9 is restricted to only the renaming phase
hsConDeclsBinders :: LConDecl GhcPs
                  -> ([LIdP GhcPs], [LFieldOcc GhcPs])
   -- See hsLTyClDeclBinders for what this does
   -- The function is boringly complicated because of the records
   -- And since we only have equality, we have to be a little careful
hsConDeclsBinders cons
  = go cons
  where
    go :: LConDecl GhcPs
       -> ([LIdP GhcPs], [LFieldOcc GhcPs])
    go r
      -- Don't re-mangle the location of field names, because we don't
      -- have a record of the full location of the field declaration anyway
      = case unLoc r of
           -- remove only the first occurrence of any seen field in order to
           -- avoid circumventing detection of duplicate fields (#9156)
           ConDeclGADT { con_names = names, con_g_args = args }
             -> (toList names, flds)
             where
                flds = get_flds_gadt args

           ConDeclH98 { con_name = name, con_args = args }
             -> ([name], flds)
             where
                flds = get_flds_h98 args

    get_flds_h98 :: HsConDeclH98Details GhcPs
                 -> [LFieldOcc GhcPs]
    get_flds_h98 (RecCon flds) = get_flds (reLoc flds)
    get_flds_h98 _ = []

    get_flds_gadt :: HsConDeclGADTDetails GhcPs
                  -> [LFieldOcc GhcPs]
#if MIN_VERSION_ghc(9,3,0)
    get_flds_gadt (RecConGADT flds _) = get_flds (reLoc flds)
#else
    get_flds_gadt (RecConGADT flds) = get_flds (reLoc flds)
#endif
    get_flds_gadt _ = []

    get_flds :: Located [LConDeclField GhcPs]
             -> [LFieldOcc GhcPs]
    get_flds flds = concatMap (cd_fld_names . unLoc) (unLoc flds)


