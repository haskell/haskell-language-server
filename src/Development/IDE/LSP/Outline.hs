{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
#include "ghc-api-version.h"

module Development.IDE.LSP.Outline
  ( setHandlersOutline
    -- * For haskell-language-server
  , moduleOutline
  )
where

import qualified Language.Haskell.LSP.Core     as LSP
import           Language.Haskell.LSP.Messages
import           Language.Haskell.LSP.Types
import           Data.Functor
import           Data.Generics
import           Data.Maybe
import           Data.Text                      ( Text
                                                , pack
                                                )
import qualified Data.Text                     as T
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error      ( realSrcSpanToRange )
import           Development.IDE.LSP.Server
import           Development.IDE.Types.Location
import           Outputable                     ( Outputable
                                                , ppr
                                                , showSDocUnsafe
                                                )

setHandlersOutline :: PartialHandlers c
setHandlersOutline = PartialHandlers $ \WithMessage {..} x -> return x
  { LSP.documentSymbolHandler = withResponse RspDocumentSymbols moduleOutline
  }

moduleOutline
  :: LSP.LspFuncs c -> IdeState -> DocumentSymbolParams -> IO (Either ResponseError DSResult)
moduleOutline _lsp ideState DocumentSymbolParams { _textDocument = TextDocumentIdentifier uri }
  = case uriToFilePath uri of
    Just (toNormalizedFilePath' -> fp) -> do
      mb_decls <- fmap fst <$> runIdeAction "Outline" (shakeExtras ideState) (useWithStaleFast GetParsedModule fp)
      pure $ Right $ case mb_decls of
        Nothing -> DSDocumentSymbols (List [])
        Just ParsedModule { pm_parsed_source = L _ltop HsModule { hsmodName, hsmodDecls, hsmodImports } }
          -> let
               declSymbols  = mapMaybe documentSymbolForDecl hsmodDecls
               moduleSymbol = hsmodName >>= \case
                 (L (RealSrcSpan l) m) -> Just $
                   (defDocumentSymbol l :: DocumentSymbol)
                     { _name  = pprText m
                     , _kind  = SkFile
                     , _range = Range (Position 0 0) (Position maxBound 0) -- _ltop is 0 0 0 0
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
               DSDocumentSymbols (List allSymbols)


    Nothing -> pure $ Right $ DSDocumentSymbols (List [])

documentSymbolForDecl :: Located (HsDecl GhcPs) -> Maybe DocumentSymbol
documentSymbolForDecl (L (RealSrcSpan l) (TyClD _ FamDecl { tcdFam = FamilyDecl { fdLName = L _ n, fdInfo, fdTyVars } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name   = showRdrName n
                  <> (case pprText fdTyVars of
                       "" -> ""
                       t  -> " " <> t
                     )
    , _detail = Just $ pprText fdInfo
    , _kind   = SkClass
    }
documentSymbolForDecl (L (RealSrcSpan l) (TyClD _ ClassDecl { tcdLName = L _ name, tcdSigs, tcdTyVars }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name     = showRdrName name
                    <> (case pprText tcdTyVars of
                         "" -> ""
                         t  -> " " <> t
                       )
    , _kind     = SkClass
    , _detail   = Just "class"
    , _children =
      Just $ List
        [ (defDocumentSymbol l :: DocumentSymbol)
            { _name           = showRdrName n
            , _kind           = SkMethod
            , _selectionRange = realSrcSpanToRange l'
            }
        | L (RealSrcSpan l)  (ClassOpSig _ False names _) <- tcdSigs
        , L (RealSrcSpan l') n                            <- names
        ]
    }
documentSymbolForDecl (L (RealSrcSpan l) (TyClD _ DataDecl { tcdLName = L _ name, tcdDataDefn = HsDataDefn { dd_cons } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name     = showRdrName name
    , _kind     = SkStruct
    , _children =
      Just $ List
        [ (defDocumentSymbol l :: DocumentSymbol)
            { _name           = showRdrName n
            , _kind           = SkConstructor
            , _selectionRange = realSrcSpanToRange l'
            , _children       = conArgRecordFields (getConArgs x)
            }
        | L (RealSrcSpan l ) x <- dd_cons
        , L (RealSrcSpan l') n <- getConNames x
        ]
    }
  where
    -- | Extract the record fields of a constructor
    conArgRecordFields (RecCon (L _ lcdfs)) = Just $ List
      [ (defDocumentSymbol l :: DocumentSymbol)
          { _name = showRdrName n
          , _kind = SkField
          }
      | L _ cdf <- lcdfs
      , L (RealSrcSpan l) n <- rdrNameFieldOcc . unLoc <$> cd_fld_names cdf
      ]
    conArgRecordFields _ = Nothing
documentSymbolForDecl (L (RealSrcSpan l) (TyClD _ SynDecl { tcdLName = L (RealSrcSpan l') n })) = Just
  (defDocumentSymbol l :: DocumentSymbol) { _name           = showRdrName n
                                          , _kind           = SkTypeParameter
                                          , _selectionRange = realSrcSpanToRange l'
                                          }
documentSymbolForDecl (L (RealSrcSpan l) (InstD _ ClsInstD { cid_inst = ClsInstDecl { cid_poly_ty } }))
  = Just (defDocumentSymbol l :: DocumentSymbol) { _name = pprText cid_poly_ty
                                                 , _kind = SkInterface
                                                 }
documentSymbolForDecl (L (RealSrcSpan l) (InstD _ DataFamInstD { dfid_inst = DataFamInstDecl HsIB { hsib_body = FamEqn { feqn_tycon, feqn_pats } } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name = showRdrName (unLoc feqn_tycon) <> " " <> T.unwords
                (map pprText feqn_pats)
    , _kind = SkInterface
    }
documentSymbolForDecl (L (RealSrcSpan l) (InstD _ TyFamInstD { tfid_inst = TyFamInstDecl HsIB { hsib_body = FamEqn { feqn_tycon, feqn_pats } } }))
  = Just (defDocumentSymbol l :: DocumentSymbol)
    { _name = showRdrName (unLoc feqn_tycon) <> " " <> T.unwords
                (map pprText feqn_pats)
    , _kind = SkInterface
    }
documentSymbolForDecl (L (RealSrcSpan l) (DerivD _ DerivDecl { deriv_type })) =
  gfindtype deriv_type <&> \(L (_ :: SrcSpan) name) ->
    (defDocumentSymbol l :: DocumentSymbol) { _name = pprText @(HsType GhcPs)
                                              name
                                            , _kind = SkInterface
                                            }
documentSymbolForDecl (L (RealSrcSpan l) (ValD _ FunBind{fun_id = L _ name})) = Just
    (defDocumentSymbol l :: DocumentSymbol)
      { _name   = showRdrName name
      , _kind   = SkFunction
      }
documentSymbolForDecl (L (RealSrcSpan l) (ValD _ PatBind{pat_lhs})) = Just
    (defDocumentSymbol l :: DocumentSymbol)
      { _name   = pprText pat_lhs
      , _kind   = SkFunction
      }

documentSymbolForDecl (L (RealSrcSpan l) (ForD _ x)) = Just
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
      Just (defDocumentSymbol empty :: DocumentSymbol)
          { _name = "imports"
          , _kind = SkModule
          , _children = Just (List importSymbols)
          , _range = importRange
          , _selectionRange = importRange
          }

documentSymbolForImport :: Located (ImportDecl GhcPs) -> Maybe DocumentSymbol
documentSymbolForImport (L (RealSrcSpan l) ImportDecl { ideclName, ideclQualified }) = Just
  (defDocumentSymbol l :: DocumentSymbol)
    { _name   = "import " <> pprText ideclName
    , _kind   = SkModule
#if MIN_GHC_API_VERSION(8,10,0)
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

showRdrName :: RdrName -> Text
showRdrName = pprText

pprText :: Outputable a => a -> Text
pprText = pack . showSDocUnsafe . ppr
