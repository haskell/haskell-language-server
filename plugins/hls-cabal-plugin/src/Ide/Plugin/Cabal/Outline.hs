{-# LANGUAGE CPP                   #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE OverloadedStrings     #-}


module Ide.Plugin.Cabal.Outline
    ( moduleOutline
    )
where

import           Control.Monad.IO.Class
import           Data.Foldable                  (toList)
import           Data.Functor
import           Data.List.NonEmpty             (nonEmpty)
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
                                                 type (|?) (InL, InR), uriToFilePath, mkRange, SymbolInformation (_deprecated))
import Language.LSP.Protocol.Message
    ( Method(Method_TextDocumentDocumentSymbol) )

import qualified Data.Text                      as T

moduleOutline :: PluginMethodHandler IdeState Method_TextDocumentDocumentSymbol
moduleOutline ideState _ DocumentSymbolParams{ _textDocument = TextDocumentIdentifier uri }
  = liftIO $ case uriToFilePath uri of
    Just (toNormalizedFilePath' -> fp) -> pure $ InR (InL [DocumentSymbol {_name="hello!"
                                                                          ,_detail=Nothing
                                                                          ,_kind=SymbolKind_Module
                                                                          ,_tags=Nothing
                                                                          ,_range=mkRange 1 0 1 11
                                                                          ,_deprecated=Nothing
                                                                          ,_selectionRange=mkRange 1 0 1 11
                                                                          ,_children=Nothing}])
    Nothing -> pure $ InL []