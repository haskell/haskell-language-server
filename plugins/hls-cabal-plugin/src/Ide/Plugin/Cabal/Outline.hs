{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Ide.Plugin.Cabal.Outline
  ( moduleOutline,
  )
where

import Control.Monad.IO.Class
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding (decodeASCII, decodeLatin1)
import Debug.Trace as Debug
import Development.IDE.Core.Rules
import Development.IDE.Core.Shake (IdeState (shakeExtras), runIdeAction, useWithStaleFast)
import Development.IDE.Types.Location (toNormalizedFilePath')
import Distribution.Fields.Field (Field (Field), Name (Name), FieldName, FieldLine(FieldLine))
import Distribution.Parsec.Position (Position (Position))
import Ide.Plugin.Cabal.Completion.Types (ParseCabalFields (..), cabalPositionToLSPPosition)
import Ide.Plugin.Cabal.Orphans ()
import Ide.Types
import Language.LSP.Protocol.Message qualified as LSP
import Language.LSP.Protocol.Types qualified as LSP

moduleOutline :: PluginMethodHandler IdeState LSP.Method_TextDocumentDocumentSymbol
moduleOutline ideState _ LSP.DocumentSymbolParams {_textDocument = LSP.TextDocumentIdentifier uri} =
  case LSP.uriToFilePath uri of
    Just (toNormalizedFilePath' -> fp) -> do
      mFields <- liftIO $ runIdeAction "cabal-plugin.fields" (shakeExtras ideState) (useWithStaleFast ParseCabalFields fp)
      let debug = fmap fst mFields
      -- Debug.traceShowM debug
      case fmap fst mFields of
        Just fieldPositions -> pure $ LSP.InR (LSP.InL allSymbols)
          where
            allSymbols = mapMaybe documentSymbolForField fieldPositions
        Nothing -> pure $ LSP.InL []
    Nothing -> pure $ LSP.InL []

documentSymbolForField :: Field Position -> Maybe LSP.DocumentSymbol
documentSymbolForField (Field (Name pos fieldName) fieldLines) = Just $ LSP.DocumentSymbol {..}
  where
    _detail = Nothing
    _deprecated = Nothing
    _name = decodeASCII fieldName

    _kind = LSP.SymbolKind_Field
    _range = cabalPositionToLSPRange pos `addNameLengthToLSPRange` decodeASCII fieldName
    _selectionRange = cabalPositionToLSPRange pos `addNameLengthToLSPRange` decodeASCII fieldName
    _children = Just $ mapMaybe documentSymbolForFieldLine fieldLines
    _tags = Nothing

documentSymbolForField _ = Nothing

documentSymbolForFieldLine :: FieldLine Position -> Maybe LSP.DocumentSymbol
documentSymbolForFieldLine (FieldLine pos line) = Just $ LSP.DocumentSymbol {..}
  where
    _detail = Nothing
    _deprecated = Nothing
    _name = decodeLatin1 line -- since there is no ascii invariant (?)

    _kind = LSP.SymbolKind_Field
    _range = cabalPositionToLSPRange pos `addNameLengthToLSPRange` decodeASCII line
    _selectionRange = cabalPositionToLSPRange pos `addNameLengthToLSPRange` decodeASCII line
    _children = Nothing
    _tags = Nothing

cabalPositionToLSPRange :: Position -> LSP.Range
cabalPositionToLSPRange pos = LSP.Range lspPos lspPos
  where lspPos = cabalPositionToLSPPosition pos

addNameLengthToLSPRange :: LSP.Range -> T.Text -> LSP.Range
addNameLengthToLSPRange (LSP.Range pos1 (LSP.Position line char)) name =
                            LSP.Range
                              pos1
                              (LSP.Position line (char + fromIntegral (T.length name)))