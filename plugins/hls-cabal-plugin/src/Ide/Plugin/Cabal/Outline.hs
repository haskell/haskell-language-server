{-# LANGUAGE CPP                   #-}

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ViewPatterns          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards #-}


module Ide.Plugin.Cabal.Outline
    ( moduleOutline
    )
where

import           Control.Monad.IO.Class
import           Data.Maybe
import           Development.IDE.Core.Rules
import Development.IDE.Core.Shake ( IdeState(shakeExtras), runIdeAction, useWithStaleFast )
import Development.IDE.Types.Location ( toNormalizedFilePath')
import           Ide.Types
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Protocol.Message           as LSP

import Data.Text.Encoding (decodeASCII)

import           Ide.Plugin.Cabal.Completion.Types           (ParseCabalFields (..))
import           Ide.Plugin.Cabal.Orphans                    ()

import Distribution.Fields.Field                             (Field (Field), Name (Name))
import Distribution.Parsec.Position                          (Position (Position))

import qualified Data.Text                      as T
import           Debug.Trace                    as Debug

moduleOutline :: PluginMethodHandler IdeState LSP.Method_TextDocumentDocumentSymbol
moduleOutline ideState _ LSP.DocumentSymbolParams{ _textDocument = LSP.TextDocumentIdentifier uri }
  = case LSP.uriToFilePath uri of
    Just (toNormalizedFilePath' -> fp) -> do
      mFields <- liftIO $ runIdeAction "cabal-plugin.fields" (shakeExtras ideState) (useWithStaleFast ParseCabalFields fp)
      let debug = fmap fst mFields
      -- Debug.traceShowM debug
      case fmap fst mFields of
        Just fieldPositions -> pure $ LSP.InR (LSP.InL allSymbols)
          where
            allSymbols = mapMaybe documentSymbolForField fieldPositions
          -- pure $ InR (InL [DocumentSymbol {_name="hello!"
          --                               ,_detail=Nothing
          --                               ,_kind=SymbolKind_Module
          --                               ,_tags=Nothing
          --                               ,_range=mkRange 1 0 1 11
          --                               ,_deprecated=Nothing
          --                               ,_selectionRange=mkRange 1 0 1 11
          --                               ,_children=Nothing}])
        Nothing -> pure $ LSP.InL []
    Nothing -> pure $ LSP.InL []

documentSymbolForField :: Field Position -> Maybe LSP.DocumentSymbol
documentSymbolForField (Field (Name pos@(Position line char) fieldName) _ )= Just $ LSP.DocumentSymbol { .. } where
  _detail         = Nothing
  _deprecated     = Nothing
  _name           = decodeASCII fieldName

  _kind           = LSP.SymbolKind_Field
  _range          = LSP.Range (parserToLSPPosition pos) (parserToLSPPosition (Position line char))
  _selectionRange = LSP.Range (parserToLSPPosition pos) (parserToLSPPosition (Position line char))
  _children       = Nothing
  _tags           = Nothing

  -- addNameLength :: UInt -> FieldName -> UInt
  -- addNameLength char name = toEnum (fromEnum char + length name)
documentSymbolForField _ = Nothing


parserToLSPPosition :: Position -> LSP.Position
parserToLSPPosition (Position start end) = LSP.Position (toEnum start) (toEnum end)