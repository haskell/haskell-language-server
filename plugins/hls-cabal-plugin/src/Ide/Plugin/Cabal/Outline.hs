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
import Development.IDE.Core.Rules
import Development.IDE.Core.Shake (IdeState (shakeExtras), runIdeAction, useWithStaleFast)
import Development.IDE.Types.Location (toNormalizedFilePath')
import Distribution.Fields.Field
  ( Field (Field, Section),
    FieldLine (FieldLine),
    Name (Name),
    SectionArg (SecArgName, SecArgOther, SecArgStr),
  )
import Distribution.Parsec.Position (Position)
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
      case fmap fst mFields of
        Just fieldPositions -> pure $ LSP.InR (LSP.InL allSymbols)
          where
            allSymbols = mapMaybe documentSymbolForField fieldPositions
        Nothing -> pure $ LSP.InL []
    Nothing -> pure $ LSP.InL []

documentSymbolForField :: Field Position -> Maybe LSP.DocumentSymbol
documentSymbolForField (Field (Name pos fieldName) fieldLines) =
  Just
    (defDocumentSymbol range)
      { LSP._name = decodeASCII fieldName,
        LSP._kind = LSP.SymbolKind_Object,
        LSP._children = Just $ mapMaybe documentSymbolForFieldLine fieldLines
      }
  where
    range = cabalPositionToLSPRange pos `addNameLengthToLSPRange` decodeASCII fieldName
documentSymbolForField (Section (Name pos fieldName) sectionArgs fields) =
  Just
    (defDocumentSymbol range)
      { LSP._name = decodeASCII fieldName,
        LSP._kind = LSP.SymbolKind_Object,
        LSP._children =
          Just
            ( mapMaybe documentSymbolForField fields
                ++ mapMaybe documentSymbolForSectionArgs sectionArgs
            )
      }
  where
    range = cabalPositionToLSPRange pos `addNameLengthToLSPRange` decodeASCII fieldName

documentSymbolForSectionArgs :: SectionArg Position -> Maybe LSP.DocumentSymbol
documentSymbolForSectionArgs (SecArgName pos identifier) =
  Just
    (defDocumentSymbol range)
      { LSP._name = decodeLatin1 identifier,
        LSP._kind = LSP.SymbolKind_Variable,
        LSP._children = Nothing
      }
  where
    range = cabalPositionToLSPRange pos `addNameLengthToLSPRange` decodeASCII identifier
documentSymbolForSectionArgs (SecArgStr pos quotedString) =
  Just
    (defDocumentSymbol range)
      { LSP._name = decodeLatin1 quotedString,
        LSP._kind = LSP.SymbolKind_Constant,
        LSP._children = Nothing
      }
  where
    range = cabalPositionToLSPRange pos `addNameLengthToLSPRange` decodeASCII quotedString
documentSymbolForSectionArgs (SecArgOther pos string) =
  Just
    (defDocumentSymbol range)
      { LSP._name = decodeLatin1 string,
        LSP._kind = LSP.SymbolKind_String,
        LSP._children = Nothing
      }
  where
    range = cabalPositionToLSPRange pos `addNameLengthToLSPRange` decodeASCII string

documentSymbolForFieldLine :: FieldLine Position -> Maybe LSP.DocumentSymbol
documentSymbolForFieldLine (FieldLine pos line) =
  Just
    (defDocumentSymbol range)
      { LSP._name = decodeLatin1 line, -- since there is no ascii invariant (?)
        LSP._kind = LSP.SymbolKind_Field,
        LSP._children = Nothing -- can't delete even though the base case covers this (?)
      }
  where
    range = cabalPositionToLSPRange pos `addNameLengthToLSPRange` decodeASCII line

cabalPositionToLSPRange :: Position -> LSP.Range
cabalPositionToLSPRange pos = LSP.Range lspPos lspPos
  where
    lspPos = cabalPositionToLSPPosition pos

addNameLengthToLSPRange :: LSP.Range -> T.Text -> LSP.Range
addNameLengthToLSPRange (LSP.Range pos1 (LSP.Position line char)) name =
  LSP.Range
    pos1
    (LSP.Position line (char + fromIntegral (T.length name)))

defDocumentSymbol :: LSP.Range -> LSP.DocumentSymbol
defDocumentSymbol range = LSP.DocumentSymbol {..}
  where
    _detail = Nothing
    _deprecated = Nothing
    _name = ""
    _kind = LSP.SymbolKind_File
    _range = range
    _selectionRange = range
    _children = Nothing
    _tags = Nothing