{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.Cabal.Outline
  ( moduleOutline,
  )
where

import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text                         as T
import           Data.Text.Encoding                (decodeUtf8)
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake        (IdeState (shakeExtras),
                                                    runIdeAction,
                                                    useWithStaleFast)
import           Development.IDE.Types.Location    (toNormalizedFilePath')
import           Distribution.Fields.Field         (Field (Field, Section),
                                                    Name (Name),
                                                    SectionArg (SecArgName, SecArgOther, SecArgStr))
import           Distribution.Parsec.Position      (Position)
import           Ide.Plugin.Cabal.Completion.Types (ParseCabalFields (..),
                                                    cabalPositionToLSPPosition)
import           Ide.Plugin.Cabal.Orphans          ()
import           Ide.Types                         (PluginMethodHandler)
import qualified Language.LSP.Protocol.Message     as LSP
import qualified Language.LSP.Protocol.Types       as LSP

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

-- | Creates a DocumentSumbol object for the
--   cabal AST, without displaying fieldLines and
--   displaying Section name and SectionArgs in one line
documentSymbolForField :: Field Position -> Maybe LSP.DocumentSymbol
documentSymbolForField (Field (Name pos fieldName) _) =
  Just
    (defDocumentSymbol range)
      { LSP._name = decodeUtf8 fieldName,
        LSP._kind = LSP.SymbolKind_Field,
        LSP._children = Nothing
      }
  where
    range = cabalPositionToLSPRange pos `addNameLengthToLSPRange` decodeUtf8 fieldName
documentSymbolForField (Section (Name pos fieldName) sectionArgs fields) =
  Just
    (defDocumentSymbol range)
      { LSP._name = joinedName,
        LSP._kind = LSP.SymbolKind_Object,
        LSP._children =
          Just
            (mapMaybe documentSymbolForField fields)
      }
  where
    joinedName = decodeUtf8 fieldName <> " " <> joinedNameForSectionArgs sectionArgs
    range = cabalPositionToLSPRange pos `addNameLengthToLSPRange` joinedName

joinedNameForSectionArgs :: [SectionArg Position] -> T.Text
joinedNameForSectionArgs sectionArgs = joinedName
  where
    joinedName = T.unwords $ map getName sectionArgs

    getName :: SectionArg Position -> T.Text
    getName (SecArgName _ identifier)  = decodeUtf8 identifier
    getName (SecArgStr _ quotedString) = decodeUtf8 quotedString
    getName (SecArgOther _ string)     = decodeUtf8 string

-- | Creates a single point LSP range
--   using cabal position
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
