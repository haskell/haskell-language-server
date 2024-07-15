{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.Cabal.Outline where

import           Control.Monad.IO.Class
import           Data.Maybe
import qualified Data.Text                               as T
import           Data.Text.Encoding                      (decodeUtf8)
import           Development.IDE.Core.Rules
import           Development.IDE.Core.Shake              (IdeState (shakeExtras),
                                                          runIdeAction,
                                                          useWithStaleFast)
import           Development.IDE.Types.Location          (toNormalizedFilePath')
import           Distribution.Fields.Field               (Field (Field, Section),
                                                          Name (Name))
import           Distribution.Parsec.Position            (Position)
import           Ide.Plugin.Cabal.Completion.CabalFields (onelineSectionArgs)
import           Ide.Plugin.Cabal.Completion.Types       (ParseCabalFields (..),
                                                          cabalPositionToLSPPosition)
import           Ide.Plugin.Cabal.Orphans                ()
import           Ide.Types                               (PluginMethodHandler)
import           Language.LSP.Protocol.Message           (Method (..))
import           Language.LSP.Protocol.Types             (DocumentSymbol (..))
import qualified Language.LSP.Protocol.Types             as LSP


moduleOutline :: PluginMethodHandler IdeState Method_TextDocumentDocumentSymbol
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

-- | Creates a @DocumentSymbol@ object for the
--   cabal AST, without displaying @fieldLines@ and
--   displaying @Section Name@ and @SectionArgs@ in one line.
--
--   @fieldLines@ are leaves of a cabal AST, so they are omitted
--   in the outline. Sections have to be displayed in one line, because
--   the AST representation looks unnatural. See examples:
--
-- *  part of a cabal file:
--
-- >   if impl(ghc >= 9.8)
-- >      ghc-options: -Wall
--
-- * AST representation:
--
-- >   if
-- >      impl
-- >      (
-- >      ghc >= 9.8
-- >      )
-- >
-- >      ghc-options:
-- >        -Wall
--
-- * resulting @DocumentSymbol@:
--
-- >   if impl(ghc >= 9.8)
-- >      ghc-options:
-- >
documentSymbolForField :: Field Position -> Maybe DocumentSymbol
documentSymbolForField (Field (Name pos fieldName) _) =
  Just
    (defDocumentSymbol range)
      { _name = decodeUtf8 fieldName,
        _kind = LSP.SymbolKind_Field,
        _children = Nothing
      }
  where
    range = cabalPositionToLSPRange pos `addNameLengthToLSPRange` decodeUtf8 fieldName
documentSymbolForField (Section (Name pos fieldName) sectionArgs fields) =
  Just
    (defDocumentSymbol range)
      { _name = joinedName,
        _kind = LSP.SymbolKind_Object,
        _children =
          Just
            (mapMaybe documentSymbolForField fields)
      }
  where
    joinedName = decodeUtf8 fieldName <> " " <> onelineSectionArgs sectionArgs
    range = cabalPositionToLSPRange pos `addNameLengthToLSPRange` joinedName

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

defDocumentSymbol :: LSP.Range -> DocumentSymbol
defDocumentSymbol range = DocumentSymbol
  { _detail = Nothing
  , _deprecated = Nothing
  , _name = ""
  , _kind = LSP.SymbolKind_File
  , _range = range
  , _selectionRange = range
  , _children = Nothing
  , _tags = Nothing
  }