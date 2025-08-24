{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ViewPatterns          #-}

module Ide.Plugin.Cabal.FoldingRange where

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
import           Ide.Plugin.Cabal.Outline
import           Ide.Types                               (PluginMethodHandler)
import           Language.LSP.Protocol.Message           (Method (..))
import           Language.LSP.Protocol.Types             (FoldingRange (..))
import qualified Language.LSP.Protocol.Types             as LSP

moduleOutline :: PluginMethodHandler IdeState Method_TextDocumentFoldingRange
moduleOutline ideState _ LSP.FoldingRangeParams {_textDocument = LSP.TextDocumentIdentifier uri} =
  case LSP.uriToFilePath uri of
    Just (toNormalizedFilePath' -> fp) -> do
      mFields <- liftIO $ runIdeAction "cabal-plugin.fields" (shakeExtras ideState) (useWithStaleFast ParseCabalFields fp)
      case fmap fst mFields of
        Just fieldPositions -> pure allRanges
          where
            allRanges = mapMaybe foldingRangeForField fieldPositions
        Nothing -> pure []
    Nothing -> pure []

-- | Creates a @FoldingRange@ object for the
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
foldingRangeForField :: Field Position -> Maybe FoldingRange
foldingRangeForField (Field (Name pos fieldName) _) =
  Just
    (defFoldingRange lspPos)
      { _collapsedText = Just (decodeUtf8 fieldName)
      }
  where
    lspPos@(LSP.Position startLine startChar) = cabalPositionToLSPPosition pos

foldingRangeForField (Section (Name pos fieldName) sectionArgs fields) =
  Just
    (defFoldingRange lspPos)
      { _endLine = endLine,
        _endCharacter = Just endChar,
        _collapsedText = Just joinedName
      }
  where
    lspPos = cabalPositionToLSPPosition pos
    LSP.Position startLine startChar = lspPos
    joinedName = decodeUtf8 fieldName <> " " <> onelineSectionArgs sectionArgs
    LSP.Position endLine endChar = fromMaybe lspPos (lastFieldPosition fields)

lastFieldPosition :: [Field Position] -> Maybe LSP.Position
lastFieldPosition [] = Nothing
lastFieldPosition xs =
    case last xs of
        Field (Name pos _) _     -> Just (cabalPositionToLSPPosition pos)
        Section (Name pos _) _ _ -> Just (cabalPositionToLSPPosition pos)

-- | Creates a single point LSP range
--   using cabal position
-- cabalPositionToLSPRange :: Position -> LSP.Range
-- cabalPositionToLSPRange pos = LSP.Range lspPos lspPos
--   where
--     lspPos = cabalPositionToLSPPosition pos

defFoldingRange :: LSP.Position -> FoldingRange
defFoldingRange (LSP.Position line char) = FoldingRange
  { _startLine = line
  , _startCharacter = Just char
  , _endLine = line
  , _endCharacter = Just char
  , _kind = Just LSP.FoldingRangeKind_Region
  , _collapsedText = Nothing
  }
