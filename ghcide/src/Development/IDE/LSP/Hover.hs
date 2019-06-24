-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

-- | Display information on hover.
module Development.IDE.LSP.Hover
    ( handle
    ) where

import Language.Haskell.LSP.Types
import Development.IDE.Types.Location

import Development.IDE.Types.Logger

import qualified Data.Text as T

import Development.IDE.Core.Rules

-- | Display information on hover.
handle
    :: Logger
    -> IdeState
    -> TextDocumentPositionParams
    -> IO (Maybe Hover)
handle loggerH compilerH (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = do
    mbResult <- case uriToFilePath' uri of
        Just (toNormalizedFilePath -> filePath) -> do
          logInfo loggerH $
              "Hover request at position " <>
              T.pack (showPosition pos) <>
              " in file: " <> T.pack (fromNormalizedFilePath filePath)
          runAction compilerH $ getAtPoint filePath pos
        Nothing       -> pure Nothing

    case mbResult of
        Just (mbRange, contents) ->
            pure $ Just $ Hover
                        (HoverContents $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator contents)
                        mbRange

        Nothing -> pure Nothing
