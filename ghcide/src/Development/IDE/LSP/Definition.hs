-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

-- | Go to the definition of a variable.
module Development.IDE.LSP.Definition
    ( handle
    ) where

import           Development.IDE.LSP.Protocol
import Development.IDE.Types.Location

import Development.IDE.Types.Logger
import Development.IDE.Core.Rules

import qualified Data.Text as T

-- | Go to the definition of a variable.
handle
    :: Logger
    -> IdeState
    -> TextDocumentPositionParams
    -> IO LocationResponseParams
handle logger compilerH (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = do


    mbResult <- case uriToFilePath' uri of
        Just (toNormalizedFilePath -> filePath) -> do
          logInfo logger $
            "Definition request at position " <>
            T.pack (showPosition pos) <>
            " in file: " <> T.pack (fromNormalizedFilePath filePath)
          runAction compilerH (getDefinition filePath pos)
        Nothing       -> pure Nothing

    case mbResult of
        Nothing ->
            pure $ MultiLoc []

        Just loc ->
            pure $ SingleLoc loc
