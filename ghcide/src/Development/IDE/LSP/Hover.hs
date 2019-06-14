-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings #-}

-- | Display information on hover.
module Development.IDE.LSP.Hover
    ( handle
    ) where

import Development.IDE.LSP.Protocol hiding (Hover)
import Language.Haskell.LSP.Types (Hover(..))

import qualified Development.IDE.Logger as Logger

import qualified Data.Text as T
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

import Development.IDE.State.Rules
import Development.IDE.Types.LSP as Compiler
import Development.IDE.Types.Diagnostics

-- | Display information on hover.
handle
    :: Logger.Handle
    -> IdeState
    -> TextDocumentPositionParams
    -> IO (Maybe Hover)
handle loggerH compilerH (TextDocumentPositionParams (TextDocumentIdentifier uri) pos) = do
    mbResult <- case uriToFilePath' uri of
        Just (toNormalizedFilePath -> filePath) -> do
          Logger.logInfo loggerH $
              "Hover request at position " <>
              renderStrict (layoutPretty defaultLayoutOptions $ prettyPosition pos) <>
              " in file: " <> T.pack (fromNormalizedFilePath filePath)
          runAction compilerH $ getAtPoint filePath pos
        Nothing       -> pure Nothing

    case mbResult of
        Just (mbRange, contents) ->
            pure $ Just $ Hover
                        (HoverContents $ MarkupContent MkMarkdown $ T.intercalate sectionSeparator $ map showHoverInformation contents)
                        mbRange

        Nothing -> pure Nothing
  where
    showHoverInformation :: Compiler.HoverText -> T.Text
    showHoverInformation = \case
        Compiler.HoverDamlCode damlCode -> T.unlines
            [ "```daml"
            , damlCode
            , "```"
            ]
        Compiler.HoverMarkdown md -> md
