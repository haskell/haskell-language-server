-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE OverloadedStrings   #-}
-- | Gives information about symbols at a given point in DAML files.
-- These are all pure functions that should execute quickly.
module Development.IDE.Functions.AtPoint (
    atPoint
  , gotoDefinition
  ) where

import           Development.IDE.Functions.Documentation
import           Development.IDE.Functions.GHCError

-- DAML compiler and infrastructure
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.LSP
import           Development.IDE.Types.SpanInfo as SpanInfo

-- GHC API imports
import "ghc-lib" GHC
import "ghc-lib-parser" DynFlags
import "ghc-lib-parser" Outputable hiding ((<>))
import "ghc-lib-parser" Name

import           Data.Maybe
import           Data.List
import qualified Data.Text as T

-- | Locate the definition of the name at a given position.
gotoDefinition
  :: [SpanInfo]
  -> Position
  -> Maybe Location
gotoDefinition srcSpans pos =
  listToMaybe $ locationsAtPoint pos srcSpans

-- | Synopsis for the name at a given position.
atPoint
  :: [TypecheckedModule]
  -> [SpanInfo]
  -> Position
  -> Maybe (Maybe Range, [HoverText])
atPoint tcs srcSpans pos = do
    SpanInfo{..} <- listToMaybe $ orderSpans $ spansAtPoint pos srcSpans
    ty <- spaninfoType
    let mbName  = getNameM spaninfoSource
        mbDefinedAt = HoverHeading . ("Defined " <>) . T.pack . showSDocUnsafe . pprNameDefnLoc <$> mbName
        mbDocs  = fmap (\name -> getDocumentation name tcs) mbName
        docInfo = maybe [] (map HoverMarkdown . docHeaders) mbDocs
        range = Range
                  (Position spaninfoStartLine spaninfoStartCol)
                  (Position spaninfoEndLine spaninfoEndCol)
        typeSig = HoverDamlCode $ case mbName of
          Nothing -> ": " <> showName ty
          Just name ->
            let modulePrefix = maybe "" (<> ".") (getModuleNameAsText name)
            in  modulePrefix <> showName name <> "\n  : " <> showName ty
        hoverInfo = docInfo <> [typeSig] <> maybeToList mbDefinedAt
    return (Just range, hoverInfo)
  where
    -- NOTE(RJR): This is a bit hacky.
    -- We don't want to show the user type signatures generated from Eq and Show
    -- instances, as they do not appear in the source program.
    -- However the user could have written an `==` or `show` function directly,
    -- in which case we still want to show information for that.
    -- Hence we just move such information later in the list of spans.
    orderSpans :: [SpanInfo] -> [SpanInfo]
    orderSpans = uncurry (++) . partition (not . isTypeclassDeclSpan)
    isTypeclassDeclSpan :: SpanInfo -> Bool
    isTypeclassDeclSpan spanInfo =
      case getNameM (spaninfoSource spanInfo) of
        Just name -> any (`isInfixOf` show name) ["==", "showsPrec"]
        Nothing -> False

locationsAtPoint :: Position -> [SpanInfo] -> [Location]
locationsAtPoint pos = map srcSpanToLocation
                     . mapMaybe (SpanInfo.getSrcSpan . spaninfoSource)
                     . spansAtPoint pos

spansAtPoint :: Position -> [SpanInfo] -> [SpanInfo]
spansAtPoint pos = filter atp where
  line = _line pos + 1
  cha = _character pos + 1
  atp SpanInfo{..} =    spaninfoStartLine <= line
                     && spaninfoEndLine >= line
                     && spaninfoStartCol <= cha
                     && spaninfoEndCol >= cha

showName :: Outputable a => a -> T.Text
showName = T.pack . prettyprint
  where
    prettyprint x = renderWithStyle unsafeGlobalDynFlags (ppr x) style
    style = mkUserStyle unsafeGlobalDynFlags neverQualify AllTheWay

getModuleNameAsText :: Name -> Maybe T.Text
getModuleNameAsText n = do
  m <- nameModule_maybe n
  return . T.pack . moduleNameString $ moduleName m
