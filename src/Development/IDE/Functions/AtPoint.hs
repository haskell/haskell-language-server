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
import Development.IDE.Orphans()

-- DAML compiler and infrastructure
import Development.Shake
import Development.IDE.UtilGHC
import Development.IDE.Compat
import Development.IDE.State.Shake
import Development.IDE.State.RuleTypes
import           Development.IDE.Types.Diagnostics
import           Development.IDE.Types.LSP
import Development.IDE.Types.Options
import           Development.IDE.Types.SpanInfo as SpanInfo

-- GHC API imports
import Avail
import GHC
import DynFlags
import FastString
import Name
import Outputable hiding ((<>))

import Control.Monad.Extra
import Control.Monad.Trans.Maybe
import           Data.Maybe
import           Data.List
import qualified Data.Text as T

-- | Locate the definition of the name at a given position.
gotoDefinition
  :: IdeOptions
  -> HscEnv
  -> [SpanInfo]
  -> Position
  -> Action (Maybe Location)
gotoDefinition ideOpts pkgState srcSpans pos =
  listToMaybe <$> locationsAtPoint ideOpts pkgState pos srcSpans

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
        mbDefinedAt = fmap (\name -> HoverMarkdown $ "**Defined " <> T.pack (showSDocUnsafe $ pprNameDefnLoc name) <> "**\n") mbName
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

locationsAtPoint :: IdeOptions -> HscEnv -> Position -> [SpanInfo] -> Action [Location]
locationsAtPoint IdeOptions{..} pkgState pos =
    fmap (map srcSpanToLocation) .
    mapMaybeM (getSpan . spaninfoSource) .
    spansAtPoint pos
  where getSpan :: SpanSource -> Action (Maybe SrcSpan)
        getSpan NoSource = pure Nothing
        getSpan (Span sp) = pure $ Just sp
        getSpan (Named name) = case nameSrcSpan name of
            sp@(RealSrcSpan _) -> pure $ Just sp
            UnhelpfulSpan _ -> runMaybeT $ do
                -- This case usually arises when the definition is in an external package.
                -- In this case the interface files contain garbage source spans
                -- so we instead read the .hie files to get useful source spans.
                let mod = nameModule name
                let unitId = moduleUnitId mod
                pkgConfig <- MaybeT $ pure $ lookupPackageConfig unitId pkgState
                hiePath <- MaybeT $ liftIO $ optLocateHieFile optPkgLocationOpts pkgConfig mod
                hieFile <- MaybeT $ use (GetHieFile hiePath) ""
                avail <- MaybeT $ pure $ listToMaybe (filterAvails (eqName name) $ hie_exports hieFile)
                srcPath <- MaybeT $ liftIO $ optLocateSrcFile optPkgLocationOpts pkgConfig mod
                -- The location will point to the source file used during compilation.
                -- This file might no longer exists and even if it does the path will be relative
                -- to the compilation directory which we don’t know.
                let span = setFileName srcPath $ nameSrcSpan $ availName avail
                pure span
        -- We ignore uniques and source spans and only compare the name and the module.
        eqName :: Name -> Name -> Bool
        eqName n n' = nameOccName n == nameOccName n' && nameModule n == nameModule n'
        setFileName f (RealSrcSpan span) = RealSrcSpan (span { srcSpanFile = mkFastString f })
        setFileName _ span@(UnhelpfulSpan _) = span

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
