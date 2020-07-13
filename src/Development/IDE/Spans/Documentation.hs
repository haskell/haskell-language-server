{-# LANGUAGE RankNTypes #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

module Development.IDE.Spans.Documentation (
    getDocumentation
  , getDocumentationTryGhc
  , getDocumentationsTryGhc
  ) where

import           Control.Monad
import           Data.List.Extra
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
#if MIN_GHC_API_VERSION(8,6,0)
import           Development.IDE.Core.Compile
#endif
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error
import           Development.IDE.Spans.Common
import           FastString
import           SrcLoc (RealLocated)

getDocumentationTryGhc :: GhcMonad m => Module -> [ParsedModule] -> Name -> m SpanDoc
getDocumentationTryGhc mod deps n = head <$> getDocumentationsTryGhc mod deps [n]

getDocumentationsTryGhc :: GhcMonad m => Module -> [ParsedModule] -> [Name] -> m [SpanDoc]

-- Interfaces are only generated for GHC >= 8.6.
-- In older versions, interface files do not embed Haddocks anyway
#if MIN_GHC_API_VERSION(8,6,0)
getDocumentationsTryGhc mod sources names = do
  res <- catchSrcErrors "docs" $ getDocsBatch mod names
  case res of
      Left _ -> return $ map (SpanDocText . getDocumentation sources) names
      Right res -> return $ zipWith unwrap res names
  where
    unwrap (Right (Just docs, _))  _= SpanDocString docs
    unwrap _ n = SpanDocText $ getDocumentation sources n
#else
getDocumentationsTryGhc _ sources names = do
  return $ map (SpanDocText . getDocumentation sources) names
#endif


getDocumentation
 :: HasSrcSpan name
 => [ParsedModule] -- ^ All of the possible modules it could be defined in.
 ->  name -- ^ The name you want documentation for.
 -> [T.Text]
-- This finds any documentation between the name you want
-- documentation for and the one before it. This is only an
-- approximately correct algorithm and there are easily constructed
-- cases where it will be wrong (if so then usually slightly but there
-- may be edge cases where it is very wrong).
-- TODO : Build a version of GHC exactprint to extract this information
-- more accurately.
getDocumentation sources targetName = fromMaybe [] $ do
  -- Find the module the target is defined in.
  targetNameSpan <- realSpan $ getLoc targetName
  tc <-
    find ((==) (Just $ srcSpanFile targetNameSpan) . annotationFileName)
      $ reverse sources -- TODO : Is reversing the list here really neccessary?

  -- Top level names bound by the module
  let bs = [ n | let L _ HsModule{hsmodDecls} = pm_parsed_source tc
           , L _ (ValD hsbind) <- hsmodDecls
           , Just n <- [name_of_bind hsbind]
           ]
  -- Sort the names' source spans.
  let sortedSpans = sortedNameSpans bs
  -- Now go ahead and extract the docs.
  let docs = ann tc
  nameInd <- elemIndex targetNameSpan sortedSpans
  let prevNameSpan =
        if nameInd >= 1
        then sortedSpans !! (nameInd - 1)
        else zeroSpan $ srcSpanFile targetNameSpan
  -- Annoyingly "-- |" documentation isn't annotated with a location,
  -- so you have to pull it out from the elements.
  pure
      $ docHeaders
      $ filter (\(L target _) -> isBetween target prevNameSpan targetNameSpan)
      $ mapMaybe (\(L l v) -> L <$> realSpan l <*> pure v)
      $ join
      $ M.elems
      docs
  where
    -- Get the name bound by a binding. We only concern ourselves with
    -- @FunBind@ (which covers functions and variables).
    name_of_bind :: HsBind GhcPs -> Maybe (Located RdrName)
    name_of_bind FunBind {fun_id} = Just fun_id
    name_of_bind _ = Nothing
    -- Get source spans from names, discard unhelpful spans, remove
    -- duplicates and sort.
    sortedNameSpans :: [Located RdrName] -> [RealSrcSpan]
    sortedNameSpans ls = nubSort (mapMaybe (realSpan . getLoc) ls)
    isBetween target before after = before <= target && target <= after
    ann = snd . pm_annotations
    annotationFileName :: ParsedModule -> Maybe FastString
    annotationFileName = fmap srcSpanFile . listToMaybe . realSpans . ann
    realSpans :: M.Map SrcSpan [Located a] -> [RealSrcSpan]
    realSpans =
        mapMaybe (realSpan . getLoc)
      . join
      . M.elems

-- | Shows this part of the documentation
docHeaders :: [RealLocated AnnotationComment]
           -> [T.Text]
docHeaders = mapMaybe (\(L _ x) -> wrk x)
  where
  wrk = \case
    -- When `Opt_Haddock` is enabled.
    AnnDocCommentNext s -> Just $ T.pack s
    -- When `Opt_KeepRawTokenStream` enabled.
    AnnLineComment s  -> if "-- |" `isPrefixOf` s
                            then Just $ T.pack s
                            else Nothing
    _ -> Nothing
