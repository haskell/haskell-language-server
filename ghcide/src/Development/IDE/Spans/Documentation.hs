-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

module Development.IDE.Spans.Documentation (
    getDocumentation
  , getDocumentationTryGhc
  ) where

import           Control.Monad
import           Data.List.Extra
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Text as T
import           Development.IDE.GHC.Error
import           Development.IDE.Spans.Calculate
import           FastString
import           GHC
import SrcLoc

#if MIN_GHC_API_VERSION(8,6,0)
import           Data.Char (isSpace)
import           Development.IDE.GHC.Util
import qualified Documentation.Haddock.Parser as H
import qualified Documentation.Haddock.Types as H
#endif

getDocumentationTryGhc
  :: HscEnv
  -> [TypecheckedModule]
  -> Name
  -> IO [T.Text]
#if MIN_GHC_API_VERSION(8,6,0)
getDocumentationTryGhc packageState tcs name = do
  res <- runGhcEnv packageState $ catchSrcErrors "docs" $ getDocs name
  case res of
    Right (Right (Just docs, _)) -> return [T.pack $ haddockToMarkdown $ H.toRegular $ H._doc $ H.parseParas Nothing $ unpackHDS docs]
    _ -> return $ getDocumentation tcs name
#else
getDocumentationTryGhc _packageState tcs name = do
  return $ getDocumentation tcs name
#endif

getDocumentation
 :: [TypecheckedModule] -- ^ All of the possible modules it could be defined in.
 ->  Name -- ^ The name you want documentation for.
 -> [T.Text]
-- This finds any documentation between the name you want
-- documentation for and the one before it. This is only an
-- approximately correct algorithm and there are easily constructed
-- cases where it will be wrong (if so then usually slightly but there
-- may be edge cases where it is very wrong).
-- TODO : Build a version of GHC exactprint to extract this information
-- more accurately.
getDocumentation tcs targetName = fromMaybe [] $ do
  -- Find the module the target is defined in.
  targetNameSpan <- realSpan $ nameSrcSpan targetName
  tc <-
    listToMaybe
      $ filter ((==) (Just $ srcSpanFile targetNameSpan) . annotationFileName)
      $ reverse tcs -- TODO : Is reversing the list here really neccessary?
  -- Names bound by the module (we want to exclude non-"top-level"
  -- bindings but unfortunately we get all here).
  let bs = mapMaybe name_of_bind
               (listifyAllSpans (tm_typechecked_source tc) :: [LHsBind GhcTc])
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
    name_of_bind :: LHsBind GhcTc -> Maybe Name
    name_of_bind (L _ FunBind {fun_id}) = Just (getName (unLoc fun_id))
    name_of_bind _ = Nothing
    -- Get source spans from names, discard unhelpful spans, remove
    -- duplicates and sort.
    sortedNameSpans :: [Name] -> [RealSrcSpan]
    sortedNameSpans ls = nubSort (mapMaybe (realSpan . nameSrcSpan) ls)
    isBetween target before after = before <= target && target <= after
    ann = snd . pm_annotations . tm_parsed_module
    annotationFileName :: TypecheckedModule -> Maybe FastString
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

#if MIN_GHC_API_VERSION(8,6,0)
-- Simple (and a bit hacky) conversion from Haddock markup to Markdown
haddockToMarkdown
  :: H.DocH String String -> String

haddockToMarkdown H.DocEmpty
  = ""
haddockToMarkdown (H.DocAppend d1 d2)
  = haddockToMarkdown d1 <> haddockToMarkdown d2
haddockToMarkdown (H.DocString s)
  = s
haddockToMarkdown (H.DocParagraph p)
  = "\n\n" ++ haddockToMarkdown p
haddockToMarkdown (H.DocIdentifier i)
  = "`" ++ i ++ "`"
haddockToMarkdown (H.DocIdentifierUnchecked i)
  = "`" ++ i ++ "`"
haddockToMarkdown (H.DocModule i)
  = "`" ++ i ++ "`"
haddockToMarkdown (H.DocWarning w)
  = haddockToMarkdown w
haddockToMarkdown (H.DocEmphasis d)
  = "*" ++ haddockToMarkdown d ++ "*"
haddockToMarkdown (H.DocBold d)
  = "**" ++ haddockToMarkdown d ++ "**"
haddockToMarkdown (H.DocMonospaced d)
  = "`" ++ escapeBackticks (haddockToMarkdown d) ++ "`"
  where
    escapeBackticks "" = ""
    escapeBackticks ('`':ss) = '\\':'`':escapeBackticks ss
    escapeBackticks (s  :ss) = s:escapeBackticks ss
haddockToMarkdown (H.DocCodeBlock d)
  = "\n```haskell\n" ++ haddockToMarkdown d ++ "\n```\n"
haddockToMarkdown (H.DocExamples es)
  = "\n```haskell\n" ++ unlines (map exampleToMarkdown es) ++ "\n```\n"
  where
    exampleToMarkdown (H.Example expr result)
      = ">>> " ++ expr ++ "\n" ++ unlines result
haddockToMarkdown (H.DocHyperlink (H.Hyperlink url Nothing))
  = "<" ++ url ++ ">"
#if MIN_VERSION_haddock_library(1,8,0)
haddockToMarkdown (H.DocHyperlink (H.Hyperlink url (Just label)))
  = "[" ++ haddockToMarkdown label ++ "](" ++ url ++ ")"
#else
haddockToMarkdown (H.DocHyperlink (H.Hyperlink url (Just label)))
  = "[" ++ label ++ "](" ++ url ++ ")"
#endif
haddockToMarkdown (H.DocPic (H.Picture url Nothing))
  = "![](" ++ url ++ ")"
haddockToMarkdown (H.DocPic (H.Picture url (Just label)))
  = "![" ++ label ++ "](" ++ url ++ ")"
haddockToMarkdown (H.DocAName aname)
  = "[" ++ aname ++ "]:"
haddockToMarkdown (H.DocHeader (H.Header level title))
  = replicate level '#' ++ " " ++ haddockToMarkdown title

haddockToMarkdown (H.DocUnorderedList things)
  = '\n' : (unlines $ map (\thing -> "+ " ++ dropWhile isSpace (haddockToMarkdown thing)) things)
haddockToMarkdown (H.DocOrderedList things)
  = '\n' : (unlines $ map (\thing -> "1. " ++ dropWhile isSpace (haddockToMarkdown thing)) things)
haddockToMarkdown (H.DocDefList things)
  = '\n' : (unlines $ map (\(term, defn) -> "+ **" ++ haddockToMarkdown term ++ "**: " ++ haddockToMarkdown defn) things)

-- we cannot render math by default
haddockToMarkdown (H.DocMathInline _)
  = "*cannot render inline math formula*"
haddockToMarkdown (H.DocMathDisplay _)
  = "\n\n*cannot render display math formula*\n\n"

-- TODO: render tables
haddockToMarkdown (H.DocTable _t)
  = "\n\n*tables are not yet supported*\n\n"

-- things I don't really know how to handle
haddockToMarkdown (H.DocProperty _)
  = ""  -- don't really know what to do
#endif