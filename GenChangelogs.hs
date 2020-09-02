#!/usr/bin/env cabal
{- cabal:
build-depends: base, process, html-conduit, http-conduit, xml-conduit, text, containers
-}

{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map.Lazy as M
import Network.HTTP.Simple
import System.Process
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.HTML.DOM
import Text.XML.Cursor
import Text.XML (Element(..))

main = do
  callCommand "git fetch --tags"
  tags <- filter (isPrefixOf "0.") . lines <$>
    readProcess "git" ["tag", "--list", "--sort=v:refname"] ""
  messages <- lines <$> readProcess "git" [ "log"
                                          , last tags <> "..HEAD"
                                          , "--merges"
                                          , "--reverse"
                                          , "--pretty=format:\"%s\""
                                          ] ""

  let -- try to get "1334" out of "merge PR #1334"
      prNums = map (filter isDigit) $
                map head $
                filter (not . null) $
                map (filter (isPrefixOf "#") . words) messages

  (flip mapM_) prNums $ \prNum -> do
    let url = "https://github.com/haskell/haskell-language-server/pull/" <> prNum
    body <- getResponseBody <$> httpLBS (parseRequest_ url)
    let cursor = fromDocument (parseLBS body)

        titles = (descendant >=> attributeIs "class" "js-issue-title" >=> child >=> content) cursor
        title = T.unpack $ T.strip $ head titles

        checkAuthor :: Element -> Bool
        checkAuthor e = maybe False (T.isInfixOf "author") (M.lookup "class" (elementAttributes e))
        authors = (descendant >=> checkElement checkAuthor >=> child >=> content) cursor
        author = T.unpack $ T.strip $ authors !! 4 -- second author is the pr author

    -- generate markdown
    putStrLn $ "- " <> title <> "\n([#" <> prNum <> "](" <> url <> ") by @" <> author <> ")"
