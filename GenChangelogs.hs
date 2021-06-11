#!/usr/bin/env cabal
{- cabal:
build-depends: base, process, text, github, time
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Monad
import           Data.List
import           Data.Maybe
import qualified Data.Text                as T
import           Data.Time.Format.ISO8601
import           Data.Time.LocalTime
import           GitHub
import           System.Process

main = do
  callCommand "git fetch --tags"
  tags <- filter (isPrefixOf "1.") . lines <$>
    readProcess "git" ["tag", "--list", "--sort=v:refname"] ""

  lastDateStr <- last . lines <$> readProcess "git" ["show", "-s", "--format=%cI", "-1", last tags] ""
  lastDate <- zonedTimeToUTC <$> iso8601ParseM lastDateStr

  prs <- github' $ pullRequestsForR "haskell" "haskell-language-server" stateClosed FetchAll
  let prsAfterLastTag = either (error . show)
                        (foldMap (\pr -> [pr | inRange pr]))
                        prs
      inRange pr
        | Just mergedDate <- simplePullRequestMergedAt pr = mergedDate > lastDate
        | otherwise = False

  forM_ prsAfterLastTag $ \SimplePullRequest{..} ->
    putStrLn $ T.unpack $ "- " <> simplePullRequestTitle <>
      "\n([#" <> T.pack (show $ unIssueNumber simplePullRequestNumber) <> "](" <> getUrl simplePullRequestHtmlUrl <> "))" <>
      " by @" <> untagName (simpleUserLogin simplePullRequestUser)
