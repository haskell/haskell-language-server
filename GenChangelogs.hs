#!/usr/bin/env cabal
{- cabal:
build-depends: base, bytestring, process, text, github, time >= 1.9
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

import           Control.Applicative      ((<|>))
import           Control.Monad
import qualified Data.ByteString.Char8    as BS
import           Data.List
import           Data.Maybe
import qualified Data.Text                as T
import           Data.Time.Clock          (UTCTime)
import           Data.Time.Format.ISO8601
import           Data.Time.LocalTime      (zonedTimeToUTC)
import           GitHub
import           System.Environment
import           System.Process

-- %cI yields either a Z or a numeric offset, depending on the committer.
parseUTC :: String -> Maybe UTCTime
parseUTC s = iso8601ParseM s <|> (zonedTimeToUTC <$> iso8601ParseM s)

main = do
  args <- getArgs
  let (githubReq,tag) = case args of
        token:tag:_ -> (github (OAuth $ BS.pack token), tag)
  prs <- githubReq $ pullRequestsForR "haskell" "haskell-language-server" stateClosed FetchAll
  lastDateStr <- last . lines <$> readProcess "git" ["show", "-s", "--format=%cI", "-1", tag] ""
  lastDate <- maybe (fail $ "no parse of " <> show lastDateStr) pure (parseUTC lastDateStr)

  let prsAfterLastTag = either (error . show)
                        (foldMap (\pr -> [pr | inRange pr]))
                        prs
      inRange pr
        | Just mergedDate <- simplePullRequestMergedAt pr = mergedDate > lastDate
        | otherwise = False

  forM_ prsAfterLastTag $ \SimplePullRequest{..} ->
    putStrLn $ T.unpack $ "- " <> simplePullRequestTitle <>
      "\n  ([#" <> T.pack (show $ unIssueNumber simplePullRequestNumber) <> "](" <> getUrl simplePullRequestHtmlUrl <> "))" <>
      " by @" <> untagName (simpleUserLogin simplePullRequestUser)
