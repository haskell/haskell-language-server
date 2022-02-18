{-# LANGUAGE CPP               #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | Information and display strings for HIE's version
-- and the current project's version
module Ide.Version where

import           Data.Maybe                    (listToMaybe)
import           Data.Version
import           GitHash                       (giCommitCount, tGitInfoCwdTry)
import           Options.Applicative.Simple    (simpleVersion)
import qualified Paths_haskell_language_server as Meta
import           System.Directory
import           System.Exit
import           System.Info
import           System.Process
import           Text.ParserCombinators.ReadP

-- >>> hlsVersion
hlsVersion :: String
hlsVersion =
  let gi = $$tGitInfoCwdTry
      commitCount = case gi of
        Right gi -> show $ giCommitCount gi
        Left _   -> "UNKNOWN"
  in concat $ concat
    [ [$(simpleVersion Meta.version)]
      -- Leave out number of commits for --depth=1 clone
      -- See https://github.com/commercialhaskell/stack/issues/792
    , [" (" ++ commitCount ++ " commits)" | commitCount /= ("1"::String) &&
                                            commitCount /= ("UNKNOWN" :: String)]
    , [" ", arch]
    , [" ", hlsGhcDisplayVersion]
    ]
  where
    hlsGhcDisplayVersion = compilerName ++ "-" ++ VERSION_ghc

data ProgramsOfInterest = ProgramsOfInterest
  { cabalVersion :: Maybe Version
  , stackVersion :: Maybe Version
  , ghcVersion   :: Maybe Version
  }

showProgramVersionOfInterest :: ProgramsOfInterest -> String
showProgramVersionOfInterest ProgramsOfInterest {..} =
  unlines
    [ "cabal:\t\t" ++ showVersionWithDefault cabalVersion
    , "stack:\t\t" ++ showVersionWithDefault stackVersion
    , "ghc:\t\t" ++ showVersionWithDefault ghcVersion
    ]
  where
    showVersionWithDefault :: Maybe Version -> String
    showVersionWithDefault = maybe "Not found" showVersion

findProgramVersions :: IO ProgramsOfInterest
findProgramVersions = ProgramsOfInterest
  <$> findVersionOf "cabal"
  <*> findVersionOf "stack"
  <*> findVersionOf "ghc"

-- | Find the version of the given program.
-- Assumes the program accepts the cli argument "--numeric-version".
-- If the invocation has a non-zero exit-code, we return 'Nothing'
findVersionOf :: FilePath -> IO (Maybe Version)
findVersionOf tool =
  findExecutable tool >>= \case
    Nothing -> pure Nothing
    Just path ->
      readProcessWithExitCode path ["--numeric-version"] "" >>= \case
        (ExitSuccess, sout, _) -> pure $ consumeParser myVersionParser sout
        _                      -> pure Nothing
  where
    myVersionParser = do
      skipSpaces
      version <- parseVersion
      skipSpaces
      pure version

    consumeParser :: ReadP a -> String -> Maybe a
    consumeParser p input = listToMaybe $ map fst . filter (null . snd) $ readP_to_S p input
