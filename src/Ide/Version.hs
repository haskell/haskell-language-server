{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Information and display strings for HIE's version
-- and the current project's version
module Ide.Version where

import           Development.GitRev              (gitCommitCount)
import           Options.Applicative.Simple      (simpleVersion)
import qualified Paths_haskell_language_server as Meta
import           System.Info

hlsVersion :: String
hlsVersion =
  let commitCount = $gitCommitCount
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
