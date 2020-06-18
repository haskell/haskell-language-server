{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Information and display strings for HIE's version
-- and the current project's version
module Ide.Version where

import           Data.Maybe
import           Development.GitRev              (gitCommitCount)
import           Distribution.System             (buildArch)
import           Distribution.Text               (display)
import           Options.Applicative.Simple      (simpleVersion)
import           Ide.Cradle                      (execProjectGhc)
import qualified HIE.Bios.Types as Bios
import qualified Ide.Cradle     as Bios
import qualified Paths_haskell_language_server as Meta
import           System.Directory
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
    , [" ", display buildArch]
    , [" ", hlsGhcDisplayVersion]
    ]

-- ---------------------------------------------------------------------

hlsGhcDisplayVersion :: String
hlsGhcDisplayVersion = compilerName ++ "-" ++ VERSION_ghc

hlsGhcVersion :: String
hlsGhcVersion = VERSION_ghc
