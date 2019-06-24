-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}

-- | Attempt at hiding the GHC version differences we can.
module Development.IDE.GHC.Compat(
    HieFileResult(..),
    HieFile(..),
    mkHieFile,
    writeHieFile,
    readHieFile
    ) where

#ifndef GHC_STABLE
import HieAst
import HieBin
import HieTypes
#else
import GHC
import GhcPlugins
import NameCache
import Avail
import TcRnTypes


mkHieFile :: ModSummary -> TcGblEnv -> RenamedSource -> Hsc HieFile
mkHieFile _ _ _ = return (HieFile () [])

writeHieFile :: FilePath -> HieFile -> IO ()
writeHieFile _ _ = return ()

readHieFile :: NameCache -> FilePath -> IO (HieFileResult, ())
readHieFile _ _ = return (HieFileResult (HieFile () []), ())

data HieFile = HieFile {hie_module :: (), hie_exports :: [AvailInfo]}
data HieFileResult = HieFileResult { hie_file_result :: HieFile }
#endif
