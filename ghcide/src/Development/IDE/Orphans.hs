-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances for GHC.
--   Note that the 'NFData' instances may not be law abiding.
module Development.IDE.Orphans() where

import           GHC                         hiding (convertLit)
import           GhcPlugins                  as GHC hiding (fst3, (<>))
import qualified StringBuffer as SB
import Control.DeepSeq
import Development.IDE.UtilGHC


-- Orphan instances for types from the GHC API.
instance Show CoreModule where show = prettyPrint
instance NFData CoreModule where rnf = rwhnf


instance Show InstalledUnitId where
    show = installedUnitIdString

instance NFData InstalledUnitId where rnf = rwhnf

instance NFData SB.StringBuffer where rnf = rwhnf

instance Show Module where
    show = moduleNameString . moduleName

instance Show RdrName where show = prettyPrint
instance Show ComponentId where show = prettyPrint
instance Show SourcePackageId where show = prettyPrint
instance Show ModuleName where show = prettyPrint
instance Show (GenLocated SrcSpan ModuleName) where show = prettyPrint
instance Show PackageName where show = prettyPrint
instance Show PackageState where show _ = "PackageState"
instance Show Name where show = prettyPrint


-- Things which are defined in this module, but still orphan since I need
-- the definitions in this module

deriving instance Show PackageDynFlags
instance NFData PackageDynFlags where
    rnf (PackageDynFlags db state insts) = db `seq` state `seq` rnf insts
