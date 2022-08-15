{-# LANGUAGE CPP #-}

module Ide.Plugin.Fourmolu.Shim (
  -- * FourmoluConfig
  cfgFilePrinterOpts,
  cfgFileFixities,
  emptyConfig,

  -- * FixityMap
  addFixityOverrides,

  -- * ConfigParseError
  showParseError,
) where

import           Ormolu.Config

#if MIN_VERSION_fourmolu(0,7,0)
import           Ormolu.Fixity
#endif

{-- Backport FourmoluConfig --}

#if !MIN_VERSION_fourmolu(0,7,0)
type FourmoluConfig = PrinterOptsPartial

cfgFilePrinterOpts :: FourmoluConfig -> PrinterOptsPartial
cfgFilePrinterOpts = id

cfgFileFixities :: FourmoluConfig -> FixityMap
cfgFileFixities _ = mempty
#endif

#if MIN_VERSION_fourmolu(0,8,1)
-- emptyConfig now provided
#elif MIN_VERSION_fourmolu(0,7,0)
emptyConfig :: FourmoluConfig
emptyConfig =
  FourmoluConfig
    { cfgFilePrinterOpts = mempty
    , cfgFileFixities = mempty
    }
#else
emptyConfig :: FourmoluConfig
emptyConfig = mempty
#endif

{-- Backport FixityMap --}

#if MIN_VERSION_fourmolu(0,7,0)
addFixityOverrides :: FixityMap -> Config region -> Config region
addFixityOverrides fixities cfg = cfg{cfgFixityOverrides = fixities}
#else
type FixityMap = ()

addFixityOverrides :: FixityMap -> Config region -> Config region
addFixityOverrides _ = id
#endif

{-- Backport ConfigParseError --}

#if MIN_VERSION_fourmolu(0,7,0)
showParseError :: Show parseException => parseException -> String
showParseError = show
#else
showParseError :: (pos, String) -> String
showParseError = snd
#endif
