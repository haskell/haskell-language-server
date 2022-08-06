{-# LANGUAGE CPP #-}

module Ide.Plugin.Fourmolu.Shim (
  -- * FourmoluConfig
  FourmoluConfig (..),
  toConfig,
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

#if MIN_VERSION_fourmolu(0,7,0)
toConfig :: FourmoluConfig -> FourmoluConfig
toConfig = id
#else
data FourmoluConfig = FourmoluConfig
  { cfgFilePrinterOpts :: PrinterOptsPartial
  , cfgFileFixities    :: FixityMap
  }

toConfig :: PrinterOptsPartial -> FourmoluConfig
toConfig opts =
  FourmoluConfig
    { cfgFilePrinterOpts = opts
    , cfgFileFixities = mempty
    }
#endif

emptyConfig :: FourmoluConfig
emptyConfig =
  FourmoluConfig
    { cfgFilePrinterOpts = mempty
    , cfgFileFixities = mempty
    }

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
