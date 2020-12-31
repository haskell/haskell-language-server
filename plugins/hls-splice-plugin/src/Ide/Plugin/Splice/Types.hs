{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Ide.Plugin.Splice.Types where

import Data.Aeson (FromJSON, ToJSON)
import Development.IDE (Range, Uri)
import GHC.Generics (Generic)
import Development.IDE.GHC.Compat (RealSrcSpan)

-- | Parameter for the addMethods PluginCommand.
data ExpandSpliceParams = ExpandSpliceParams
    { uri :: Uri
    , spliceSpan :: RealSrcSpan
    , spliceContext :: SpliceContext
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- FIXME: HsDecl needs different treatment of splicing.
data SpliceContext = Expr {- HsDecl | -} | Pat | HsType
    deriving (Read, Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ExpandStyle = Inplace | Commented
    deriving (Read, Show, Eq, Ord, Generic)
