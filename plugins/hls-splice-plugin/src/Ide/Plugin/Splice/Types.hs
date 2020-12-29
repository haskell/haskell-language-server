{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Splice.Types where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), withText)
import Data.Function (on)
import qualified Data.Text as T
import Development.IDE (Range, Uri)
import GHC.Generics (Generic)
import TcRnTypes (SpliceType (Typed, Untyped))

newtype SpliceType_ = SpliceType_ {getSpliceType :: SpliceType}
    deriving (Generic)

instance Eq SpliceType_ where
    (==) = eqSpliceType `on` getSpliceType

instance Ord SpliceType_ where
    compare = cmpSpliceType `on` getSpliceType

instance Show SpliceType_ where
    showsPrec _ (SpliceType_ Typed) = showString "Typed"
    showsPrec _ (SpliceType_ Untyped) = showString "Untyped"

instance ToJSON SpliceType_ where
    toJSON (SpliceType_ Typed) = "Typed"
    toJSON (SpliceType_ Untyped) = "Untyped"

instance FromJSON SpliceType_ where
    parseJSON = withText "Typed or Untyped" $ \case
        "Typed" -> pure $ SpliceType_ Typed
        "Untyepd" -> pure $ SpliceType_ Untyped
        txt -> fail $ "Typed, or Untyped is expected, but got: " <> T.unpack txt

eqSpliceType :: SpliceType -> SpliceType -> Bool
eqSpliceType Typed Typed = True
eqSpliceType Untyped Untyped = True
eqSpliceType Typed Untyped = False
eqSpliceType Untyped Typed = False

cmpSpliceType :: SpliceType -> SpliceType -> Ordering
cmpSpliceType Typed Typed = EQ
cmpSpliceType Typed Untyped = LT
cmpSpliceType Untyped Untyped = EQ
cmpSpliceType Untyped Typed = GT

-- | Parameter for the addMethods PluginCommand.
data ExpandSpliceParams = ExpandSpliceParams
    { uri :: Uri
    , range :: Range
    , spliceContext :: SpliceContext
    }
    deriving (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SpliceContext = Expr | HsDecl | Pat | HsType
    deriving (Read, Show, Eq, Ord, Generic)
    deriving anyclass (ToJSON, FromJSON)

data ExpandStyle = Inplace | Commented
    deriving (Read, Show, Eq, Ord, Generic)
