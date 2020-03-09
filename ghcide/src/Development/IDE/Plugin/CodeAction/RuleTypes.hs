{-# LANGUAGE TypeFamilies #-}
module Development.IDE.Plugin.CodeAction.RuleTypes
    (PackageExports(..), PackageExportsMap
    ,IdentInfo(..)
    ,mkIdentInfos
    ) where

import Avail (AvailInfo(..))
import Data.Hashable (Hashable)
import Control.DeepSeq (NFData)
import Data.Binary (Binary)
import Data.Text (pack, Text)
import Development.IDE.GHC.Util
import Development.Shake (RuleResult)
import Data.HashMap.Strict (HashMap)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Name
import FieldLabel (flSelector)

type Identifier = Text
type ModuleName = Text

data IdentInfo = IdentInfo
    { name :: !Identifier
    , rendered :: Text
    , parent :: !(Maybe Text)
    , isDatacon :: !Bool
    }
    deriving (Eq, Generic, Show)

instance NFData IdentInfo

mkIdentInfos :: AvailInfo -> [IdentInfo]
mkIdentInfos (Avail n) =
    [IdentInfo (pack (prettyPrint n)) (pack (printName n)) Nothing (isDataConName n)]
mkIdentInfos (AvailTC parent (n:nn) flds)
    -- Following the GHC convention that parent == n if parent is exported
    | n == parent
    = [ IdentInfo (pack (prettyPrint n)) (pack (printName n)) (Just $! parentP) True
        | n <- nn ++ map flSelector flds
      ] ++
      [ IdentInfo (pack (prettyPrint n)) (pack (printName n)) Nothing False]
    where
        parentP = pack $ prettyPrint parent

mkIdentInfos (AvailTC _ nn flds)
    = [ IdentInfo (pack (prettyPrint n)) (pack (printName n)) Nothing True
        | n <- nn ++ map flSelector flds
      ]

-- Rule type for caching Package Exports
type instance RuleResult PackageExports = PackageExportsMap
type PackageExportsMap = HashMap Identifier [(IdentInfo,ModuleName)]

newtype PackageExports = PackageExports HscEnvEq
    deriving (Eq, Show, Typeable, Generic)

instance Hashable PackageExports
instance NFData   PackageExports
instance Binary   PackageExports
