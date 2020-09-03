module Development.IDE.Types.Exports
(
    IdentInfo(..),
    ExportsMap,
    createExportsMap,
) where

import Avail (AvailInfo(..))
import Control.DeepSeq (NFData)
import Data.Text (pack, Text)
import Development.IDE.GHC.Compat
import Development.IDE.GHC.Util
import Data.HashMap.Strict (HashMap)
import GHC.Generics (Generic)
import Name
import FieldLabel (flSelector)
import qualified Data.HashMap.Strict as Map
import GhcPlugins (IfaceExport)

type ExportsMap = HashMap IdentifierText [(IdentInfo,ModuleNameText)]

type IdentifierText = Text
type ModuleNameText = Text

data IdentInfo = IdentInfo
    { name :: !Text
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

createExportsMap :: [ModIface] -> ExportsMap
createExportsMap = Map.fromListWith (++) . concatMap doOne
  where
    doOne mi = concatMap (unpackAvail mn) (mi_exports mi)
      where
        mn = moduleName $ mi_module mi

unpackAvail :: ModuleName -> IfaceExport -> [(Text, [(IdentInfo, Text)])]
unpackAvail mod =
  map (\id@IdentInfo {..} -> (name, [(id, pack $ moduleNameString mod)]))
    . mkIdentInfos
