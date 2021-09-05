{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
module Development.IDE.Types.Exports
(
    IdentInfo(..),
    ExportsMap(..),
    createExportsMap,
    createExportsMapMg,
    createExportsMapTc
,createExportsMapHieDb,size) where

import           Avail                       (AvailInfo (..), availName)
import           Control.DeepSeq             (NFData (..))
import           Control.Monad
import           Data.Bifunctor              (Bifunctor (second))
import           Data.HashMap.Strict         (HashMap, elems)
import qualified Data.HashMap.Strict         as Map
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as Set
import           Data.Hashable               (Hashable)
import           Data.List                   (isSuffixOf)
import           Data.Text                   (Text, pack)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Orphans ()
import           Development.IDE.GHC.Util
import           FieldLabel                  (flSelector)
import           GHC.Generics                (Generic)
import           GhcPlugins                  (IfaceExport, ModGuts (..))
import           HieDb
import           Name
import           TcRnTypes                   (TcGblEnv (..))


data ExportsMap = ExportsMap
    {getExportsMap :: HashMap IdentifierText (HashSet IdentInfo)
    , getModuleExportsMap :: Map.HashMap ModuleNameText [Text]
    }
    deriving (Show)

size :: ExportsMap -> Int
size = sum . map length . elems . getExportsMap

instance Semigroup ExportsMap where
  ExportsMap a b <> ExportsMap c d = ExportsMap (Map.unionWith (<>) a c) (Map.unionWith (<>) b d)

instance Monoid ExportsMap where
  mempty = ExportsMap Map.empty Map.empty

type IdentifierText = Text

data IdentInfo = IdentInfo
    { name           :: !OccName
    , rendered       :: Text
    , parent         :: !(Maybe Text)
    , isDatacon      :: !Bool
    , moduleNameText :: !Text
    }
    deriving (Generic, Show)
    deriving anyclass Hashable

instance Eq IdentInfo where
    a == b = name a == name b
          && parent a == parent b
          && isDatacon a == isDatacon b
          && moduleNameText a == moduleNameText b

instance NFData IdentInfo where
    rnf IdentInfo{..} =
        -- deliberately skip the rendered field
        rnf name `seq` rnf parent `seq` rnf isDatacon `seq` rnf moduleNameText

-- | Render an identifier as imported or exported style.
-- TODO: pattern synonym
renderIEWrapped :: Name -> Text
renderIEWrapped n
  | isTcOcc occ && isSymOcc occ = "type " <> pack (printName n)
  | otherwise = pack $ printName n
  where
    occ = occName n

mkIdentInfos :: Text -> AvailInfo -> [IdentInfo]
mkIdentInfos mod (Avail n) =
    [IdentInfo (nameOccName n) (renderIEWrapped n) Nothing (isDataConName n) mod]
mkIdentInfos mod (AvailTC parent (n:nn) flds)
    -- Following the GHC convention that parent == n if parent is exported
    | n == parent
    = [ IdentInfo (nameOccName n) (renderIEWrapped n) (Just $! parentP) (isDataConName n) mod
        | n <- nn ++ map flSelector flds
      ] ++
      [ IdentInfo (nameOccName n) (renderIEWrapped n) Nothing (isDataConName n) mod]
    where
        parentP = pack $ printName parent

mkIdentInfos mod (AvailTC _ nn flds)
    = [ IdentInfo (nameOccName n) (renderIEWrapped n) Nothing (isDataConName n) mod
        | n <- nn ++ map flSelector flds
      ]

buildModuleExportMap:: [ModIface] -> Map.HashMap Text [Text]
buildModuleExportMap modIfaces = do
  let exports = map extractModuleExports modIfaces
  Map.fromListWith (<>) exports

extractModuleExports :: ModIface -> (Text, [Text])
extractModuleExports modIFace = do
  let modName = pack $ moduleNameString $ moduleName $ mi_module modIFace
  let ifaces = mi_exports modIFace
  let functionSet = map (pack . getOccString . availName) ifaces
  (modName, functionSet)

createExportsMap :: [ModIface] -> ExportsMap
createExportsMap modIface = do
  let exportsMap = (Map.fromListWith (<>) . concatMap doOne) modIface
      moduleExportMap = buildModuleExportMap modIface
  ExportsMap exportsMap moduleExportMap
  where
    doOne mi = do
      let getModDetails = unpackAvail $ moduleName $ mi_module mi
      concatMap (fmap (second Set.fromList) . getModDetails) (mi_exports mi)

createExportsMapMg :: [ModGuts] -> ExportsMap
createExportsMapMg modIface = do
  let exportsMap = (Map.fromListWith (<>) . concatMap doOne) modIface
  ExportsMap exportsMap Map.empty
  where
    doOne mi = do
      let getModuleName = moduleName $ mg_module mi
      concatMap (fmap (second Set.fromList) . unpackAvail getModuleName) (mg_exports mi)

createExportsMapTc :: [TcGblEnv] -> ExportsMap
createExportsMapTc modIface = do
  let exportsMap = (Map.fromListWith (<>) . concatMap doOne) modIface
  ExportsMap exportsMap Map.empty 
  where
    doOne mi = do
      let getModuleName = moduleName $ tcg_mod mi
      concatMap (fmap (second Set.fromList) . unpackAvail getModuleName) (tcg_exports mi)

nonInternalModules :: ModuleName -> Bool
nonInternalModules = not . (".Internal" `isSuffixOf`) . moduleNameString

createExportsMapHieDb :: HieDb -> IO ExportsMap
createExportsMapHieDb hiedb = do
    mods <- getAllIndexedMods hiedb
    idents <- forM (filter (nonInternalModules . modInfoName . hieModInfo) mods) $ \m -> do
        let mn = modInfoName $ hieModInfo m
            mText = pack $ moduleNameString mn
        fmap (wrap . unwrap mText) <$> getExportsForModule hiedb mn
    let exportsMap = Map.fromListWith (<>) (concat idents)
    return $ ExportsMap exportsMap Map.empty
  where
    wrap identInfo = (rendered identInfo, Set.fromList [identInfo])
    -- unwrap :: ExportRow -> IdentInfo
    unwrap m ExportRow{..} = IdentInfo exportName n p exportIsDatacon m
      where
          n = pack (occNameString exportName)
          p = pack . occNameString <$> exportParent

unpackAvail :: ModuleName -> IfaceExport -> [(Text, [IdentInfo])]
unpackAvail mn
  | nonInternalModules mn = map f . mkIdentInfos mod
  | otherwise = const []
  where
    !mod = pack $ moduleNameString mn
    f id@IdentInfo {..} = (pack (prettyPrint name), [id])
