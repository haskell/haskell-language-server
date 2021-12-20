{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
module Development.IDE.Types.Exports
(
    IdentInfo(..),
    ExportsMap(..),
    createExportsMap,
    createExportsMapMg,
    createExportsMapTc,
    buildModuleExportMapFrom,
    createExportsMapHieDb,
    size,
    updateExportsMapMg
    ) where

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
import           GHC.Generics                (Generic)
import           HieDb


data ExportsMap = ExportsMap
    { getExportsMap       :: HashMap IdentifierText (HashSet IdentInfo)
    , getModuleExportsMap :: HashMap ModuleNameText (HashSet IdentInfo)
    }
    deriving (Show)

deleteEntriesForModule :: ModuleNameText -> ExportsMap -> ExportsMap
deleteEntriesForModule m em = ExportsMap
    { getExportsMap =
        let moduleIds = Map.lookupDefault mempty m (getModuleExportsMap em)
        in deleteAll
            (rendered <$> Set.toList moduleIds)
            (getExportsMap em)
    , getModuleExportsMap = Map.delete m (getModuleExportsMap em)
    }
    where
        deleteAll keys map = foldr Map.delete map keys

size :: ExportsMap -> Int
size = sum . map length . elems . getExportsMap

instance Semigroup ExportsMap where
  ExportsMap a b <> ExportsMap c d = ExportsMap (Map.unionWith (<>) a c) (Map.unionWith (<>) b d)

instance Monoid ExportsMap where
  mempty = ExportsMap Map.empty Map.empty

type IdentifierText = Text
type ModuleNameText = Text

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
mkIdentInfos mod (AvailName n) =
    [IdentInfo (nameOccName n) (renderIEWrapped n) Nothing (isDataConName n) mod]
mkIdentInfos mod (AvailFL fl) =
    [IdentInfo (nameOccName n) (renderIEWrapped n) Nothing (isDataConName n) mod]
    where
      n = flSelector fl
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

createExportsMap :: [ModIface] -> ExportsMap
createExportsMap modIface = do
  let exportList = concatMap doOne modIface
  let exportsMap = Map.fromListWith (<>) $ map (\(a,_,c) -> (a, c)) exportList
  ExportsMap exportsMap $ buildModuleExportMap $ map (\(_,b,c) -> (b, c)) exportList
  where
    doOne modIFace = do
      let getModDetails = unpackAvail $ moduleName $ mi_module modIFace
      concatMap (fmap (second Set.fromList) . getModDetails) (mi_exports modIFace)

createExportsMapMg :: [ModGuts] -> ExportsMap
createExportsMapMg modGuts = do
  let exportList = concatMap doOne modGuts
  let exportsMap = Map.fromListWith (<>) $ map (\(a,_,c) -> (a, c)) exportList
  ExportsMap exportsMap $ buildModuleExportMap $ map (\(_,b,c) -> (b, c)) exportList
  where
    doOne mi = do
      let getModuleName = moduleName $ mg_module mi
      concatMap (fmap (second Set.fromList) . unpackAvail getModuleName) (mg_exports mi)

updateExportsMapMg :: [ModGuts] -> ExportsMap -> ExportsMap
updateExportsMapMg modGuts old =
    old' <> new
    where
        new = createExportsMapMg modGuts
        old' = deleteAll old (Map.keys $ getModuleExportsMap new)
        deleteAll = foldr deleteEntriesForModule


createExportsMapTc :: [TcGblEnv] -> ExportsMap
createExportsMapTc modIface = do
  let exportList = concatMap doOne modIface
  let exportsMap = Map.fromListWith (<>) $ map (\(a,_,c) -> (a, c)) exportList
  ExportsMap exportsMap $ buildModuleExportMap $ map (\(_,b,c) -> (b, c)) exportList
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
    return $ ExportsMap exportsMap $ buildModuleExportMap (concat idents)
  where
    wrap identInfo = (rendered identInfo, Set.fromList [identInfo])
    -- unwrap :: ExportRow -> IdentInfo
    unwrap m ExportRow{..} = IdentInfo exportName n p exportIsDatacon m
      where
          n = pack (occNameString exportName)
          p = pack . occNameString <$> exportParent

unpackAvail :: ModuleName -> IfaceExport -> [(Text, Text, [IdentInfo])]
unpackAvail mn
  | nonInternalModules mn = map f . mkIdentInfos mod
  | otherwise = const []
  where
    !mod = pack $ moduleNameString mn
    f id@IdentInfo {..} = (pack (prettyPrint name), moduleNameText,[id])


identInfoToKeyVal :: IdentInfo -> (ModuleNameText, IdentInfo)
identInfoToKeyVal identInfo =
  (moduleNameText identInfo, identInfo)

buildModuleExportMap:: [(Text, HashSet IdentInfo)] -> Map.HashMap ModuleNameText (HashSet IdentInfo)
buildModuleExportMap exportsMap = do
  let lst = concatMap (Set.toList. snd) exportsMap
  let lstThree = map identInfoToKeyVal lst
  sortAndGroup lstThree

buildModuleExportMapFrom:: [ModIface] -> Map.HashMap Text (HashSet IdentInfo)
buildModuleExportMapFrom modIfaces = do
  let exports = map extractModuleExports modIfaces
  Map.fromListWith (<>) exports

extractModuleExports :: ModIface -> (Text, HashSet IdentInfo)
extractModuleExports modIFace = do
  let modName = pack $ moduleNameString $ moduleName $ mi_module modIFace
  let functionSet = Set.fromList $ concatMap (mkIdentInfos modName) $ mi_exports modIFace
  (modName, functionSet)

sortAndGroup :: [(ModuleNameText, IdentInfo)] -> Map.HashMap ModuleNameText (HashSet IdentInfo)
sortAndGroup assocs = Map.fromListWith (<>) [(k, Set.fromList [v]) | (k, v) <- assocs]
