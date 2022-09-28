{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RankNTypes         #-}
module Development.IDE.Types.Exports
(
    IdentInfo(..),
    ExportsMap(..),
    rendered,
    moduleNameText,
    occNameText,
    isDatacon,
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
import           Data.Hashable               (Hashable)
import           Data.HashMap.Strict         (HashMap, elems)
import qualified Data.HashMap.Strict         as Map
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as Set
import           Data.List                   (foldl', isSuffixOf)
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding          (decodeUtf8)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Orphans ()
import           Development.IDE.GHC.Util
import           GHC.Generics                (Generic)
import           HieDb


data ExportsMap = ExportsMap
    { getExportsMap       :: !(HashMap IdentifierText (HashSet IdentInfo))
    , getModuleExportsMap :: !(HashMap ModuleNameText (HashSet IdentInfo))
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


rendered :: IdentInfo -> IdentifierText
rendered = occNameText . name

-- | Render an identifier as imported or exported style.
-- TODO: pattern synonymoccNameText :: OccName -> Text
occNameText :: OccName -> IdentifierText
occNameText name
  | isTcOcc name && isSymOcc name = "type " <> renderOcc
  | otherwise = renderOcc
  where
    renderOcc = decodeUtf8 . bytesFS . occNameFS $ name

moduleNameText :: IdentInfo -> ModuleNameText
moduleNameText = moduleNameText' . identModuleName

moduleNameText' :: ModuleName -> ModuleNameText
moduleNameText' = decodeUtf8 . bytesFS . moduleNameFS

data IdentInfo = IdentInfo
    { name            :: !OccName
    , parent          :: !(Maybe OccName)
    , identModuleName :: !ModuleName
    }
    deriving (Generic, Show)
    deriving anyclass Hashable

isDatacon :: IdentInfo -> Bool
isDatacon = isDataOcc . name

instance Eq IdentInfo where
    a == b = name a == name b
          && parent a == parent b
          && identModuleName a == identModuleName b

instance NFData IdentInfo where
    rnf IdentInfo{..} =
        -- deliberately skip the rendered field
        rnf name `seq` rnf parent `seq` rnf identModuleName

mkIdentInfos :: ModuleName -> AvailInfo -> [IdentInfo]
mkIdentInfos mod (AvailName n) =
    [IdentInfo (nameOccName n) Nothing mod]
mkIdentInfos mod (AvailFL fl) =
    [IdentInfo (nameOccName n) Nothing mod]
    where
      n = flSelector fl
mkIdentInfos mod (AvailTC parent (n:nn) flds)
    -- Following the GHC convention that parent == n if parent is exported
    | n == parent
    = [ IdentInfo (nameOccName n) (Just $! nameOccName parent) mod
        | n <- nn ++ map flSelector flds
      ] ++
      [ IdentInfo (nameOccName n) Nothing mod]

mkIdentInfos mod (AvailTC _ nn flds)
    = [ IdentInfo (nameOccName n) Nothing mod
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
updateExportsMapMg modGuts old = old' <> new
    where
        new = createExportsMapMg modGuts
        old' = deleteAll old (Map.keys $ getModuleExportsMap new)
        deleteAll = foldl' (flip deleteEntriesForModule)

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

type WithHieDb = forall a. (HieDb -> IO a) -> IO a

createExportsMapHieDb :: WithHieDb -> IO ExportsMap
createExportsMapHieDb withHieDb = do
    mods <- withHieDb getAllIndexedMods
    idents <- forM (filter (nonInternalModules . modInfoName . hieModInfo) mods) $ \m -> do
        let mn = modInfoName $ hieModInfo m
        fmap (wrap . unwrap mn) <$> withHieDb (\hieDb -> getExportsForModule hieDb mn)
    let exportsMap = Map.fromListWith (<>) (concat idents)
    return $! ExportsMap exportsMap $ buildModuleExportMap (concat idents)
  where
    wrap identInfo = (rendered identInfo, Set.fromList [identInfo])
    -- unwrap :: ExportRow -> IdentInfo
    unwrap m ExportRow{..} = IdentInfo exportName exportParent m

unpackAvail :: ModuleName -> IfaceExport -> [(Text, Text, [IdentInfo])]
unpackAvail mn
  | nonInternalModules mn = map f . mkIdentInfos mn
  | otherwise = const []
  where
    f id@IdentInfo {..} = (printOutputable name, moduleNameText id,[id])


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
  let modName = moduleName $ mi_module modIFace
  let functionSet = Set.fromList $ concatMap (mkIdentInfos modName) $ mi_exports modIFace
  (moduleNameText' modName, functionSet)

sortAndGroup :: [(ModuleNameText, IdentInfo)] -> Map.HashMap ModuleNameText (HashSet IdentInfo)
sortAndGroup assocs = Map.fromListWith (<>) [(k, Set.fromList [v]) | (k, v) <- assocs]
