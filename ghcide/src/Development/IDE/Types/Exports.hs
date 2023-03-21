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
    renderOcc,
    mkTypeOcc,
    mkVarOrDataOcc,
    isDatacon,
    createExportsMap,
    createExportsMapMg,
    buildModuleExportMapFrom,
    createExportsMapHieDb,
    size,
    exportsMapSize,
    updateExportsMapMg
    ) where

import           Control.DeepSeq             (NFData (..), force, ($!!))
import           Control.Monad
import           Data.Bifunctor              (Bifunctor (second))
import           Data.Char                   (isUpper)
import           Data.Hashable               (Hashable)
import           Data.HashMap.Strict         (HashMap, elems)
import qualified Data.HashMap.Strict         as Map
import           Data.HashSet                (HashSet)
import qualified Data.HashSet                as Set
import           Data.List                   (foldl', isSuffixOf)
import           Data.Text                   (Text, uncons)
import           Data.Text.Encoding          (decodeUtf8, encodeUtf8)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Orphans ()
import           Development.IDE.GHC.Util
import           GHC.Generics                (Generic)
import           HieDb


data ExportsMap = ExportsMap
    { getExportsMap       :: !(OccEnv (HashSet IdentInfo))
    , getModuleExportsMap :: !(ModuleNameEnv (HashSet IdentInfo))
    }

instance NFData ExportsMap where
  rnf (ExportsMap a b) = foldOccEnv (\a b -> rnf a `seq` b) (seqEltsUFM rnf b) a

instance Show ExportsMap where
  show (ExportsMap occs mods) =
    unwords [ "ExportsMap { getExportsMap ="
            , printWithoutUniques $ mapOccEnv (text . show) occs
            , "getModuleExportsMap ="
            , printWithoutUniques $ mapUFM (text . show) mods
            , "}"
            ]

-- | `updateExportsMap old new` results in an export map containing
-- the union of old and new, but with all the module entries new overriding
-- those in old.
updateExportsMap :: ExportsMap -> ExportsMap -> ExportsMap
updateExportsMap old new = ExportsMap
  { getExportsMap = delListFromOccEnv (getExportsMap old) old_occs `plusOccEnv` getExportsMap new -- plusOccEnv is right biased
  , getModuleExportsMap = (getModuleExportsMap old) `plusUFM` (getModuleExportsMap new) -- plusUFM is right biased
  }
  where old_occs = concat [map name $ Set.toList (lookupWithDefaultUFM_Directly (getModuleExportsMap old) mempty m_uniq)
                          | m_uniq <- nonDetKeysUFM (getModuleExportsMap new)]

size :: ExportsMap -> Int
size = sum . map (Set.size) . nonDetOccEnvElts . getExportsMap

mkVarOrDataOcc :: Text -> OccName
mkVarOrDataOcc t = mkOcc $ mkFastStringByteString $ encodeUtf8 t
  where
    mkOcc
      | Just (c,_) <- uncons t
      , c == ':' || isUpper c = mkDataOccFS
      | otherwise = mkVarOccFS

mkTypeOcc :: Text -> OccName
mkTypeOcc t = mkTcOccFS $ mkFastStringByteString $ encodeUtf8 t

exportsMapSize :: ExportsMap -> Int
exportsMapSize = foldOccEnv (\_ x -> x+1) 0 . getExportsMap

instance Semigroup ExportsMap where
  ExportsMap a b <> ExportsMap c d = ExportsMap (plusOccEnv_C (<>) a c) (plusUFM_C (<>) b d)

instance Monoid ExportsMap where
  mempty = ExportsMap emptyOccEnv emptyUFM

rendered :: IdentInfo -> Text
rendered = occNameText . name

-- | Render an identifier as imported or exported style.
-- TODO: pattern synonymoccNameText :: OccName -> Text
occNameText :: OccName -> Text
occNameText name
  | isSymOcc name = "(" <> renderedOcc <> ")"
  | isTcOcc name && isSymOcc name = "type (" <> renderedOcc <> ")"
  | otherwise = renderedOcc
  where
    renderedOcc = renderOcc name

renderOcc :: OccName -> Text
renderOcc = decodeUtf8 . bytesFS . occNameFS

moduleNameText :: IdentInfo -> Text
moduleNameText = moduleNameText' . identModuleName

moduleNameText' :: ModuleName -> Text
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
  let exportsMap = mkOccEnv_C (<>) $ map (\(a,_,c) -> (a, c)) exportList
  force $ ExportsMap exportsMap $ buildModuleExportMap $ map (\(_,b,c) -> (b, c)) exportList -- UFM is lazy, so need to seq
  where
    doOne modIFace = do
      let getModDetails = unpackAvail $ moduleName $ mi_module modIFace
      concatMap (getModDetails) (mi_exports modIFace)

createExportsMapMg :: [ModGuts] -> ExportsMap
createExportsMapMg modGuts = do
  let exportList = concatMap doOne modGuts
  let exportsMap = mkOccEnv_C (<>) $ map (\(a,_,c) -> (a, c)) exportList
  force $ ExportsMap exportsMap $ buildModuleExportMap $ map (\(_,b,c) -> (b, c)) exportList -- UFM is lazy, so need to seq
  where
    doOne mi = do
      let getModuleName = moduleName $ mg_module mi
      concatMap (unpackAvail getModuleName) (mg_exports mi)

updateExportsMapMg :: [ModGuts] -> ExportsMap -> ExportsMap
updateExportsMapMg modGuts old = updateExportsMap old new
    where
        new = createExportsMapMg modGuts

nonInternalModules :: ModuleName -> Bool
nonInternalModules = not . (".Internal" `isSuffixOf`) . moduleNameString

type WithHieDb = forall a. (HieDb -> IO a) -> IO a

createExportsMapHieDb :: WithHieDb -> IO ExportsMap
createExportsMapHieDb withHieDb = do
    mods <- withHieDb getAllIndexedMods
    idents' <- forM (filter (nonInternalModules . modInfoName . hieModInfo) mods) $ \m -> do
        let mn = modInfoName $ hieModInfo m
        fmap (unwrap mn) <$> withHieDb (\hieDb -> getExportsForModule hieDb mn)
    let idents = concat idents'
    let exportsMap = mkOccEnv_C (<>) (keyWith name idents)
    return $!! ExportsMap exportsMap $ buildModuleExportMap (keyWith identModuleName idents) -- UFM is lazy so need to seq
  where
    unwrap m ExportRow{..} = IdentInfo exportName exportParent m
    keyWith f xs = [(f x, Set.singleton x) | x <- xs]

unpackAvail :: ModuleName -> IfaceExport -> [(OccName, ModuleName, HashSet IdentInfo)]
unpackAvail mn
  | nonInternalModules mn = map f . mkIdentInfos mn
  | otherwise = const []
  where
    f id@IdentInfo {..} = (name, mn, Set.singleton id)


identInfoToKeyVal :: IdentInfo -> (ModuleName, IdentInfo)
identInfoToKeyVal identInfo =
  (identModuleName identInfo, identInfo)

buildModuleExportMap:: [(ModuleName, HashSet IdentInfo)] -> ModuleNameEnv (HashSet IdentInfo)
buildModuleExportMap exportsMap = do
  let lst = concatMap (Set.toList. snd) exportsMap
  let lstThree = map identInfoToKeyVal lst
  sortAndGroup lstThree

buildModuleExportMapFrom:: [ModIface] -> ModuleNameEnv (HashSet IdentInfo)
buildModuleExportMapFrom modIfaces = do
  let exports = map extractModuleExports modIfaces
  listToUFM_C (<>) exports

extractModuleExports :: ModIface -> (ModuleName, HashSet IdentInfo)
extractModuleExports modIFace = do
  let modName = moduleName $ mi_module modIFace
  let functionSet = Set.fromList $ concatMap (mkIdentInfos modName) $ mi_exports modIFace
  (modName, functionSet)

sortAndGroup :: [(ModuleName, IdentInfo)] -> ModuleNameEnv (HashSet IdentInfo)
sortAndGroup assocs = listToUFM_C (<>) [(k, Set.fromList [v]) | (k, v) <- assocs]
