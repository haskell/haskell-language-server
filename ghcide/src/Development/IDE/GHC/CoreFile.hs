{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

-- | CoreFiles let us serialize Core to a file in order to later recover it
-- without reparsing or retypechecking
module Development.IDE.GHC.CoreFile
  ( CoreFile(..)
  , codeGutsToCoreFile
  , typecheckCoreFile
  , readBinCoreFile
  , writeBinCoreFile
  , getImplicitBinds
  , occNamePrefixes) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Foldable
import           Data.IORef
import           Data.List                       (isPrefixOf)
import           Data.Maybe
import qualified Data.Text                       as T
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat.Util as Util
import           GHC.Core
import           GHC.CoreToIface
import           GHC.Fingerprint
import           GHC.Iface.Binary
import           GHC.Iface.Env
#if MIN_VERSION_ghc(9,11,0)
import qualified GHC.Iface.Load                  as Iface
#endif
import           GHC.Iface.Recomp.Binary         (fingerprintBinMem)
import           GHC.IfaceToCore
import           GHC.Types.Id.Make
import           GHC.Types.TypeEnv
import           GHC.Utils.Binary
import           Prelude                         hiding (mod)


-- | Initial ram buffer to allocate for writing interface files
initBinMemSize :: Int
initBinMemSize = 1024 * 1024

data CoreFile
  = CoreFile
  { cf_bindings   :: [TopIfaceBinding IfaceId]
  -- ^ The actual core file bindings, deserialized lazily
  , cf_iface_hash :: !Fingerprint
  }

-- | Like IfaceBinding, but lets us serialize internal names as well
data TopIfaceBinding v
  = TopIfaceNonRec v IfaceExpr
  | TopIfaceRec    [(v, IfaceExpr)]
  deriving (Functor, Foldable, Traversable)

-- | GHC doesn't export 'tcIdDetails', 'tcIfaceInfo', or 'tcIfaceType',
-- but it does export 'tcIfaceDecl'
-- so we use `IfaceDecl` as a container for all of these
-- invariant: 'IfaceId' is always a 'IfaceId' constructor
type IfaceId = IfaceDecl

instance Binary (TopIfaceBinding IfaceId) where
  put_ bh (TopIfaceNonRec d e) = do
    putByte bh 0
    put_ bh d
    put_ bh e
  put_ bh (TopIfaceRec vs) = do
    putByte bh 1
    put_ bh vs
  get bh = do
    t <- getByte bh
    case t of
      0 -> TopIfaceNonRec <$> get bh <*> get bh
      1 -> TopIfaceRec <$> get bh
      _ -> error "Binary TopIfaceBinding"

instance Binary CoreFile where
  put_ bh (CoreFile core fp) = lazyPut bh core >> put_ bh fp
  get bh = CoreFile <$> lazyGet bh <*> get bh

readBinCoreFile
  :: NameCacheUpdater
  -> FilePath
  -> IO (CoreFile, Fingerprint)
readBinCoreFile name_cache fat_hi_path = do
    bh <- readBinMem fat_hi_path
    file <- getWithUserData name_cache bh
    !fp <- Util.getFileHash fat_hi_path
    return (file, fp)

-- | Write a core file
writeBinCoreFile :: DynFlags -> FilePath -> CoreFile -> IO Fingerprint
writeBinCoreFile _dflags core_path fat_iface = do
    bh <- openBinMem initBinMemSize

    let quietTrace =
          QuietBinIFace

    putWithUserData
      quietTrace
#if MIN_VERSION_ghc(9,11,0)
      (Iface.flagsToIfCompression _dflags)
#endif
      bh
      fat_iface

    -- And send the result to the file
    writeBinMem bh core_path

    !fp <- fingerprintBinMem bh
    pure fp

-- Implicit binds aren't tidied, so we can't serialise them.
-- This isn't a problem however since we can regenerate them from the
-- original ModIface
codeGutsToCoreFile
  :: Fingerprint -- ^ Hash of the interface this was generated from
  -> CgGuts
  -> CoreFile
#if MIN_VERSION_ghc(9,5,0)
-- In GHC 9.6, implicit binds are tidied and part of core binds
codeGutsToCoreFile hash CgGuts{..} = CoreFile (map (toIfaceTopBind1 cg_module) cg_binds) hash
#else
codeGutsToCoreFile hash CgGuts{..} = CoreFile (map (toIfaceTopBind1 cg_module) $ filter isNotImplictBind cg_binds) hash

-- | Implicit binds can be generated from the interface and are not tidied,
-- so we must filter them out
isNotImplictBind :: CoreBind -> Bool
isNotImplictBind bind = not . all isImplicitId $ bindBindings bind

bindBindings :: CoreBind -> [Var]
bindBindings (NonRec b _) = [b]
bindBindings (Rec bnds)   = map fst bnds
#endif

getImplicitBinds :: TyCon -> [CoreBind]
getImplicitBinds tc = cls_binds ++ getTyConImplicitBinds tc
  where
    cls_binds = maybe [] getClassImplicitBinds (tyConClass_maybe tc)

getTyConImplicitBinds :: TyCon -> [CoreBind]
getTyConImplicitBinds tc
  | isNewTyCon tc = []  -- See Note [Compulsory newtype unfolding] in MkId
  | otherwise     = map get_defn (mapMaybe dataConWrapId_maybe (tyConDataCons tc))

getClassImplicitBinds :: Class -> [CoreBind]
getClassImplicitBinds cls
  = [ NonRec op (mkDictSelRhs cls val_index)
    | (op, val_index) <- classAllSelIds cls `zip` [0..] ]

get_defn :: Id -> CoreBind
get_defn identifier = NonRec identifier templ
  where
    templ = case maybeUnfoldingTemplate (realIdUnfolding identifier) of
              Nothing -> error "get_dfn: no unfolding template"
              Just x  -> x

toIfaceTopBndr1 :: Module -> Id -> IfaceId
toIfaceTopBndr1 mod identifier
  = IfaceId (mangleDeclName mod $ getName identifier)
            (toIfaceType (idType identifier))
            (toIfaceIdDetails (idDetails identifier))
            (toIfaceIdInfo (idInfo identifier))

toIfaceTopBind1 :: Module -> Bind Id -> TopIfaceBinding IfaceId
toIfaceTopBind1 mod (NonRec b r) = TopIfaceNonRec (toIfaceTopBndr1 mod b) (toIfaceExpr r)
toIfaceTopBind1 mod (Rec prs)    = TopIfaceRec [(toIfaceTopBndr1 mod b, toIfaceExpr r) | (b,r) <- prs]

typecheckCoreFile :: Module -> IORef TypeEnv -> CoreFile -> IfG CoreProgram
typecheckCoreFile this_mod type_var (CoreFile prepd_binding _) =
  initIfaceLcl this_mod (text "typecheckCoreFile") NotBoot $ do
    tcTopIfaceBindings1 type_var prepd_binding

-- | Internal names can't be serialized, so we mange them
-- to an external name and restore at deserialization time
-- This is necessary because we rely on stuffing TopIfaceBindings into
-- a IfaceId because we don't have access to 'tcIfaceType' etc..
mangleDeclName :: Module -> Name -> Name
mangleDeclName mod name
  | isExternalName name = name
  | otherwise = mkExternalName (nameUnique name) (mangleModule mod) (nameOccName name) (nameSrcSpan name)

-- | Mangle the module name too to avoid conflicts
mangleModule :: Module -> Module
mangleModule mod = mkModule (moduleUnit mod) (mkModuleName $ "GHCIDEINTERNAL" ++ moduleNameString (moduleName mod))

isGhcideModule :: Module -> Bool
isGhcideModule mod = "GHCIDEINTERNAL" `isPrefixOf` (moduleNameString $ moduleName mod)

-- Is this a fake external name that we need to make into an internal name?
isGhcideName :: Name -> Bool
isGhcideName = isGhcideModule . nameModule

tcTopIfaceBindings1 :: IORef TypeEnv -> [TopIfaceBinding IfaceId]
          -> IfL [CoreBind]
tcTopIfaceBindings1 ty_var ver_decls
   = do
     int <- mapM (traverse tcIfaceId) ver_decls
     let all_ids = concatMap toList int
     liftIO $ modifyIORef ty_var (flip extendTypeEnvList $ map AnId all_ids)
     extendIfaceIdEnv all_ids $ mapM tc_iface_bindings int

tcIfaceId :: IfaceId -> IfL Id
tcIfaceId = fmap getIfaceId . tcIfaceDecl False <=< unmangle_decl_name
  where
    unmangle_decl_name ifid@IfaceId{ ifName = name }
    -- Check if the name is mangled
      | isGhcideName name = do
        name' <- newIfaceName (mkVarOcc $ getOccString name)
        pure $ ifid{ ifName = name' }
      | otherwise = pure ifid
    unmangle_decl_name _ifid = error "tcIfaceId: got non IfaceId: "
    -- invariant: 'IfaceId' is always a 'IfaceId' constructor
    getIfaceId (AnId identifier) = identifier
    getIfaceId _                 = error "tcIfaceId: got non Id"

tc_iface_bindings :: TopIfaceBinding Id -> IfL CoreBind
tc_iface_bindings (TopIfaceNonRec v e) = do
  e' <- tcIfaceExpr e
  pure $ NonRec v e'
tc_iface_bindings (TopIfaceRec vs) = do
  vs' <- traverse (\(v, e) -> (v,) <$> tcIfaceExpr e) vs
  pure $ Rec vs'

-- | Prefixes that can occur in a GHC OccName
occNamePrefixes :: [T.Text]
occNamePrefixes =
  [
    -- long ones
    "$con2tag_"
  , "$tag2con_"
  , "$maxtag_"

  -- four chars
  , "$sel:"
  , "$tc'"

  -- three chars
  , "$dm"
  , "$co"
  , "$tc"
  , "$cp"
  , "$fx"

  -- two chars
  , "$W"
  , "$w"
  , "$m"
  , "$b"
  , "$c"
  , "$d"
  , "$i"
  , "$s"
  , "$f"
  , "$r"
  , "C:"
  , "N:"
  , "D:"
  , "$p"
  , "$L"
  , "$f"
  , "$t"
  , "$c"
  , "$m"
  ]
