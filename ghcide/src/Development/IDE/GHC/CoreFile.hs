{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

-- | CoreFiles let us serialize Core to a file in order to later recover it
-- without reparsing or retypechecking
module Development.IDE.GHC.CoreFile
  ( CoreFile
  , codeGutsToCoreFile
  , typecheckCoreFile
  , readBinCoreFile
  , writeBinCoreFile
  , getImplicitBinds) where

import Data.IORef
import Data.Foldable
import Data.List (isPrefixOf)
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe

import Development.IDE.GHC.Compat

#if MIN_VERSION_ghc(9,0,0)
import GHC.Utils.Binary
import GHC.Core
import GHC.CoreToIface
import GHC.IfaceToCore
import GHC.Iface.Env
import GHC.Iface.Binary
import GHC.Types.Id.Make

#if MIN_VERSION_ghc(9,2,0)
import GHC.Types.TypeEnv
#else
import GHC.Driver.Types
#endif

#elif MIN_VERSION_ghc(8,6,0)
import Binary
import CoreSyn
import ToIface
import TcIface
import IfaceEnv
import BinIface
import HscTypes
import IdInfo
import Var
import Unique
import MkId
#endif

-- | Initial ram buffer to allocate for writing interface files
initBinMemSize :: Int
initBinMemSize = 1024 * 1024

newtype CoreFile = CoreFile { cf_bindings :: [TopIfaceBinding IfaceId] }

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
  put_ bh (CoreFile a) = put_ bh a
  get bh = CoreFile <$> get bh

readBinCoreFile
  :: NameCacheUpdater
  -> FilePath
  -> IO CoreFile
readBinCoreFile name_cache fat_hi_path = do
    bh <- readBinMem fat_hi_path
    getWithUserData name_cache bh

-- | Write a core file
writeBinCoreFile :: FilePath -> CoreFile -> IO ()
writeBinCoreFile core_path fat_iface = do
    bh <- openBinMem initBinMemSize

    let quietTrace =
#if MIN_VERSION_ghc(9,2,0)
          QuietBinIFace
#else
          (const $ pure ())
#endif

    putWithUserData quietTrace bh fat_iface

    -- And send the result to the file
    writeBinMem bh core_path

-- Implicit binds aren't tidied, so we can't serialise them.
-- This isn't a problem however since we can regenerate them from the
-- original ModIface
codeGutsToCoreFile :: CgGuts -> CoreFile
codeGutsToCoreFile CgGuts{..} = CoreFile (map (toIfaceTopBind cg_module) $ filter isNotImplictBind cg_binds)

-- | Implicit binds can be generated from the interface and are not tidied,
-- so we must filter them out
isNotImplictBind :: CoreBind -> Bool
isNotImplictBind bind = any (not . isImplicitId) $ bindBindings bind

bindBindings :: CoreBind -> [Var]
bindBindings (NonRec b _) = [b]
bindBindings (Rec bnds) = map fst bnds

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
get_defn id = NonRec id (unfoldingTemplate (realIdUnfolding id))

toIfaceTopBndr :: Module -> Id -> IfaceId
toIfaceTopBndr mod id
  = IfaceId (mangleDeclName mod $ getName id)
            (toIfaceType (idType id))
            (toIfaceIdDetails (idDetails id))
            (toIfaceIdInfo (idInfo id))

toIfaceTopBind :: Module -> Bind Id -> TopIfaceBinding IfaceId
toIfaceTopBind mod (NonRec b r) = TopIfaceNonRec (toIfaceTopBndr mod b) (toIfaceExpr r)
toIfaceTopBind mod (Rec prs)    = TopIfaceRec [(toIfaceTopBndr mod b, toIfaceExpr r) | (b,r) <- prs]

typecheckCoreFile :: Module -> IORef TypeEnv -> CoreFile -> IfG CoreProgram
typecheckCoreFile this_mod type_var (CoreFile prepd_binding) =
  initIfaceLcl this_mod (text "typecheckCoreFile") NotBoot $ do
    tcTopIfaceBindings type_var prepd_binding

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

tcTopIfaceBindings :: IORef TypeEnv -> [TopIfaceBinding IfaceId]
          -> IfL [CoreBind]
tcTopIfaceBindings ty_var ver_decls
   = do
     int <- mapM (traverse $ tcIfaceId) ver_decls
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
    -- invariant: 'IfaceId' is always a 'IfaceId' constructor
    getIfaceId (AnId id) = id
    getIfaceId _ = error "tcIfaceId: got non Id"

tc_iface_bindings :: TopIfaceBinding Id -> IfL CoreBind
tc_iface_bindings (TopIfaceNonRec v e) = do
  e' <- tcIfaceExpr e
  pure $ NonRec v e'
tc_iface_bindings (TopIfaceRec vs) = do
  vs' <- traverse (\(v, e) -> (,) <$> pure v <*> tcIfaceExpr e) vs
  pure $ Rec vs'
