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
  ) where

import           Control.Monad
import           Data.IORef
import           Data.Maybe
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat.Util as Util
import           GHC.Core
import           GHC.CoreToIface
import           GHC.Fingerprint
import           GHC.Iface.Binary
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
  { cf_bindings   :: [IfaceBindingX IfaceMaybeRhs IfaceTopBndrInfo]
  -- ^ The actual core file bindings, deserialized lazily
  , cf_iface_hash :: !Fingerprint
  }

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
-- In GHC 9.6, implicit binds are tidied and part of core binds
codeGutsToCoreFile hash CgGuts{..} = CoreFile (map toIfaceTopBind cg_binds) hash

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

typecheckCoreFile :: Module -> IORef TypeEnv -> CoreFile -> IfG CoreProgram
typecheckCoreFile this_mod type_var (CoreFile prepd_binding _) =
  initIfaceLcl this_mod (text "typecheckCoreFile") NotBoot $ do
    tcTopIfaceBindings type_var prepd_binding
