-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances for GHC.
--   Note that the 'NFData' instances may not be law abiding.
module Development.IDE.GHC.Orphans() where
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Util

import           Control.DeepSeq
import           Control.Monad.Trans.Reader (ReaderT (..))
import           Data.Aeson
import           Data.Hashable
import           Data.String                (IsString (fromString))
import           Data.Text                  (unpack)

-- See Note [Guidelines For Using CPP In GHCIDE Import Statements]

import           GHC.ByteCode.Types
import           GHC.Data.Bag
import           GHC.Data.FastString
import qualified GHC.Data.StringBuffer      as SB
import           GHC.Types.SrcLoc

#if !MIN_VERSION_ghc(9,3,0)
import           GHC                        (ModuleGraph)
import           GHC.Types.Unique           (getKey)
#endif

import           Data.Bifunctor             (Bifunctor (..))
import           GHC.Parser.Annotation

#if MIN_VERSION_ghc(9,3,0)
import           GHC.Types.PkgQual
#endif

#if MIN_VERSION_ghc(9,5,0)
import           GHC.Unit.Home.ModInfo
#endif

-- Orphan instance for Shake.hs
-- https://hub.darcs.net/ross/transformers/issue/86
deriving instance (Semigroup (m a)) => Semigroup (ReaderT r m a)

-- Orphan instances for types from the GHC API.
instance Show CoreModule where show = unpack . printOutputable
instance NFData CoreModule where rnf = rwhnf
instance Show CgGuts where show = unpack . printOutputable . cg_module
instance NFData CgGuts where rnf = rwhnf
instance Show ModDetails where show = const "<moddetails>"
instance NFData ModDetails where rnf = rwhnf
instance NFData SafeHaskellMode where rnf = rwhnf
instance Show Linkable where show = unpack . printOutputable
instance NFData Linkable where rnf (LM a b c) = rnf a `seq` rnf b `seq` rnf c
instance NFData Unlinked where
  rnf (DotO f)   = rnf f
  rnf (DotA f)   = rnf f
  rnf (DotDLL f) = rnf f
  rnf (BCOs a b) = seqCompiledByteCode a `seq` liftRnf rwhnf b
  rnf  _         = error "rnf: not implemented for Unlinked"
instance Show PackageFlag where show = unpack . printOutputable
instance Show InteractiveImport where show = unpack . printOutputable
instance Show PackageName  where show = unpack . printOutputable

instance Show UnitId where show = unpack . printOutputable
deriving instance Ord SrcSpan
deriving instance Ord UnhelpfulSpanReason

instance NFData SB.StringBuffer where rnf = rwhnf

instance Show Module where
    show = moduleNameString . moduleName

#if !MIN_VERSION_ghc(9,3,0)
instance Outputable a => Show (GenLocated SrcSpan a) where show = unpack . printOutputable
#endif

#if !MIN_VERSION_ghc(9,5,0)
instance (NFData l, NFData e) => NFData (GenLocated l e) where
    rnf (L l e) = rnf l `seq` rnf e
#endif

instance Show ModSummary where
    show = show . ms_mod

instance Show ParsedModule where
    show = show . pm_mod_summary

instance NFData ModSummary where
    rnf = rwhnf

instance Ord FastString where
    compare a b = if a == b then EQ else compare (fs_sbs a) (fs_sbs b)

instance NFData (SrcSpanAnn' a) where
    rnf = rwhnf

instance Bifunctor GenLocated where
    bimap f g (L l x) = L (f l) (g x)

deriving instance Functor SrcSpanAnn'

instance NFData ParsedModule where
    rnf = rwhnf

instance Show HieFile where
    show = show . hie_module

instance NFData HieFile where
    rnf = rwhnf

#if !MIN_VERSION_ghc(9,3,0)
deriving instance Eq SourceModified
deriving instance Show SourceModified
instance NFData SourceModified where
    rnf = rwhnf
#endif

instance Hashable ModuleName where
    hashWithSalt salt = hashWithSalt salt . show


instance NFData a => NFData (IdentifierDetails a) where
    rnf (IdentifierDetails a b) = rnf a `seq` rnf (length b)

instance NFData RealSrcSpan where
    rnf = rwhnf

srcSpanFileTag, srcSpanStartLineTag, srcSpanStartColTag,
    srcSpanEndLineTag, srcSpanEndColTag :: String
srcSpanFileTag = "srcSpanFile"
srcSpanStartLineTag = "srcSpanStartLine"
srcSpanStartColTag = "srcSpanStartCol"
srcSpanEndLineTag = "srcSpanEndLine"
srcSpanEndColTag = "srcSpanEndCol"

instance ToJSON RealSrcSpan where
  toJSON spn =
      object
        [ fromString srcSpanFileTag .= unpackFS (srcSpanFile spn)
        , fromString srcSpanStartLineTag .= srcSpanStartLine spn
        , fromString srcSpanStartColTag .= srcSpanStartCol spn
        , fromString srcSpanEndLineTag .= srcSpanEndLine spn
        , fromString srcSpanEndColTag .= srcSpanEndCol spn
        ]

instance FromJSON RealSrcSpan where
  parseJSON = withObject "object" $ \obj -> do
      file <- fromString <$> (obj .: fromString srcSpanFileTag)
      mkRealSrcSpan
        <$> (mkRealSrcLoc file
                <$> obj .: fromString srcSpanStartLineTag
                <*> obj .: fromString srcSpanStartColTag
            )
        <*> (mkRealSrcLoc file
                <$> obj .: fromString srcSpanEndLineTag
                <*> obj .: fromString srcSpanEndColTag
            )

instance NFData Type where
    rnf = rwhnf

instance Show a => Show (Bag a) where
    show = show . bagToList

#if !MIN_VERSION_ghc(9,5,0)
instance NFData HsDocString where
    rnf = rwhnf
#endif

instance Show ModGuts where
    show _ = "modguts"
instance NFData ModGuts where
    rnf = rwhnf

instance NFData (ImportDecl GhcPs) where
    rnf = rwhnf

#if MIN_VERSION_ghc(9,5,0)
instance (NFData (HsModule a)) where
#else
instance (NFData HsModule) where
#endif
  rnf = rwhnf

instance Show OccName where show = unpack . printOutputable


#if MIN_VERSION_ghc(9,7,0)
instance Hashable OccName where hashWithSalt s n = hashWithSalt s (getKey $ getUnique $ occNameFS n, getKey $ getUnique $ occNameSpace n)
#else
instance Hashable OccName where hashWithSalt s n = hashWithSalt s (getKey $ getUnique n)
#endif

instance Show HomeModInfo where show = show . mi_module . hm_iface

instance Show ModuleGraph where show _ = "ModuleGraph {..}"
instance NFData ModuleGraph where rnf = rwhnf

instance NFData HomeModInfo where
  rnf (HomeModInfo iface dets link) = rwhnf iface `seq` rnf dets `seq` rnf link

#if MIN_VERSION_ghc(9,3,0)
instance NFData PkgQual where
  rnf NoPkgQual      = ()
  rnf (ThisPkg uid)  = rnf uid
  rnf (OtherPkg uid) = rnf uid

instance NFData UnitId where
  rnf = rwhnf

instance NFData NodeKey where
  rnf = rwhnf
#endif

#if MIN_VERSION_ghc(9,5,0)
instance NFData HomeModLinkable where
  rnf = rwhnf
#endif

instance NFData (HsExpr (GhcPass Renamed)) where
    rnf = rwhnf

instance NFData (Pat (GhcPass Renamed)) where
    rnf = rwhnf

instance NFData Extension where
  rnf = rwhnf

instance NFData (UniqFM Name [Name]) where
  rnf (ufmToIntMap -> m) = rnf m
