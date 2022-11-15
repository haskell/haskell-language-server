{-# LANGUAGE CPP             #-}
{-# LANGUAGE ConstraintKinds #-}
-- | GHC Utils and Datastructures re-exports.
--
-- Mainly handles module hierarchy re-organisation of GHC
-- from version < 9.0 to >= 9.0.
--
-- Some Functions, such as 'toList' shadow other function-names.
-- This way this module can be imported qualified more naturally.
module Development.IDE.GHC.Compat.Util (
    -- * Exception handling
    MonadCatch,
    GhcException,
    handleGhcException,
    catch,
    try,
    -- * Bags
    Bag,
    bagToList,
    listToBag,
    unionBags,
    isEmptyBag,
    -- * Boolean Formula
    LBooleanFormula,
    BooleanFormula(..),
    -- * OverridingBool
#if !MIN_VERSION_ghc(9,3,0)
    OverridingBool(..),
#endif
    -- * Maybes
    MaybeErr(..),
    orElse,
    -- * Pair
    Pair(..),
    -- * EnumSet
    EnumSet,
    toList,
    -- * FastString exports
    FastString,
#if MIN_VERSION_ghc(9,2,0)
    -- Export here, so we can coerce safely on consumer sites
    LexicalFastString(..),
#endif
    uniq,
    unpackFS,
    mkFastString,
    fsLit,
    pprHsString,
    -- * Fingerprint
    Fingerprint(..),
    getFileHash,
    fingerprintData,
    fingerprintString,
    fingerprintFingerprints,
    -- * Unique
    Uniquable,
    nonDetCmpUnique,
    getUnique,
    Unique,
    mkUnique,
    newTagUnique,
    -- * UniqDFM
    emptyUDFM,
    plusUDFM,
    plusUDFM_C,
    -- * String Buffer
    StringBuffer(..),
    hGetStringBuffer,
    stringToStringBuffer,
    nextChar,
    atEnd,
    ) where

#if MIN_VERSION_ghc(9,0,0)
import           Control.Exception.Safe  (MonadCatch, catch, try)
import           GHC.Data.Bag
import           GHC.Data.BooleanFormula
import           GHC.Data.EnumSet

import           GHC.Data.FastString
import           GHC.Data.Maybe
import           GHC.Data.Pair
import           GHC.Data.StringBuffer
import           GHC.Types.Unique
import           GHC.Types.Unique.DFM
import           GHC.Utils.Fingerprint
import           GHC.Utils.Misc
import           GHC.Utils.Outputable    (pprHsString)
import           GHC.Utils.Panic         hiding (try)
#else
import           Bag
import           BooleanFormula
import           EnumSet
import qualified Exception
import           FastString
import           Fingerprint
import           Maybes
import           Outputable              (pprHsString)
import           Pair
import           Panic                   hiding (try)
import           StringBuffer
import           UniqDFM
import           Unique
import           Util
#endif

#if !MIN_VERSION_ghc(9,0,0)
type MonadCatch = Exception.ExceptionMonad

-- We are using Safe here, which is not equivalent, but probably what we want.
catch :: (Exception.ExceptionMonad m, Exception e) => m a -> (e -> m a) -> m a
catch = Exception.gcatch

try :: (Exception.ExceptionMonad m, Exception e) => m a -> m (Either e a)
try = Exception.gtry
#endif
