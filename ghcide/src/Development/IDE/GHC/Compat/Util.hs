{-# LANGUAGE CPP #-}
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
    OverridingBool(..),
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
    -- Export here, so we can coerce safely on consumer sites
    LexicalFastString(..),
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

import           Control.Exception.Safe  (MonadCatch, catch, try)
import           GHC.Data.Bag
import           GHC.Data.Bool
import           GHC.Data.BooleanFormula
import           GHC.Data.EnumSet
import           GHC.Data.FastString
import           GHC.Data.Maybe
import           GHC.Data.Pair
import           GHC.Data.StringBuffer
import           GHC.Types.Unique
import           GHC.Types.Unique.DFM
import           GHC.Utils.Fingerprint
import           GHC.Utils.Outputable    (pprHsString)
import           GHC.Utils.Panic         hiding (try)
