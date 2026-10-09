{-# LANGUAGE CPP #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

-- | General utility functions, mostly focused around GHC operations.
module Development.IDE.GHC.Util(
    modifyDynFlags,
    evalGhcEnv,
    -- * GHC wrappers
    printRdrName,
    Development.IDE.GHC.Util.printName,
    ParseResult(..), runParser,
    lookupPackageConfig,
    textToStringBuffer,
    bytestringToStringBuffer,
    stringBufferToByteString,
    moduleImportPath,
    cgGutsToCoreModule,
    fingerprintToBS,
    fingerprintFromByteString,
    fingerprintFromStringBuffer,
    fingerprintFromPut,
    -- * General utilities
    readFileUtf8,
    setHieDir,
    dontWriteHieFiles,
    disableWarningsAsErrors,
    printOutputable,
    printOutputableOneLine,
    printOutputableQualified,
    getExtensions,
    getExtensionsSet,
    stripOccNamePrefix,
    ) where

import           Data.Binary.Put                   (Put, runPut)
import qualified Data.ByteString                   as BS
import           Data.ByteString.Internal          (ByteString (..))
import qualified Data.ByteString.Internal          as BS
import qualified Data.ByteString.Lazy              as LBS
import           Data.IORef
import           Data.List.Extra
import           Data.Maybe
import qualified Data.Text                         as T
import qualified Data.Text.Encoding                as T
import qualified Data.Text.Encoding.Error          as T
import           Development.IDE.GHC.Compat        as GHC hiding (unitState)
import qualified Development.IDE.GHC.Compat.Parser as Compat
import qualified Development.IDE.GHC.Compat.Units  as Compat
import           Development.IDE.Types.Location
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.Storable
import           GHC                               hiding (ParsedModule (..),
                                                    parser)
import           Ide.PluginUtils                   (unescape)
import           System.FilePath

import           Data.Monoid                       (First (..))
import           GHC.Data.EnumSet
import           GHC.Data.FastString
import           GHC.Data.StringBuffer
import           GHC.Utils.Fingerprint
----------------------------------------------------------------------
-- GHC setup

-- | Used to modify dyn flags in preference to calling 'setSessionDynFlags',
--   since that function also reloads packages (which is very slow).
modifyDynFlags :: GhcMonad m => (DynFlags -> DynFlags) -> m ()
modifyDynFlags f = do
  newFlags <- f <$> getSessionDynFlags
  -- We do not use setSessionDynFlags here since we handle package
  -- initialization separately.
  modifySession $ \h ->
    hscSetFlags newFlags h { hsc_IC = (hsc_IC h) {ic_dflags = newFlags} }

-- | Given a 'Unit' try and find the associated 'PackageConfig' in the environment.
lookupPackageConfig :: Unit -> HscEnv -> Maybe GHC.UnitInfo
lookupPackageConfig unit env =
    Compat.lookupUnit' False unitState prClsre unit
    where
        unitState = Compat.getUnitInfoMap env
        prClsre = preloadClosureUs env


-- | Convert from the @text@ package to the @GHC@ 'StringBuffer'.
--   Currently implemented somewhat inefficiently (if it ever comes up in a profile).
textToStringBuffer :: T.Text -> StringBuffer
textToStringBuffer = stringToStringBuffer . T.unpack

runParser :: DynFlags -> String -> P a -> ParseResult a
runParser flags str parser = unP parser parseState
    where
      filename = "<interactive>"
      location = mkRealSrcLoc (mkFastString filename) 1 1
      buffer = stringToStringBuffer str
      parseState = Compat.initParserState (Compat.initParserOpts flags) buffer location

stringBufferToByteString :: StringBuffer -> ByteString
stringBufferToByteString StringBuffer{..} = PS buf cur len

bytestringToStringBuffer :: ByteString -> StringBuffer
bytestringToStringBuffer (PS buf cur len) = StringBuffer{..}

-- | Pretty print a 'RdrName' wrapping operators in parens
printRdrName :: RdrName -> String
printRdrName name = T.unpack $ printOutputable $ parenSymOcc rn (ppr rn)
  where
    rn = rdrNameOcc name

-- | Pretty print a 'Name' wrapping operators in parens
printName :: Name -> String
printName = printRdrName . nameRdrName

-- | Run a 'Ghc' monad value using an existing 'HscEnv'. Sets up and tears down all the required
--   pieces, but designed to be more efficient than a standard 'runGhc'.
evalGhcEnv :: HscEnv -> Ghc b -> IO b
evalGhcEnv env act = snd <$> runGhcEnv env act

-- | Run a 'Ghc' monad value using an existing 'HscEnv'. Sets up and tears down all the required
--   pieces, but designed to be more efficient than a standard 'runGhc'.
runGhcEnv :: HscEnv -> Ghc a -> IO (HscEnv, a)
runGhcEnv env act = do
    hsc_env <- initTempFs env
    ref <- newIORef hsc_env
    res <- unGhc (withCleanupSession act) (Session ref)
    (,res) <$> readIORef ref

-- | Given a module location, and its parse tree, figure out what is the include directory implied by it.
--   For example, given the file @\/usr\/\Test\/Foo\/Bar.hs@ with the module name @Foo.Bar@ the directory
--   @\/usr\/Test@ should be on the include path to find sibling modules.
moduleImportPath :: NormalizedFilePath -> GHC.ModuleName -> Maybe FilePath
-- The call to takeDirectory is required since DAML does not require that
-- the file name matches the module name in the last component.
-- Once that has changed we can get rid of this.
moduleImportPath (takeDirectory . fromNormalizedFilePath -> pathDir) mn
    -- This happens for single-component modules since takeDirectory "A" == "."
    | modDir == "." = Just pathDir
    | otherwise = dropTrailingPathSeparator <$> stripSuffix modDir pathDir
  where
    -- A for module A.B
    modDir =
        takeDirectory $
        fromNormalizedFilePath $ toNormalizedFilePath' $
        moduleNameSlashes mn

-- | Read a UTF8 file, with lenient decoding, so it will never raise a decoding error.
readFileUtf8 :: FilePath -> IO T.Text
readFileUtf8 f = T.decodeUtf8With T.lenientDecode <$> BS.readFile f

-- | Convert from a 'CgGuts' to a 'CoreModule'.
cgGutsToCoreModule :: SafeHaskellMode -> CgGuts -> ModDetails -> CoreModule
cgGutsToCoreModule safeMode guts modDetails = CoreModule
    (cg_module guts)
    (md_types modDetails)
    (cg_binds guts)
    safeMode

-- | Convert a 'Fingerprint' to a 'ByteString' by copying the byte across.
--   Will produce an 8 byte unreadable ByteString.
fingerprintToBS :: Fingerprint -> BS.ByteString
fingerprintToBS (Fingerprint a b) = BS.unsafeCreate 8 $ \ptr -> do
    let ptr' = castPtr ptr
    pokeElemOff ptr' 0 a
    pokeElemOff ptr' 1 b

-- | Take the 'Fingerprint' of a 'StringBuffer'.
fingerprintFromStringBuffer :: StringBuffer -> IO Fingerprint
fingerprintFromStringBuffer (StringBuffer buf len cur) =
    withForeignPtr buf $ \ptr -> fingerprintData (ptr `plusPtr` cur) len

fingerprintFromByteString :: ByteString -> IO Fingerprint
fingerprintFromByteString bs = do
    let (fptr, offset, len) = BS.toForeignPtr bs
    withForeignPtr fptr $ \ptr ->
        fingerprintData (ptr `plusPtr` offset) len

fingerprintFromPut :: Put -> IO Fingerprint
fingerprintFromPut = fingerprintFromByteString . LBS.toStrict . runPut

--------------------------------------------------------------------------------
-- Tracing exactprint terms

-- | Print a GHC value in `defaultUserStyle` without unique symbols.
--
-- This is the most common print utility.
-- It will do something additionally compared to what the 'Outputable' instance does.
--
--   1. print with a user-friendly style: `a_a4ME` as `a`.
--   2. unescape escape sequences of printable unicode characters within a pair of double quotes
printOutputable :: Outputable a => a -> T.Text
printOutputable = printOutputable' printWithoutUniques

printOutputableOneLine :: Outputable a => a -> T.Text
printOutputableOneLine = printOutputable' printWithoutUniquesOneLine

printOutputableQualified :: Outputable a => PrintUnqualified -> a -> T.Text
printOutputableQualified ctx =
    printOutputable' (printSDocQualifiedUnsafe ctx . ppr)

printOutputable' :: Outputable a => (a -> String) -> a -> T.Text
printOutputable' print =
    -- IfaceTyLit from GHC.Iface.Type implements Outputable with 'show'.
    -- Showing a String escapes non-ascii printable characters. We unescape it here.
    -- More discussion at https://github.com/haskell/haskell-language-server/issues/3115.
    unescape . T.pack . print
{-# INLINE printOutputable #-}

getExtensions :: ParsedModule -> [Extension]
getExtensions = toList . getExtensionsSet

getExtensionsSet :: ParsedModule -> EnumSet Extension
getExtensionsSet = extensionFlags . ms_hspp_opts . pm_mod_summary

-- | When e.g. DuplicateRecordFields is enabled, compiler generates
-- names like "$sel:accessor:One" and "$sel:accessor:Two" to
-- disambiguate record selectors
-- https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields/DuplicateRecordFields#Implementation
stripOccNamePrefix :: T.Text -> T.Text
stripOccNamePrefix name = T.takeWhile (/=':') $ fromMaybe name $
    getFirst $ foldMap (First . (`T.stripPrefix` name))
    occNamePrefixes

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
