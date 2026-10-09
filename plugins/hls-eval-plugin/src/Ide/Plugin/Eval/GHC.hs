{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |GHC API utilities
module Ide.Plugin.Eval.GHC (
    addExtension,
    addImport,
    hasPackage,
    addPackages,
    modifyFlags,
    showDynFlags,
    setSessionAndInteractiveDynFlags,
) where

import           Data.List                       (isPrefixOf)
import           Data.Maybe                      (mapMaybe)
import           Data.String                     (fromString)
import qualified Data.Text                       as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import qualified Development.IDE.GHC.Compat.Util as EnumSet
import           Development.IDE.GHC.Util        (printOutputable)

import           GHC.LanguageExtensions.Type     (Extension (..))
import           Ide.Plugin.Eval.Util            (gStrictTry)

import           GHC                             (setTopSessionDynFlags,
                                                  setUnitDynFlags)
import           GHC.Driver.Env
import           GHC.Driver.Session              (getDynFlags)

hasPackage :: DynFlags -> String -> Bool
hasPackage df = hasPackage_ (packageFlags df)

hasPackage_ :: [PackageFlag] -> [Char] -> Bool
hasPackage_ pkgFlags name = any (name `isPrefixOf`) (pkgNames_ pkgFlags)

pkgNames :: DynFlags -> [String]
pkgNames = pkgNames_ . packageFlags

pkgNames_ :: [PackageFlag] -> [String]
pkgNames_ =
    mapMaybe
        ( \case
            ExposePackage _ (PackageArg n) _  -> Just n
            ExposePackage _ (UnitIdArg uid) _ -> Just $ T.unpack $ printOutputable uid
            _                                 -> Nothing
        )

addPackages :: [String] -> Ghc (Either String DynFlags)
addPackages pkgNames = gStrictTry $
    modifyFlags $ \df ->
        df{packageFlags = foldr (\pkgName pf -> if hasPackage_ pf pkgName then pf else expose pkgName : pf) (packageFlags df) pkgNames}
  where
    expose name = ExposePackage ("-package " ++ name) (PackageArg name) (ModRenaming True [])

modifyFlags :: GhcMonad m => (DynFlags -> DynFlags) -> m DynFlags
modifyFlags f = do
    df <- getSessionDynFlags
    _ <- setSessionDynFlags (f df)
    getSessionDynFlags

-- | Add import to evaluation context
addImport :: GhcMonad m => String -> m [InteractiveImport]
addImport i = do
    ctx <- getContext
    idecl <- parseImportDecl i
    setContext $ IIDecl idecl : ctx
    getContext

-- | Add extension to interactive evaluation session
addExtension :: GhcMonad m => Extension -> m ()
addExtension ext =
    modifySession $ \hsc -> hsc{hsc_IC = setExtension (hsc_IC hsc) ext}

setExtension :: InteractiveContext -> Extension -> InteractiveContext
setExtension ic ext = ic{ic_dflags = xopt_set (ic_dflags ic) ext}

deriving instance Read Extension

-- | Partial display of DynFlags contents, for testing purposes
showDynFlags :: DynFlags -> String
showDynFlags df =
    T.unpack . printOutputable . vcat . map (\(n, d) -> text (n ++ ": ") <+> d) $
        [ ("extensions", ppr . extensions $ df)
        , ("extensionFlags", ppr . EnumSet.toList . extensionFlags $ df)
        , ("importPaths", vList $ importPaths df)
        , ("generalFlags", pprHsString . fromString . show . EnumSet.toList . generalFlags $ df)
        , ("pkgNames", vcat . map text $ pkgNames df)
        , ("packageFlags", vcat . map ppr $ packageFlags df)
        ]

vList :: [String] -> SDoc
vList = vcat . map text

setSessionAndInteractiveDynFlags :: DynFlags -> Ghc ()
setSessionAndInteractiveDynFlags df = do
    _ <- setUnitDynFlags (homeUnitId_ df) df
    modifySession (hscUpdateLoggerFlags . hscSetActiveUnitId (homeUnitId_ df))
    df' <- getDynFlags
    setTopSessionDynFlags df'
    sessDyns <- getSessionDynFlags
    setInteractiveDynFlags sessDyns
