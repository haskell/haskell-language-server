{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -Wno-orphans #-}

-- |GHC API utilities
module Ide.Plugin.Eval.GHC (
    isExpr,
    addExtension,
    addImport,
    hasPackage,
    addPackages,
    modifyFlags,
    showDynFlags,
) where

import Data.List (isPrefixOf)
import Data.Maybe (mapMaybe)
import Development.IDE.GHC.Compat
import qualified EnumSet
import GHC.LanguageExtensions.Type (Extension (..))
import GhcMonad (modifySession)
import GhcPlugins (DefUnitId (..), InstalledUnitId (..), fsLit, hsc_IC)
import HscTypes (InteractiveContext (ic_dflags))
import Ide.Plugin.Eval.Util (asS, gStrictTry)
import qualified Lexer
import Module (UnitId (DefiniteUnitId))
import Outputable (
    Outputable (ppr),
    SDoc,
    showSDocUnsafe,
    text,
    vcat,
    (<+>),
 )
import qualified Parser
import SrcLoc (mkRealSrcLoc)
import StringBuffer (stringToStringBuffer)

{- $setup
>>> import GHC
>>> import GHC.Paths
>>> run act = runGhc (Just libdir) (getInteractiveDynFlags >>= act)
>>> libdir
"/Users/titto/.ghcup/ghc/8.8.4/lib/ghc-8.8.4"
-}

{- | Returns true if string is an expression

>>> isExprTst e df = return (isExpr df e)
>>> run $ isExprTst "3"
True

>>> run $ isExprTst "(x+y)"
True

>>> run $ isExprTst "import Data.Maybe"
False

>>> run $ isExprTst "three=3"
False
-}
isExpr :: DynFlags -> String -> Bool
isExpr df stmt = case parseThing Parser.parseExpression df stmt of
    Lexer.POk _ _ -> True
    Lexer.PFailed{} -> False

parseThing :: Lexer.P thing -> DynFlags -> String -> Lexer.ParseResult thing
parseThing parser dflags stmt = do
    let buf = stringToStringBuffer stmt
        loc = mkRealSrcLoc (fsLit "<interactive>") 1 1

    Lexer.unP parser (Lexer.mkPState dflags buf loc)

{- | True if specified package is present in DynFlags

-- >>> hasPackageTst pkg = run $ \df -> return (hasPackage df pkg)
>>> hasPackageTst pkg = run $ \_ -> addPackages [pkg] >>= return . either Left (\df -> Right (hasPackage df pkg))

>>> hasPackageTst "base"
Right True

>>> hasPackageTst "ghc"
Right True

>>> hasPackageTst "extra"
Left "<command line>: cannot satisfy -package extra\n    (use -v for more information)"

>>> hasPackageTst "QuickCheck"
Left "<command line>: cannot satisfy -package QuickCheck\n    (use -v for more information)"
-}
hasPackage :: DynFlags -> String -> Bool
hasPackage df = hasPackage_ (packageFlags df)

hasPackage_ :: [PackageFlag] -> [Char] -> Bool
hasPackage_ pkgFlags name = any (name `isPrefixOf`) (pkgNames_ pkgFlags)

{- |
>>> run (return . pkgNames)
[]
-}
pkgNames :: DynFlags -> [String]
pkgNames = pkgNames_ . packageFlags

pkgNames_ :: [PackageFlag] -> [String]
pkgNames_ =
    mapMaybe
        ( \case
            ExposePackage _ (PackageArg n) _ -> Just n
            ExposePackage _ (UnitIdArg (DefiniteUnitId n)) _ -> Just $ asS n
            _ -> Nothing
        )

{- | Expose a list of packages.
>>> addPackagesTest pkgs = run (\_ -> (packageFlags <$>) <$> addPackages pkgs)

>>> addPackagesTest []
Right []

>>> addPackagesTest ["base","base","array"]
Right [-package base{package base True ([])},-package array{package array True ([])}]

>>> addPackagesTest ["Cabal"]
Right [-package Cabal{package Cabal True ([])}]

>>> addPackagesTest ["QuickCheck"]
Left "<command line>: cannot satisfy -package QuickCheck\n    (use -v for more information)"

>>> addPackagesTest ["base","notThere"]
Left "<command line>: cannot satisfy -package notThere\n    (use -v for more information)"

prop> \(x::Int) -> x + x == 2 * x
+++ OK, passed 100 tests.
-}
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

-- modifyFlags f = do
--         modifyDynFlags f
--         getSessionDynFlags

{- | Add import to evaluation context

>>> run $ \_ -> addImport "import Data.Maybe"
Could not find module ‘Data.Maybe’
Use -v (or `:set -v` in ghci) to see a list of the files searched for.

>>> run $ \df -> addPackages ["base"] >> addImport "import Data.Maybe"
[import Data.Maybe]

>>> run $ \df -> addPackages ["base"] >> addImport "import qualified Data.Maybe as M"
[import qualified Data.Maybe as M]
-}
addImport :: GhcMonad m => String -> m [InteractiveImport]
addImport i = do
    ctx <- getContext
    -- dbgO "CONTEXT" ctx
    idecl <- parseImportDecl i
    setContext $ IIDecl idecl : ctx
    -- ctx' <- getContext
    -- dbg "CONTEXT'" ctx'
    getContext

{- | Add extension to interactive evaluation session
>>> import GHC.LanguageExtensions.Type(Extension(..))
>>> run $ \_ -> addExtension DeriveGeneric
()
-}
addExtension :: GhcMonad m => Extension -> m ()
addExtension ext =
    modifySession $ \hsc -> hsc{hsc_IC = setExtension (hsc_IC hsc) ext}

setExtension :: InteractiveContext -> Extension -> InteractiveContext
setExtension ic ext = ic{ic_dflags = xopt_set (ic_dflags ic) ext}

deriving instance Read Extension

-- Partial display of DynFlags contents, for testing purposes
showDynFlags :: DynFlags -> String
showDynFlags df =
    showSDocUnsafe . vcat . map (\(n, d) -> text (n ++ ": ") <+> d) $
        [ ("extensions", ppr . extensions $ df)
        , ("extensionFlags", ppr . EnumSet.toList . extensionFlags $ df)
        , ("importPaths", vList $ importPaths df)
        , -- , ("includePaths", text . show $ includePaths df)
          -- ("packageEnv", ppr $ packageEnv df)
          ("pkgNames", vcat . map text $ pkgNames df)
        , ("packageFlags", vcat . map ppr $ packageFlags df)
        -- ,("pkgDatabase",(map) (ppr . installedPackageId) . pkgDatabase $ df)
        -- ("pkgDatabase", text . show <$> pkgDatabase $ df)
        ]

vList :: [String] -> SDoc
vList = vcat . map text
