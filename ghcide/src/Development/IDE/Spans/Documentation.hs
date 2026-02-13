-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}

module Development.IDE.Spans.Documentation (
    getDocumentation
  , getDocumentationTryGhc
  , getDocumentationsTryGhc
  , DocMap
  , mkDocMap
  ) where

import           Control.Monad
import           Control.Monad.Extra             (findM)
import           Control.Monad.IO.Class
import           Data.Either
import           Data.Foldable
import           Data.IntMap                     (IntMap)
import           Data.List.Extra
import qualified Data.Map                        as M
import           Data.Maybe
import qualified Data.Set                        as S
import qualified Data.Text                       as T
import           Data.Version                    (showVersion)
import           Development.IDE.Core.Compile
import           Development.IDE.Core.RuleTypes
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.Error
import           Development.IDE.GHC.Util        (printOutputable)
import           Development.IDE.Spans.Common
import           Development.IDE.Types.Options   (LinkTargets (..))
import           GHC.Iface.Ext.Utils             (RefMap)
import           GHC.Plugins                     (GenericUnitInfo (unitPackageName))
import           Ide.Types                       (OptLinkTo (..))
import           Language.LSP.Protocol.Types     (Uri (..), filePathToUri,
                                                  getUri)
import           Prelude                         hiding (mod)
import           System.Directory
import           System.FilePath


mkDocMap
  :: HscEnv
  -> RefMap a
  -> TcGblEnv
  -> LinkTargets
  -> IO DocAndTyThingMap
mkDocMap env rm this_mod linkTgts =
  do
     (Just Docs{docs_decls = UniqMap this_docs, docs_args = UniqMap this_arg_docs}) <- extractDocs (hsc_dflags env) this_mod
     d <- foldrM getDocs (fmap (\(_, x) -> (map hsDocString x) `SpanDocString` SpanDocUris Nothing Nothing) this_docs) names
     k <- foldrM getType (tcg_type_env this_mod) names
     a <- foldrM getArgDocs (fmap (\(_, m) -> fmap (\x -> [hsDocString x] `SpanDocString` SpanDocUris Nothing Nothing) m) this_arg_docs) names
     pure $ DKMap d k a
  where
    getDocs n nameMap
      | maybe True (mod ==) $ nameModule_maybe n = pure nameMap -- we already have the docs in this_docs, or they do not exist
      | otherwise = do
      (doc, _argDoc) <- getDocumentationTryGhc env linkTgts n
      pure $ extendNameEnv nameMap n doc
    getType n nameMap
      | Nothing <- lookupNameEnv nameMap n
      = do kind <- lookupKind env n
           pure $ maybe nameMap (extendNameEnv nameMap n) kind
      | otherwise = pure nameMap
    getArgDocs n nameMap
      | maybe True (mod ==) $ nameModule_maybe n = pure nameMap
      | otherwise = do
      (_doc, argDoc) <- getDocumentationTryGhc env linkTgts n
      pure $ extendNameEnv nameMap n argDoc
    names = rights $ S.toList idents
    idents = M.keysSet rm
    mod = tcg_mod this_mod

lookupKind :: HscEnv -> Name -> IO (Maybe TyThing)
lookupKind env =
    fmap (fromRight Nothing) . catchSrcErrors (hsc_dflags env) "span" . lookupName env

getDocumentationTryGhc :: HscEnv -> LinkTargets -> Name -> IO (SpanDoc, IntMap SpanDoc)
getDocumentationTryGhc env l2h n =
  (fromMaybe (emptySpanDoc, mempty) . listToMaybe <$> getDocumentationsTryGhc env l2h [n])
    `catch` (\(_ :: IOEnvFailure) -> pure (emptySpanDoc, mempty))

getDocumentationsTryGhc :: HscEnv -> LinkTargets -> [Name] -> IO [(SpanDoc, IntMap SpanDoc)]
getDocumentationsTryGhc env linkTgts names = do
  resOr <- catchSrcErrors (hsc_dflags env) "docs" $ getDocsBatch env names
  case resOr of
      Left _    -> return []
      Right res -> zipWithM unwrap res names
  where
    unwrap (Right (Just docs, argDocs)) n = (\uris -> (SpanDocString (map hsDocString docs) uris, fmap (\x -> SpanDocString [hsDocString x] uris) argDocs)) <$> getUris n
    unwrap _ n                      = mkSpanDocText n

    mkSpanDocText name =
      (\uris -> (SpanDocText [] uris, mempty)) <$> getUris name

    -- Get the uris to the documentation and source html pages if they exist
    getUris name = do
      (docFu, srcFu) <-
        case nameModule_maybe name of
          Just mod -> liftIO $ do
            doc <- lookupDocHtmlForModule env mod
            src <- lookupSrcHtmlForModule env mod
            -- If found, the local files are used as hints for the hackage links, this helps with symbols defined in an internal module but re-exported by another.
            let
              LinkTargets{linkDoc,linkSource} = linkTgts
              doc_link = case linkDoc of
                LinkToHackage ->
                  toHackageDocUriText env mod (takeFileName <$> doc)
                LinkToLocal ->
                  toFileUriText doc
              src_link = case linkSource of
                LinkToHackage ->
                  toHackageSrcUriText env mod (takeFileName <$> src)
                LinkToLocal ->
                  toFileUriText src
            pure (doc_link, src_link)
          Nothing -> pure (Nothing, Nothing)

      let docUri = (<> "#" <> selector <> printOutputable name) <$> docFu
          srcUri = (<> "#" <> printOutputable name) <$> srcFu
          selector
            | isValName name = "v:"
            | otherwise = "t:"
      return $ SpanDocUris docUri srcUri

    toFileUriText = fmap (getUri . filePathToUri)
    toHackageUriText subdir sep env mod hint = do
     ui <- lookupUnit env (moduleUnit mod)
     let htmlFile = case hint of
           Nothing -> T.intercalate sep (map T.pack $ moduleNameChunks mod) <> ".html"
           Just foundFile -> T.replace "-" sep $ T.pack foundFile
     pure $!
      mconcat $
      [ "http://hackage.haskell.org/package/"
      , printOutputable (unitPackageName ui), "-", T.pack $ showVersion (unitPackageVersion ui), "/"
      , subdir , "/"
      , htmlFile
      ]
    toHackageDocUriText mod = toHackageUriText "docs" "-" mod
    toHackageSrcUriText mod = toHackageUriText "docs/src" "." mod

getDocumentation
 :: HasSrcSpan name
 => [ParsedModule] -- ^ All of the possible modules it could be defined in.
 ->  name -- ^ The name you want documentation for.
 -> [T.Text]
getDocumentation _sources _targetName = []

-- These are taken from haskell-ide-engine's Haddock plugin

-- | Given a module finds the local @doc/html/Foo-Bar-Baz.html@ page.
-- An example for a cabal installed module:
-- @~/.cabal/store/ghc-8.10.1/vctr-0.12.1.2-98e2e861/share/doc/html/Data-Vector-Primitive.html@
lookupDocHtmlForModule :: HscEnv -> Module -> IO (Maybe FilePath)
lookupDocHtmlForModule =
  lookupHtmlForModule (\pkgDocDir modDocName -> pkgDocDir </> modDocName <.> "html")

-- | Given a module finds the hyperlinked source @doc/html/src/Foo.Bar.Baz.html@ page.
-- An example for a cabal installed module:
-- @~/.cabal/store/ghc-8.10.1/vctr-0.12.1.2-98e2e861/share/doc/html/src/Data.Vector.Primitive.html@
lookupSrcHtmlForModule :: HscEnv -> Module -> IO (Maybe FilePath)
lookupSrcHtmlForModule =
  lookupHtmlForModule (\pkgDocDir modDocName -> pkgDocDir </> "src" </> modDocName <.> "html")

lookupHtmlForModule :: (FilePath -> FilePath -> FilePath) -> HscEnv -> Module -> IO (Maybe FilePath)
lookupHtmlForModule mkDocPath hscEnv m = do
  -- try all directories
  let mfs = fmap (concatMap go) (lookupHtmls hscEnv ui)
  html <- findM doesFileExist (concat . maybeToList $ mfs)
  -- canonicalize located html to remove /../ indirection which can break some clients
  -- (vscode on Windows at least)
  traverse canonicalizePath html
  where
    go pkgDocDir = map (mkDocPath pkgDocDir) mns
    ui = moduleUnit m
    -- try to locate html file from most to least specific name e.g.
    --  first Language.LSP.Types.Uri.html and Language-Haskell-LSP-Types-Uri.html
    --  then Language.LSP.Types.html and Language-Haskell-LSP-Types.html etc.
    mns = do
      chunks <- (reverse . drop1 . inits) $ moduleNameChunks m
      -- The file might use "." or "-" as separator
      map (`intercalate` chunks) [".", "-"]

moduleNameChunks :: Module -> [String]
moduleNameChunks m = splitOn "." $ (moduleNameString . moduleName) m

lookupHtmls :: HscEnv -> Unit -> Maybe [FilePath]
lookupHtmls df ui =
  -- use haddockInterfaces instead of haddockHTMLs: GHC treats haddockHTMLs as URL not path
  -- and therefore doesn't expand $topdir on Windows
  map takeDirectory . unitHaddockInterfaces <$> lookupUnit df ui
