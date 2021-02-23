{-# LANGUAGE RankNTypes #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

module Development.IDE.Spans.Documentation (
    getDocumentation
  , getDocumentationTryGhc
  , getDocumentationsTryGhc
  , DocMap
  , mkDocMap
  ) where

import           Control.Monad
import           Control.Monad.Extra (findM)
import           Data.Either
import           Data.Foldable
import           Data.List.Extra
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.Maybe
import qualified Data.Text as T
import           Development.IDE.Core.Compile
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error
import           Development.IDE.Spans.Common
import           Development.IDE.Core.RuleTypes
import           System.Directory
import           System.FilePath

import           FastString
import           SrcLoc (RealLocated)
import           GhcMonad
import           Packages
import           Name
import           Language.LSP.Types (getUri, filePathToUri)
import           TcRnTypes
import           ExtractDocs
import           NameEnv
import HscTypes (HscEnv(hsc_dflags))

mkDocMap
  :: HscEnv
  -> RefMap a
  -> TcGblEnv
  -> IO DocAndKindMap
mkDocMap env rm this_mod =
  do let (_ , DeclDocMap this_docs, _) = extractDocs this_mod
     d <- foldrM getDocs (mkNameEnv $ M.toList $ fmap (`SpanDocString` SpanDocUris Nothing Nothing) this_docs) names
     k <- foldrM getType (tcg_type_env this_mod) names
     pure $ DKMap d k
  where
    getDocs n map
      | maybe True (mod ==) $ nameModule_maybe n = pure map -- we already have the docs in this_docs, or they do not exist
      | otherwise = do
      doc <- getDocumentationTryGhc env mod n
      pure $ extendNameEnv map n doc
    getType n map
      | isTcOcc $ occName n = do
        kind <- lookupKind env mod n
        pure $ maybe map (extendNameEnv map n) kind
      | otherwise = pure map
    names = rights $ S.toList idents
    idents = M.keysSet rm
    mod = tcg_mod this_mod

lookupKind :: HscEnv -> Module -> Name -> IO (Maybe TyThing)
lookupKind env mod =
    fmap (fromRight Nothing) . catchSrcErrors (hsc_dflags env) "span" . lookupName env mod

getDocumentationTryGhc :: HscEnv -> Module -> Name -> IO SpanDoc
getDocumentationTryGhc env mod n = head <$> getDocumentationsTryGhc env mod [n]

getDocumentationsTryGhc :: HscEnv -> Module -> [Name] -> IO [SpanDoc]
getDocumentationsTryGhc env mod names = do
  res <- catchSrcErrors (hsc_dflags env) "docs" $ getDocsBatch env mod names
  case res of
      Left _ -> return []
      Right res -> zipWithM unwrap res names
  where
    unwrap (Right (Just docs, _)) n = SpanDocString docs <$> getUris n
    unwrap _ n = mkSpanDocText n

    mkSpanDocText name =
      SpanDocText [] <$> getUris name

    -- Get the uris to the documentation and source html pages if they exist
    getUris name = do
      let df = hsc_dflags env
      (docFu, srcFu) <-
        case nameModule_maybe name of
          Just mod -> liftIO $ do
            doc <- toFileUriText $ lookupDocHtmlForModule df mod
            src <- toFileUriText $ lookupSrcHtmlForModule df mod
            return (doc, src)
          Nothing -> pure (Nothing, Nothing)
      let docUri = (<> "#" <> selector <> showNameWithoutUniques name) <$> docFu
          srcUri = (<> "#" <> showNameWithoutUniques name) <$> srcFu
          selector
            | isValName name = "v:"
            | otherwise = "t:"
      return $ SpanDocUris docUri srcUri

    toFileUriText = (fmap . fmap) (getUri . filePathToUri)

getDocumentation
 :: HasSrcSpan name
 => [ParsedModule] -- ^ All of the possible modules it could be defined in.
 ->  name -- ^ The name you want documentation for.
 -> [T.Text]
-- This finds any documentation between the name you want
-- documentation for and the one before it. This is only an
-- approximately correct algorithm and there are easily constructed
-- cases where it will be wrong (if so then usually slightly but there
-- may be edge cases where it is very wrong).
-- TODO : Build a version of GHC exactprint to extract this information
-- more accurately.
getDocumentation sources targetName = fromMaybe [] $ do
  -- Find the module the target is defined in.
  targetNameSpan <- realSpan $ getLoc targetName
  tc <-
    find ((==) (Just $ srcSpanFile targetNameSpan) . annotationFileName)
      $ reverse sources -- TODO : Is reversing the list here really neccessary?

  -- Top level names bound by the module
  let bs = [ n | let L _ HsModule{hsmodDecls} = pm_parsed_source tc
           , L _ (ValD _ hsbind) <- hsmodDecls
           , Just n <- [name_of_bind hsbind]
           ]
  -- Sort the names' source spans.
  let sortedSpans = sortedNameSpans bs
  -- Now go ahead and extract the docs.
  let docs = ann tc
  nameInd <- elemIndex targetNameSpan sortedSpans
  let prevNameSpan =
        if nameInd >= 1
        then sortedSpans !! (nameInd - 1)
        else zeroSpan $ srcSpanFile targetNameSpan
  -- Annoyingly "-- |" documentation isn't annotated with a location,
  -- so you have to pull it out from the elements.
  pure
      $ docHeaders
      $ filter (\(L target _) -> isBetween target prevNameSpan targetNameSpan)
      $ mapMaybe (\(L l v) -> L <$> realSpan l <*> pure v)
      $ join
      $ M.elems
      docs
  where
    -- Get the name bound by a binding. We only concern ourselves with
    -- @FunBind@ (which covers functions and variables).
    name_of_bind :: HsBind GhcPs -> Maybe (Located RdrName)
    name_of_bind FunBind {fun_id} = Just fun_id
    name_of_bind _ = Nothing
    -- Get source spans from names, discard unhelpful spans, remove
    -- duplicates and sort.
    sortedNameSpans :: [Located RdrName] -> [RealSrcSpan]
    sortedNameSpans ls = nubSort (mapMaybe (realSpan . getLoc) ls)
    isBetween target before after = before <= target && target <= after
    ann = snd . pm_annotations
    annotationFileName :: ParsedModule -> Maybe FastString
    annotationFileName = fmap srcSpanFile . listToMaybe . realSpans . ann
    realSpans :: M.Map SrcSpan [Located a] -> [RealSrcSpan]
    realSpans =
        mapMaybe (realSpan . getLoc)
      . join
      . M.elems

-- | Shows this part of the documentation
docHeaders :: [RealLocated AnnotationComment]
           -> [T.Text]
docHeaders = mapMaybe (\(L _ x) -> wrk x)
  where
  wrk = \case
    -- When `Opt_Haddock` is enabled.
    AnnDocCommentNext s -> Just $ T.pack s
    -- When `Opt_KeepRawTokenStream` enabled.
    AnnLineComment s  -> if "-- |" `isPrefixOf` s
                            then Just $ T.pack s
                            else Nothing
    _ -> Nothing

-- These are taken from haskell-ide-engine's Haddock plugin

-- | Given a module finds the local @doc/html/Foo-Bar-Baz.html@ page.
-- An example for a cabal installed module:
-- @~/.cabal/store/ghc-8.10.1/vctr-0.12.1.2-98e2e861/share/doc/html/Data-Vector-Primitive.html@
lookupDocHtmlForModule :: DynFlags -> Module -> IO (Maybe FilePath)
lookupDocHtmlForModule =
  lookupHtmlForModule (\pkgDocDir modDocName -> pkgDocDir </> modDocName <.> "html")

-- | Given a module finds the hyperlinked source @doc/html/src/Foo.Bar.Baz.html@ page.
-- An example for a cabal installed module:
-- @~/.cabal/store/ghc-8.10.1/vctr-0.12.1.2-98e2e861/share/doc/html/src/Data.Vector.Primitive.html@
lookupSrcHtmlForModule :: DynFlags -> Module -> IO (Maybe FilePath)
lookupSrcHtmlForModule =
  lookupHtmlForModule (\pkgDocDir modDocName -> pkgDocDir </> "src" </> modDocName <.> "html")

lookupHtmlForModule :: (FilePath -> FilePath -> FilePath) -> DynFlags -> Module -> IO (Maybe FilePath)
lookupHtmlForModule mkDocPath df m = do
  -- try all directories
  let mfs = fmap (concatMap go) (lookupHtmls df ui)
  html <- findM doesFileExist (concat . maybeToList $ mfs)
  -- canonicalize located html to remove /../ indirection which can break some clients
  -- (vscode on Windows at least)
  traverse canonicalizePath html
  where
    go pkgDocDir = map (mkDocPath pkgDocDir) mns
    ui = moduleUnitId m
    -- try to locate html file from most to least specific name e.g.
    --  first Language.LSP.Types.Uri.html and Language-Haskell-LSP-Types-Uri.html
    --  then Language.LSP.Types.html and Language-Haskell-LSP-Types.html etc.
    mns = do
      chunks <- (reverse . drop1 . inits . splitOn ".") $ (moduleNameString . moduleName) m
      -- The file might use "." or "-" as separator
      map (`intercalate` chunks) [".", "-"]

lookupHtmls :: DynFlags -> UnitId -> Maybe [FilePath]
lookupHtmls df ui =
  -- use haddockInterfaces instead of haddockHTMLs: GHC treats haddockHTMLs as URL not path
  -- and therefore doesn't expand $topdir on Windows
  map takeDirectory . haddockInterfaces <$> lookupPackage df ui
