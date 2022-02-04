{-# LANGUAGE RankNTypes #-}
-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE CPP        #-}

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
import           Data.List.Extra
import qualified Data.Map                        as M
import           Data.Maybe
import qualified Data.Set                        as S
import qualified Data.Text                       as T
import           Development.IDE.Core.Compile
import           Development.IDE.Core.RuleTypes
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.Error
import           Development.IDE.Spans.Common
import           System.Directory
import           System.FilePath

import           Language.LSP.Types             (filePathToUri, getUri)
import qualified Data.Map                       as Map

mkDocMap
  :: HscEnv
  -> RefMap a
  -> TcGblEnv
  -> IO DocAndKindMap
mkDocMap env rm this_mod =
  do
#if MIN_VERSION_ghc(9,2,0)
     (_ , DeclDocMap this_docs, _) <- extractDocs this_mod
#else
     let (_ , DeclDocMap this_docs, _) = extractDocs this_mod
#endif
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
--  2021-11-17: FIXME: Code uses batch search for singleton & assumes that search always succeeds.
getDocumentationTryGhc env mod n = fromJust . Map.lookup n <$> getDocumentationsTryGhc env mod [n]

getDocumentationsTryGhc :: HscEnv -> Module -> [Name] -> IO (Map.Map Name SpanDoc)
getDocumentationsTryGhc env mod names = do
  res <- fun
  case res of
    Left _    -> return mempty -- catchSrcErrors (hsc_dflags env) "docs"
    Right res -> sequenceA $ Map.mapWithKey unwrap res
  where
    fun :: IO (Either ErrorMessages (Map.Map Name (Either T.Text (Maybe HsDocString, Maybe (Map.Map Int HsDocString)))))
    fun = getDocsBatch env mod names

    unwrap :: Name -> Either a (Maybe HsDocString, b) -> IO SpanDoc
    unwrap name a = extractDocString a <$> getSpanDocUris name
     where
      extractDocString :: Either b1 (Maybe HsDocString, b2) -> SpanDocUris -> SpanDoc
      --  2021-11-17: FIXME: ArgDocs get dropped here - instead propagate them.
      extractDocString (Right (Just docs, _)) = SpanDocString docs
      extractDocString _ = SpanDocText mempty

      -- | Get the uris to the documentation and source html pages if they exist
      getSpanDocUris :: Name -> IO SpanDocUris
      getSpanDocUris name = do
        (docFu, srcFu) <-
            case nameModule_maybe name of
            Just mod -> liftIO $ do
                doc <- toFileUriText $ lookupDocHtmlForModule env mod
                src <- toFileUriText $ lookupSrcHtmlForModule env mod
                return (doc, src)
            Nothing -> pure mempty
        let docUri = (<> "#" <> selector <> showNameWithoutUniques name) <$> docFu
            srcUri = (<> "#" <> showNameWithoutUniques name) <$> srcFu
            selector
                | isValName name = "v:"
                | otherwise = "t:"
        return $ SpanDocUris docUri srcUri
       where
        toFileUriText :: IO (Maybe FilePath) -> IO (Maybe T.Text)
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
-- TODO : Implement this for GHC 9.2 with in-tree annotations
--        (alternatively, just remove it and rely soley on GHC's parsing)
getDocumentation sources targetName = fromMaybe [] $ do
#if MIN_VERSION_ghc(9,2,0)
  Nothing
#else
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
      $ fold
      docs
  where
    -- Get the name bound by a binding. We only concern ourselves with
    -- @FunBind@ (which covers functions and variables).
    name_of_bind :: HsBind GhcPs -> Maybe (Located RdrName)
    name_of_bind FunBind {fun_id} = Just fun_id
    name_of_bind _                = Nothing
    -- Get source spans from names, discard unhelpful spans, remove
    -- duplicates and sort.
    sortedNameSpans :: [Located RdrName] -> [RealSrcSpan]
    sortedNameSpans ls = nubSort (mapMaybe (realSpan . getLoc) ls)
    isBetween target before after = before <= target && target <= after
#if MIN_VERSION_ghc(9,0,0)
    ann = apiAnnComments . pm_annotations
#else
    ann = fmap filterReal . snd . pm_annotations
    filterReal :: [Located a] -> [RealLocated a]
    filterReal = mapMaybe (\(L l v) -> (`L`v) <$> realSpan l)
#endif
    annotationFileName :: ParsedModule -> Maybe FastString
    annotationFileName = fmap srcSpanFile . listToMaybe . map getRealSrcSpan . fold . ann

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
#endif

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
      chunks <- (reverse . drop1 . inits . splitOn ".") $ (moduleNameString . moduleName) m
      -- The file might use "." or "-" as separator
      map (`intercalate` chunks) [".", "-"]

lookupHtmls :: HscEnv -> Unit -> Maybe [FilePath]
lookupHtmls df ui =
  -- use haddockInterfaces instead of haddockHTMLs: GHC treats haddockHTMLs as URL not path
  -- and therefore doesn't expand $topdir on Windows
  map takeDirectory . unitHaddockInterfaces <$> lookupUnit df ui
