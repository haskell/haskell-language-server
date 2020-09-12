{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Ide.Plugin.Munihac where

import Development.IDE
import Ide.Types
import Language.Haskell.LSP.Types
import Development.IDE.GHC.Compat
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe
import Data.IORef
import TcRnMonad
import RnNames
import qualified Data.Map as Map
import GHC.Generics
import Data.Hashable
import Development.Shake.Classes
import Control.DeepSeq (rwhnf)
import qualified Data.HashMap.Strict as HashMap

descriptor :: PluginId -> PluginDescriptor
descriptor pid = (defaultPluginDescriptor pid){
    pluginCodeLensProvider = Just lensProvider,
    pluginCodeActionProvider = Just codeActionProvider
}

data GetMinimalImports = GetMinimalImports
  deriving (Show, Generic, Eq, Ord)
instance Hashable GetMinimalImports
instance NFData   GetMinimalImports
instance Binary   GetMinimalImports
type instance RuleResult GetMinimalImports = MinimalImports

newtype MinimalImports =
    MinimalImports [(LImportDecl GhcRn, Maybe (LImportDecl GhcRn))]

instance NFData MinimalImports where rnf = rwhnf
instance Show MinimalImports where show _ = "<minimal imports>"

minimalImportsRule :: Rules ()
minimalImportsRule = define $ \GetMinimalImports file -> do
    res <- findMinimalImportsActionImpl file
    return ( [], Just res )

codeActionProvider :: CodeActionProvider
codeActionProvider _lspFuncs _state _pid docId range _context
  | TextDocumentIdentifier{_uri} <- docId
  , Just nfp <- fmap toNormalizedFilePath' $ uriToFilePath' _uri
  = do
      MinimalImports minimalImports <-
        runAction "munihac.codeaction" _state $ maybe (MinimalImports []) fst <$> useWithStale GetMinimalImports nfp
      let firstImportLine = getStartLine . fst =<< headMaybe (minimalImports)
          lastImportLine = getStartLine . fst =<< lastMaybe (minimalImports)
          isInsideImports = case (firstImportLine, lastImportLine) of
              (Just fi, Just la) -> firstLine range >= fi && firstLine range <= la
              _ -> False
          hasRelevantImports = not $ null $ filter (isRelevant . unLoc . fst) minimalImports
      -- If the range is inside an import declaration
      -- and there is at least one relevant import
      -- return one code action to fix them all

          edit = WorkspaceEdit (Just $ HashMap.fromList [(_uri, List edits)]) Nothing
          edits = [ TextEdit range newText
                  | (L (RealSrcSpan l) _import, Just minimalImport) <- minimalImports
                  , let range = realSrcSpanToRange l
                  , let newText = T.pack $ prettyPrint minimalImport]

          ca = CACodeAction $ CodeAction "title" Nothing Nothing (Just edit) Nothing
      return $ Right $ List [ca | isInsideImports && hasRelevantImports]
codeActionProvider _ _ _ _ _ _ = return $ Right $ List []

firstLine :: Range -> Int
firstLine (Range (Position line _) _) = line

lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe x = Just $ last x

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x:_) = Just x

getStartLine :: GenLocated SrcSpan e -> Maybe Int
getStartLine (L (RealSrcSpan l) _) = Just $ srcSpanStartLine l
getStartLine _ = Nothing

lensProvider :: CodeLensProvider
lensProvider _lspFuncs state _pid CodeLensParams{_textDocument}
  | TextDocumentIdentifier{_uri} <- _textDocument
  , Just nfp <- fmap toNormalizedFilePath' $ uriToFilePath' _uri
  = do
    -- Obtain the list of imports
    MinimalImports imports <- runAction "munihac" state $ maybe (MinimalImports []) fst <$> useWithStale GetMinimalImports nfp
    let relevantImportLenses =
            [ mkCodeLens (realSrcSpanToRange l)
                         (mkCommand $ T.pack $ prettyPrint minimalImport)
            | (L (RealSrcSpan l) imp, Just minimalImport) <- imports
            -- For every import statement without an import list
            , isRelevant imp
            ]
    -- Find out the explicit import lists (GHC)
    -- Generate a code lens
    return $ Right $ List relevantImportLenses
lensProvider _ _ _ _ = return $ Right $ List []

-- | An import declaration is relevant if:
--     - it has no import list
--     - AND is not a qualified import
--     - AND is not the Prelude
isRelevant :: ImportDecl GhcRn -> Bool
isRelevant ImportDecl{..}
  = maybe True fst ideclHiding
  && not ideclImplicit
  && ideclQualified == NotQualified
isRelevant _ = False

-- | Returns the renamed imports for a module
getImportsAction :: NormalizedFilePath -> Action [LImportDecl GhcRn]
getImportsAction nfp = do
  res <- useWithStale TypeCheck nfp
  case res of
      Just (TcModuleResult{tmrModule = TypecheckedModule{..}}, _)
        | Just (_,imports,_,_) <- tm_renamed_source
        -> return imports
      _ -> return []

mkCodeLens :: Range -> Command -> CodeLens
mkCodeLens range c = CodeLens {
            _range = range,
            _command = Just c,
            _xdata = Nothing
        }

mkCommand :: Text -> Command
mkCommand title = Command title "non-existent-command-id" Nothing

findMinimalImportsActionImpl :: NormalizedFilePath -> Action MinimalImports
findMinimalImportsActionImpl nfp = do
    tcResult <- use TypeCheck nfp
    ghcSession <- use GhcSessionDeps nfp
    liftIO $ findMinimalImports ghcSession tcResult

findMinimalImports
    :: Maybe HscEnvEq
    -> Maybe TcModuleResult
    -> IO MinimalImports
findMinimalImports hsc tmrModule = do
    (imports, minimalImports) <- extractMinimalImports hsc tmrModule
    let minimalImportsMap = Map.fromList
         [ ( startPos, minimalImport)
         | minimalImport@(L (RealSrcSpan l) _) <- fromMaybe [] minimalImports
         , let startPos = srcSpanStartLine l
         ]
    return $ MinimalImports
           [ (import_, Map.lookup startLine minimalImportsMap)
           | import_@(L (RealSrcSpan l) _) <- imports
           , let startLine = srcSpanStartLine l]


-- | Use the ghc api to extract a minimal, explicit set of imports for this module
extractMinimalImports
  :: Maybe (HscEnvEq)
  -> Maybe (TcModuleResult)
  -> IO ([LImportDecl GhcRn], Maybe [LImportDecl GhcRn])
extractMinimalImports (Just (hsc)) (Just (tmrModule -> TypecheckedModule{..})) = do
    -- extract the original imports and the typechecking environment
    let (tcEnv,_) = tm_internals_
        Just (_, imports, _, _) = tm_renamed_source
        ParsedModule{ pm_parsed_source = L loc _} = tm_parsed_module
        span = fromMaybe (error "expected real") $ realSpan loc

    -- GHC is secretly full of mutable state
    gblElts <- readIORef (tcg_used_gres tcEnv)

    -- call findImportUsage does exactly what we need
    -- GHC is full of treats like this
    let usage = findImportUsage imports gblElts
    (_, minimalImports) <- initTcWithGbl (hscEnv hsc) tcEnv span $ getMinimalImports usage

    -- return both the original imports and the computed minimal ones
    return (imports, minimalImports)

extractMinimalImports _ _ = return ([], Nothing)
