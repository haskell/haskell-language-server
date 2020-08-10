{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Ide.Plugin.ImportLens (descriptor) where
import           Control.Monad                  (forM)
import           Data.Aeson                     (ToJSON)
import           Data.Aeson                     (Value (Null))
import           Data.Aeson                     (ToJSON (toJSON))
import           Data.Aeson.Types               (FromJSON)
import qualified Data.HashMap.Strict            as HashMap
import           Data.IORef                     (readIORef)
import           Data.Map                       (Map)
import qualified Data.Map.Strict                as Map
import           Data.Maybe                     (catMaybes, fromMaybe)
import qualified Data.Text                      as T
import           Development.IDE.Core.RuleTypes (GhcSessionDeps (GhcSessionDeps),
                                                 TcModuleResult (tmrModule),
                                                 TypeCheck (TypeCheck))
import           Development.IDE.Core.Service   (IdeState, runAction)
import           Development.IDE.Core.Shake     (IdeAction, IdeState (..),
                                                 runIdeAction, useWithStaleFast,
                                                 use_)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error      (realSpan, realSrcSpanToRange)
import           Development.IDE.GHC.Util       (hscEnv, prettyPrint)
import           GHC.Generics                   (Generic)
import           Ide.Plugin
import           Ide.Types
import           Language.Haskell.LSP.Types
import           PrelNames                      (pRELUDE)
import           RnNames                        (findImportUsage,
                                                 getMinimalImports)
import           TcRnMonad                      (initTcWithGbl)
import           TcRnTypes                      (TcGblEnv (tcg_used_gres))

importCommandId :: CommandId
importCommandId = "ImportLensCommand"

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId) {
    pluginCodeLensProvider = Just provider,
    pluginCommands = [ importLensCommand ]
}

importLensCommand :: PluginCommand
importLensCommand =
    PluginCommand importCommandId "Explicit import command" runImportCommand

data ImportCommandParams = ImportCommandParams WorkspaceEdit
  deriving Generic
  deriving anyclass (FromJSON, ToJSON)

runImportCommand :: CommandFunction ImportCommandParams
runImportCommand _lspFuncs _state (ImportCommandParams edit) = do
    return (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams edit))

-- For every implicit import statement,
-- return a code lens of the corresponding explicit import
-- Example. For the module below:
--
-- > import Data.List
-- >
-- > f = intercalate " " . sortBy length
--
-- the provider should produce one code lens:
--
-- > import Data.List (intercalate, sortBy)

provider :: CodeLensProvider
provider _lspFuncs state pId CodeLensParams{..}
  | TextDocumentIdentifier{_uri} <- _textDocument
  , Just nfp <- uriToNormalizedFilePath $ toNormalizedUri _uri
  = do
    Just (tmr, _) <- runIde state $ useWithStaleFast TypeCheck nfp
    hsc <- hscEnv <$> runAction "importLens" state (use_ GhcSessionDeps nfp)
    (imports, mbMinImports) <- extractMinimalImports hsc (tmrModule tmr)

    case mbMinImports of
        Just minImports -> do
            let minImportsMap =
                    Map.fromList [ (srcSpanStart l, i) | L l i <- minImports ]
            commands <- forM imports $ generateLens pId _uri minImportsMap
            return $ Right (List $ catMaybes commands)
        _ ->
            return $ Right (List [])

  | otherwise
  = return $ Right (List [])

extractMinimalImports :: HscEnv -> TypecheckedModule -> IO ([LImportDecl GhcRn], Maybe [LImportDecl GhcRn])
extractMinimalImports hsc TypecheckedModule{..} = do
    let (tcEnv,_) = tm_internals_
        Just (_, imports, _, _) = tm_renamed_source
        ParsedModule{ pm_parsed_source = L loc _} = tm_parsed_module

    gblElts <- readIORef (tcg_used_gres tcEnv)
    let usage = findImportUsage imports gblElts
        span = fromMaybe (error "expected real") $ realSpan loc
    (_, minimalImports) <- initTcWithGbl hsc tcEnv span $ getMinimalImports usage
    return (imports, minimalImports)

generateLens :: PluginId -> Uri -> Map SrcLoc (ImportDecl GhcRn) -> LImportDecl GhcRn -> IO (Maybe CodeLens)
generateLens pId uri minImports (L src imp)
  | ImportDecl{ideclHiding = Just (False,_)} <- imp
  = return Nothing
  | RealSrcSpan l <- src
  , Just explicit <- Map.lookup (srcSpanStart src) minImports
  , L _ mn <- ideclName imp
  , mn /= moduleName pRELUDE
  = do
    let title = T.pack $ prettyPrint explicit
        commandArgs = Nothing
    c <- mkLspCommand pId importCommandId title commandArgs
    let _range :: Range = realSrcSpanToRange l
        _xdata = Nothing
        edit = WorkspaceEdit (Just editsMap) Nothing
        editsMap = HashMap.fromList [(uri, List [importEdit])]
        importEdit = TextEdit _range title
        args = ImportCommandParams edit
        _arguments = Just (List [toJSON args])
        _command = Just (c :: Command){_arguments}
    return $ Just CodeLens{..}
  | otherwise
  = return Nothing

runIde :: IdeState -> IdeAction a -> IO a
runIde state = runIdeAction "importLens" (shakeExtras state)
