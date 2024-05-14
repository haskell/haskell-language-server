{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

{-# OPTIONS_GHC -Wwarn -fno-warn-type-defaults #-}

{- | Keep the module name in sync with its file path.

Provide CodeLenses to:
* Add a module header ("module /moduleName/ where") to empty Haskell files
* Fix the module name if incorrect
-}
module Ide.Plugin.ModuleName (
    descriptor,
    Log,
) where

import           Control.Monad                        (forM_, void)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Data.Aeson                           (toJSON)
import           Data.Char                            (isLower, isUpper)
import           Data.List                            (intercalate, minimumBy,
                                                       stripPrefix)
import qualified Data.List.NonEmpty                   as NE
import qualified Data.Map                             as Map
import           Data.Maybe                           (mapMaybe)
import           Data.Ord                             (comparing)
import           Data.String                          (IsString)
import qualified Data.Text                            as T
import           Development.IDE                      (GetParsedModule (GetParsedModule),
                                                       GhcSession (GhcSession),
                                                       IdeState, Pretty,
                                                       Priority (Debug),
                                                       Recorder, WithPriority,
                                                       colon, evalGhcEnv,
                                                       hscEnvWithImportPaths,
                                                       logWith,
                                                       realSrcSpanToRange,
                                                       rootDir, runAction,
                                                       useWithStale, (<+>))
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping (toCurrentRange)
import           Development.IDE.GHC.Compat           (GenLocated (L),
                                                       getSessionDynFlags,
                                                       hsmodName, importPaths,
                                                       locA, moduleNameString,
                                                       pattern RealSrcSpan,
                                                       pm_parsed_source, unLoc)
import           Ide.Logger                           (Pretty (..))
import           Ide.Plugin.Error
import           Ide.Types
import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types
import           Language.LSP.Server
import           Language.LSP.VFS                     (virtualFileText)
import           System.FilePath                      (dropExtension,
                                                       isAbsolute, normalise,
                                                       pathSeparator,
                                                       splitDirectories,
                                                       takeFileName, (</>))

-- |Plugin descriptor
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId "Provides a code action to alter the module name if it is wrong")
        { pluginHandlers = mkPluginHandler SMethod_TextDocumentCodeLens (codeLens recorder)
        , pluginCommands = [PluginCommand updateModuleNameCommand "set name of module to match with file path" (command recorder)]
        }

updateModuleNameCommand :: IsString p => p
updateModuleNameCommand = "updateModuleName"

-- | Generate code lenses
codeLens :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentCodeLens
codeLens recorder state pluginId CodeLensParams{_textDocument=TextDocumentIdentifier uri} = do
  res <- action recorder state uri
  pure $ InL (asCodeLens  <$> res)
  where
    asCodeLens :: Action -> CodeLens
    asCodeLens Replace{..} = CodeLens aRange (Just cmd) Nothing
      where
        cmd = mkLspCommand pluginId updateModuleNameCommand aTitle (Just [toJSON aUri])

-- | (Quasi) Idempotent command execution: recalculate action to execute on command request
command :: Recorder (WithPriority Log) -> CommandFunction IdeState Uri
command recorder state _ uri = do
  actMaybe <- action recorder state uri
  forM_ actMaybe $ \Replace{..} ->
    let
      -- | Convert an Action to the corresponding edit operation
      edit = WorkspaceEdit (Just $ Map.singleton aUri [TextEdit aRange aCode]) Nothing Nothing
    in
      void $ lift $ sendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (const (pure ()))
  pure $ InR Null

-- | A source code change
data Action = Replace
  { aUri   :: Uri
  , aRange :: Range
  , aTitle :: T.Text
  , aCode  :: T.Text
  }
  deriving (Show)

-- | Required action (that can be converted to either CodeLenses or CodeActions)
action :: Recorder (WithPriority Log) -> IdeState -> Uri -> ExceptT PluginError (LspM c) [Action]
action recorder state uri = do
    nfp <- getNormalizedFilePathE  uri
    fp <- uriToFilePathE uri

    contents <- lift . getVirtualFile $ toNormalizedUri uri
    let emptyModule = maybe True (T.null . T.strip . virtualFileText) contents

    correctNames <- mapExceptT liftIO $ pathModuleNames recorder state nfp fp
    logWith recorder Debug (CorrectNames correctNames)
    let bestName = minimumBy (comparing T.length) <$> NE.nonEmpty correctNames
    logWith recorder Debug (BestName bestName)

    statedNameMaybe <- liftIO $ codeModuleName state nfp
    logWith recorder Debug (ModuleName $ snd <$> statedNameMaybe)
    case (bestName, statedNameMaybe) of
      (Just bestName, Just (nameRange, statedName))
        | statedName `notElem` correctNames ->
            pure [Replace uri nameRange ("Set module name to " <> bestName) bestName]
      (Just bestName, Nothing)
        | emptyModule ->
            let code = "module " <> bestName <> " where\n"
            in pure [Replace uri (Range (Position 0 0) (Position 0 0)) code code]
      _ -> pure []

toAbsolute :: FilePath -> FilePath -> FilePath
toAbsolute root path
    | isAbsolute path = path
    | otherwise = root </> path
-- | Possible module names, as derived by the position of the module in the
-- source directories.  There may be more than one possible name, if the source
-- directories are nested inside each other.
pathModuleNames :: Recorder (WithPriority Log) -> IdeState -> NormalizedFilePath -> FilePath -> ExceptT PluginError IO [T.Text]
pathModuleNames recorder state normFilePath filePath
  | firstLetter isLower $ takeFileName filePath = return ["Main"]
  | otherwise = do
      (session, _) <- runActionE "ModuleName.ghcSession" state $ useWithStaleE GhcSession normFilePath
      srcPaths <- liftIO $ evalGhcEnv (hscEnvWithImportPaths session) $ importPaths <$> getSessionDynFlags
      logWith recorder Debug (SrcPaths srcPaths)

      -- Append a `pathSeparator` to make the path looks like a directory,
      --   and then we can drop it uniformly.
      -- See https://github.com/haskell/haskell-language-server/pull/3092 for details.
      let paths = map (normalise . (<> pure pathSeparator)) srcPaths
      logWith recorder Debug (NormalisedPaths paths)

      let mdlPath = (toAbsolute $ rootDir state) filePath
      logWith recorder Debug (AbsoluteFilePath mdlPath)

      let suffixes = mapMaybe (`stripPrefix` mdlPath) paths
      pure (map moduleNameFrom suffixes)
  where
    firstLetter :: (Char -> Bool) -> FilePath -> Bool
    firstLetter _ []       = False
    firstLetter pred (c:_) = pred c

    moduleNameFrom =
      T.pack
        . intercalate "."
        -- Do not suggest names whose components start from a lower-case char,
        -- they are guaranteed to be malformed.
        . filter (firstLetter isUpper)
        . splitDirectories
        . dropExtension

-- | The module name, as stated in the module
codeModuleName :: IdeState -> NormalizedFilePath -> IO (Maybe (Range, T.Text))
codeModuleName state nfp = runMaybeT $ do
  (pm, mp) <- MaybeT . runAction "ModuleName.GetParsedModule" state $ useWithStale GetParsedModule nfp
  L (locA -> (RealSrcSpan l _)) m <- MaybeT . pure . hsmodName . unLoc $ pm_parsed_source pm
  range <- MaybeT . pure $ toCurrentRange mp (realSrcSpanToRange l)
  pure (range, T.pack $ moduleNameString m)

data Log =
    CorrectNames [T.Text]
  | BestName (Maybe T.Text)
  | ModuleName (Maybe T.Text)
  | SrcPaths [FilePath]
  | NormalisedPaths [FilePath]
  | AbsoluteFilePath FilePath
  deriving Show

instance Pretty Log where
  pretty log = "ModuleName." <> case log of
    CorrectNames log     -> "CorrectNames" <> colon <+> pretty log
    BestName log         -> "BestName" <> colon <+> pretty log
    ModuleName log       -> "StatedNameMaybe" <> colon <+> pretty log
    SrcPaths log         -> "SrcPaths" <> colon <+> pretty log
    NormalisedPaths log  -> "NormalisedPaths" <> colon <+> pretty log
    AbsoluteFilePath log -> "AbsoluteFilePath" <> colon <+> pretty log
