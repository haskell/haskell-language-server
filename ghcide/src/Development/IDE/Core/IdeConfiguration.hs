{-# LANGUAGE DuplicateRecordFields #-}
module Development.IDE.Core.IdeConfiguration
  ( IdeConfiguration(..)
  , registerIdeConfiguration
  , parseConfiguration
  , parseWorkspaceFolder
  , isWorkspaceFile
  , modifyWorkspaceFolders
  )
where

import           Control.Concurrent.Extra
import           Control.Monad
import           Data.HashSet                   (HashSet, singleton)
import           Data.Text                      (Text, isPrefixOf)
import           Development.IDE.Core.Shake
import           Development.IDE.Types.Location
import           Development.Shake
import           Language.Haskell.LSP.Types
import           System.FilePath (isRelative)

-- | Lsp client relevant configuration details
data IdeConfiguration = IdeConfiguration
  { workspaceFolders :: HashSet NormalizedUri
  }
  deriving (Show)

newtype IdeConfigurationVar = IdeConfigurationVar {unIdeConfigurationRef :: Var IdeConfiguration}

instance IsIdeGlobal IdeConfigurationVar

registerIdeConfiguration :: ShakeExtras -> IdeConfiguration -> IO ()
registerIdeConfiguration extras =
  addIdeGlobalExtras extras . IdeConfigurationVar <=< newVar

getIdeConfiguration :: Action IdeConfiguration
getIdeConfiguration =
  getIdeGlobalAction >>= liftIO . readVar . unIdeConfigurationRef

parseConfiguration :: InitializeParams -> IdeConfiguration
parseConfiguration InitializeParams {..} =
  IdeConfiguration { .. }
 where
  workspaceFolders =
    foldMap (singleton . toNormalizedUri) _rootUri
      <> (foldMap . foldMap)
           (singleton . parseWorkspaceFolder)
           _workspaceFolders

parseWorkspaceFolder :: WorkspaceFolder -> NormalizedUri
parseWorkspaceFolder =
  toNormalizedUri . Uri . (_uri :: WorkspaceFolder -> Text)

modifyWorkspaceFolders
  :: IdeState -> (HashSet NormalizedUri -> HashSet NormalizedUri) -> IO ()
modifyWorkspaceFolders ide f = do
  IdeConfigurationVar var <- getIdeGlobalState ide
  IdeConfiguration    ws  <- readVar var
  writeVar var (IdeConfiguration (f ws))

isWorkspaceFile :: NormalizedFilePath -> Action Bool
isWorkspaceFile file =
  if isRelative (fromNormalizedFilePath file)
    then return True
    else do
      IdeConfiguration {..} <- getIdeConfiguration
      let toText = getUri . fromNormalizedUri
      return $
        any
          (\root -> toText root `isPrefixOf` toText (filePathToUri' file))
          workspaceFolders
