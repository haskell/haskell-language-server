{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}

module Ide.Plugin.Cabal.CabalAdd.Types where

import           Data.Aeson.Types                (FromJSON, ToJSON)
import           Data.String                     (IsString)
import qualified Data.Text                       as T
import           Distribution.Compat.Prelude     (Generic)
import           Distribution.PackageDescription
import           Ide.Logger
import           Ide.Plugin.Cabal.Orphans        ()
import           Language.LSP.Protocol.Types

data Log
  = LogFoundResponsibleCabalFile FilePath
  | LogCalledCabalAddDependencyCommand CabalAddDependencyCommandParams
  | LogCalledCabalAddModuleCommand ModuleInsertionConfig
  | LogCreatedEdit WorkspaceEdit
  | LogExecutedCommand
  | LogFailedToResolveComponent String
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogFoundResponsibleCabalFile fp -> "Located the responsible cabal file at " <+> pretty fp
    LogCalledCabalAddDependencyCommand params -> "Called CabalAddDependency command with:\n" <+> pretty params
    LogCalledCabalAddModuleCommand params -> "Called CabalAddModule command with:\n" <+> pretty params
    LogCreatedEdit edit -> "Created inplace edit:\n" <+> pretty edit
    LogExecutedCommand -> "Executed CabalAdd command"
    LogFailedToResolveComponent cS -> "Failed to resolve component in CabalAdd with error:" <+> viaShow cS

cabalAddDependencyCommandId :: (IsString p) => p
cabalAddDependencyCommandId = "cabalAddDependency"

cabalAddModuleCommandId :: (IsString p) => p
cabalAddModuleCommandId = "cabalAddModule"

-- | Relevant data needed to add a module to a cabal file.
--
-- This will be sent as json to the client with a code action we offer to add this dependency to a cabal file.
-- If the user decides to execute the corresponding code action, the client sends us this data again, and we then
-- use it to execute the `CabalAddDependencyCommand`.
data ModuleInsertionConfig = ModuleInsertionConfig
  { targetFile      :: FilePath
  -- ^ The file we want to insert information about the new module into.
  , moduleToInsert  :: T.Text
  -- ^ The module name of the module to be inserted into the targetFile at the insertionPosition.
  , modVerTxtDocId  :: VersionedTextDocumentIdentifier
  , insertionStanza :: ComponentName
  -- ^ Which stanza the module will be inserted into.
  , insertionLabel  :: T.Text
  -- ^ A label which describes which field the module will be inserted into.
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty ModuleInsertionConfig where
  pretty ModuleInsertionConfig{..} =
    "CabalAddModule parameters:"
      <+> vcat
        [ "cabal path:" <+> pretty targetFile
        , "target:" <+> pretty moduleToInsert
        , "stanza:" <+> viaShow insertionStanza
        , "label:" <+> pretty insertionLabel
        ]

-- | Contains all source directories of a stanza with the name of the first parameter.
data StanzaItem = StanzaItem
  { siComponent    :: ComponentName
  , siHsSourceDirs :: [FilePath]
  }
  deriving (Show)

-- | Relevant data needed to add a dependency to a cabal file.
--
-- This will be sent as json to the client with a code action we offer to add this dependency to a cabal file.
-- If the user decides to execute the corresponding code action, the client sends us this data again, and we then
-- use it to execute the `CabalAddDependencyCommand`.
data CabalAddDependencyCommandParams = CabalAddDependencyCommandParams
  { depCabalPath   :: FilePath
  , depVerTxtDocId :: VersionedTextDocumentIdentifier
  , depBuildTarget :: Maybe String
  , depDependency  :: T.Text
  , depVersion     :: Maybe T.Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty CabalAddDependencyCommandParams where
  pretty CabalAddDependencyCommandParams{..} =
    "CabalAddDependency parameters:"
      <+> vcat
        [ "cabal path:" <+> pretty depCabalPath
        , "target:" <+> pretty depBuildTarget
        , "dependendency:" <+> pretty depDependency
        , "version:" <+> pretty depVersion
        ]
