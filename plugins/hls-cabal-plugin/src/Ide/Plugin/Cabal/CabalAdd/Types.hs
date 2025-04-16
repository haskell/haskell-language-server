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
  | LogCalledCabalAddPackageCommand CabalAddPackageCommandParams
  | LogCalledCabalAddModuleCommand ModuleInsertionConfig
  | LogCreatedEdit WorkspaceEdit
  | LogExecutedCommand
  deriving (Show)

instance Pretty Log where
  pretty = \case
    LogFoundResponsibleCabalFile fp -> "Located the responsible cabal file at " <+> pretty fp
    LogCalledCabalAddPackageCommand params -> "Called CabalAddPackage command with:\n" <+> pretty params
    LogCalledCabalAddModuleCommand params -> "Called CabalAddModule command with:\n" <+> pretty params
    LogCreatedEdit edit -> "Created inplace edit:\n" <+> pretty edit
    LogExecutedCommand -> "Executed CabalAdd command"

cabalAddPackageCommandId :: (IsString p) => p
cabalAddPackageCommandId = "cabalAddPackage"

cabalAddModuleCommandId :: (IsString p) => p
cabalAddModuleCommandId = "cabalAddModule"

-- | Relevant data needed to add a module to a cabal file.
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
    "CabalAdd parameters:"
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

-- | Parameters for the LSP `CabalAddCommand`
data CabalAddPackageCommandParams = CabalAddPackageCommandParams
  { pkgCabalPath   :: FilePath
  , pkgVerTxtDocId :: VersionedTextDocumentIdentifier
  , pkgBuildTarget :: Maybe String
  , pkgDependency  :: T.Text
  , pkgVersion     :: Maybe T.Text
  }
  deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

instance Pretty CabalAddPackageCommandParams where
  pretty CabalAddPackageCommandParams{..} =
    "CabalAdd parameters:"
      <+> vcat
        [ "cabal path:" <+> pretty pkgCabalPath
        , "target:" <+> pretty pkgBuildTarget
        , "dependendency:" <+> pretty pkgDependency
        , "version:" <+> pretty pkgVersion
        ]
