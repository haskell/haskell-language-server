{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides code actions to add missing pragmas (whenever GHC suggests to)
module Ide.Plugin.Pragmas
  (
      codeAction
  ,   commands
  ) where

import           Control.Lens hiding (List)
import           Data.Aeson
import qualified Data.HashMap.Strict             as H
import qualified Data.Text                       as T
import           Ide.Plugin
import           Ide.Types
import qualified GHC.Generics                    as Generics
import qualified Language.Haskell.LSP.Types      as J
import qualified Language.Haskell.LSP.Types.Lens as J

import           Development.IDE.Types.Diagnostics as D
import           Language.Haskell.LSP.Types

-- ---------------------------------------------------------------------

_pragmasDescriptor :: PluginId -> PluginDescriptor
_pragmasDescriptor plId = PluginDescriptor
  { pluginId = plId
  -- , pluginName = "Add Missing Pragmas"
  -- , pluginDesc = "Provide code actions to add missing pragmas when GHC suggests this"
  , pluginCommands =
      [ PluginCommand "addPragma" "add the given pragma" addPragmaCmd
      ]
  , pluginCodeActionProvider = Just codeActionProvider
  , pluginDiagnosticProvider = Nothing
  , pluginHoverProvider = Nothing
  , pluginSymbolProvider = Nothing
  , pluginFormattingProvider = Nothing
  }

-- ---------------------------------------------------------------------

commands :: [PluginCommand]
commands = [ PluginCommand "addPragma" "add the given pragma" addPragmaCmd
           ]

-- ---------------------------------------------------------------------

-- | Parameters for the addPragma PluginCommand.
data AddPragmaParams = AddPragmaParams
  { file   :: J.Uri  -- ^ Uri of the file to add the pragma to
  , pragma :: T.Text -- ^ Name of the Pragma to add
  }
  deriving (Show, Eq, Generics.Generic, ToJSON, FromJSON)

-- | Add a Pragma to the given URI at the top of the file.
-- Pragma is added to the first line of the Uri.
-- It is assumed that the pragma name is a valid pragma,
-- thus, not validated.
addPragmaCmd :: AddPragmaParams -> IO (Either ResponseError J.WorkspaceEdit)
addPragmaCmd (AddPragmaParams uri pragmaName) = do
  let
    pos = J.Position 0 0
    textEdits = J.List
      [J.TextEdit (J.Range pos pos)
                  ("{-# LANGUAGE " <> pragmaName <> " #-}\n")
      ]
    res = J.WorkspaceEdit
      (Just $ H.singleton uri textEdits)
      Nothing
  return $ Right res

-- ---------------------------------------------------------------------

codeAction :: CodeActionProvider
codeAction = codeActionProvider

-- | Offer to add a missing Language Pragma to the top of a file.
-- Pragmas are defined by a curated list of known pragmas, see 'possiblePragmas'.
codeActionProvider :: CodeActionProvider
codeActionProvider _ plId docId _ (J.CodeActionContext (J.List diags) _monly) = do
  cmds <- mapM mkCommand pragmas
  return $ Right $ List cmds
  where
    -- Filter diagnostics that are from ghcmod
    ghcDiags = filter (\d -> d ^. J.source == Just "bios") diags
    -- Get all potential Pragmas for all diagnostics.
    pragmas = concatMap (\d -> findPragma (d ^. J.message)) ghcDiags
    mkCommand pragmaName = do
      let
        -- | Code Action for the given command.
        codeAction :: J.Command -> J.CAResult
        codeAction cmd = J.CACodeAction $ J.CodeAction title (Just J.CodeActionQuickFix) (Just (J.List [])) Nothing (Just cmd)
        title = "Add \"" <> pragmaName <> "\""
        cmdParams = [toJSON (AddPragmaParams (docId ^. J.uri) pragmaName )]
      cmd <- mkLspCommand plId "addPragma" title  (Just cmdParams)
      return $ codeAction cmd

-- ---------------------------------------------------------------------

-- | Find all Pragmas are an infix of the search term.
findPragma :: T.Text -> [T.Text]
findPragma str = concatMap check possiblePragmas
  where
    check p = [p | T.isInfixOf p str]

-- ---------------------------------------------------------------------

-- | Possible Pragma names.
-- Is non-exhaustive, and may be extended.
possiblePragmas :: [T.Text]
possiblePragmas =
  [
    "ConstraintKinds"
  , "DefaultSignatures"
  , "DeriveAnyClass"
  , "DeriveDataTypeable"
  , "DeriveFoldable"
  , "DeriveFunctor"
  , "DeriveGeneric"
  , "DeriveLift"
  , "DeriveTraversable"
  , "DerivingStrategies"
  , "DerivingVia"
  , "EmptyCase"
  , "EmptyDataDecls"
  , "EmptyDataDeriving"
  , "FlexibleContexts"
  , "FlexibleInstances"
  , "GADTs"
  , "GHCForeignImportPrim"
  , "GeneralizedNewtypeDeriving"
  , "IncoherentInstances"
  , "InstanceSigs"
  , "KindSignatures"
  , "MultiParamTypeClasses"
  , "MultiWayIf"
  , "NamedFieldPuns"
  , "NamedWildCards"
  , "OverloadedStrings"
  , "ParallelListComp"
  , "PartialTypeSignatures"
  , "PatternGuards"
  , "PatternSignatures"
  , "PatternSynonyms"
  , "QuasiQuotes"
  , "Rank2Types"
  , "RankNTypes"
  , "RecordPuns"
  , "RecordWildCards"
  , "RecursiveDo"
  , "RelaxedPolyRec"
  , "RoleAnnotations"
  , "ScopedTypeVariables"
  , "StandaloneDeriving"
  , "StaticPointers"
  , "TemplateHaskell"
  , "TemplateHaskellQuotes"
  , "TransformListComp"
  , "TupleSections"
  , "TypeApplications"
  , "TypeFamilies"
  , "TypeFamilyDependencies"
  , "TypeInType"
  , "TypeOperators"
  , "TypeSynonymInstances"
  , "UnboxedSums"
  , "UndecidableInstances"
  , "UndecidableSuperClasses"
  , "ViewPatterns"
  ]

-- ---------------------------------------------------------------------
