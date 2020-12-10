{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Provides code actions to add missing pragmas (whenever GHC suggests to)
module Ide.Plugin.Pragmas
  (
      descriptor
  -- ,   commands -- TODO: get rid of this
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
import           Development.IDE as D
import           Language.Haskell.LSP.Types

import qualified Language.Haskell.LSP.Core as LSP
import qualified Language.Haskell.LSP.VFS as VFS
import Development.IDE.Core.Shake (ShakeExtras(logger))

-- ---------------------------------------------------------------------

descriptor :: PluginId -> PluginDescriptor
descriptor plId = (defaultPluginDescriptor plId)
  { pluginCommands = commands
  , pluginCodeActionProvider = Just codeActionProvider
  , pluginCompletionProvider = Just completion
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
addPragmaCmd :: CommandFunction AddPragmaParams
addPragmaCmd _lf _ide (AddPragmaParams uri pragmaName) = do
  let
    pos = J.Position 0 0
    textEdits = J.List
      [J.TextEdit (J.Range pos pos)
                  ("{-# LANGUAGE " <> pragmaName <> " #-}\n")
      ]
    res = J.WorkspaceEdit
      (Just $ H.singleton uri textEdits)
      Nothing
  return (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams res))

-- ---------------------------------------------------------------------

-- | Offer to add a missing Language Pragma to the top of a file.
-- Pragmas are defined by a curated list of known pragmas, see 'possiblePragmas'.
codeActionProvider :: CodeActionProvider
codeActionProvider _ _ plId docId _ (J.CodeActionContext (J.List diags) _monly) = do
  cmds <- mapM mkCommand pragmas
  -- cmds <- mapM mkCommand ("FooPragma":pragmas)
  return $ Right $ List cmds
  where
    -- Filter diagnostics that are from ghcmod
    ghcDiags = filter (\d -> d ^. J.source == Just "typecheck") diags
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

logStuff :: IdeState -> T.Text -> IO ()
logStuff ide = logInfo (logger (shakeExtras ide))

completion :: CompletionProvider
completion lspFuncs _ide complParams = do
    let (TextDocumentIdentifier uri) = complParams ^. J.textDocument
        position = complParams ^. J.position
    logStuff _ide (T.pack "test ---------------------.......")
    putStrLn $ "Uri" ++ show uri
    putStrLn $ "nor uri" ++ show (toNormalizedUri uri)
    logStuff _ide (T.pack "--------------------------------.......")
    contents <- LSP.getVirtualFileFunc lspFuncs $ toNormalizedUri uri
    fmap Right $ case (contents, uriToFilePath' uri) of
        (Just cnts, Just _path) -> do
            pfix <- VFS.getCompletionPrefix position cnts
            logStuff _ide (T.pack "test &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&.......")
            logStuff _ide $ "pfix" <> (T.pack. show $ pfix)
            logStuff _ide (T.pack "test &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&.......")
            return $ result pfix
            where
                result (Just pfix)
                    | "{-# LANGUAGE" `T.isPrefixOf` VFS.fullLine pfix
                    = Completions $ List $ map buildCompletion possiblePragmas
                    | otherwise
                    = Completions $ List []
                result Nothing = Completions $ List []
                buildCompletion p =
                   CompletionItem
                     label
                     kind
                     tags
                     detail
                     documentation
                     deprecated
                     preselect
                     sortText
                     filterText
                     insertText
                     insertTextFormat
                     textEdit
                     additionalTextEdits
                     commitCharacters
                     command
                     xd
                    where
                         label = p
                         kind = Nothing
                         tags = List []
                         detail = Nothing
                         documentation = Nothing
                         deprecated = Nothing
                         preselect = Nothing
                         sortText = Nothing
                         filterText = Nothing
                         insertText = Nothing
                         insertTextFormat = Nothing
                         textEdit = Nothing
                         additionalTextEdits = Nothing
                         commitCharacters = Nothing
                         command = Nothing
                         xd = Nothing
        _ -> return $ Completions $ List []
