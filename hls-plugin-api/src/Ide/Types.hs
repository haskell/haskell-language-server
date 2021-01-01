{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Types
    (
      IdePlugins(..)
    , PluginDescriptor(..)
    , defaultPluginDescriptor
    , PluginCommand(..)
    , PluginId(..)
    , CommandId(..)
    , DiagnosticProvider(..)
    , DiagnosticProviderFunc(..)
    , SymbolsProvider
    , FormattingType(..)
    , FormattingProvider
    , noneProvider
    , HoverProvider
    , CodeActionProvider
    , CodeLensProvider
    , CommandFunction
    , ExecuteCommandProvider
    , CompletionProvider
    , RenameProvider
    , WithSnippets(..)
    ) where

import           Data.Aeson                    hiding (defaultOptions)
import qualified Data.Map  as Map
import qualified Data.Set                      as S
import           Data.String
import qualified Data.Text                     as T
import           Development.Shake
import           Ide.Plugin.Config
import qualified Language.Haskell.LSP.Core as LSP
import           Language.Haskell.LSP.Types
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

newtype IdePlugins ideState = IdePlugins
  { ipMap :: Map.Map PluginId (PluginDescriptor ideState)}

-- ---------------------------------------------------------------------

data PluginDescriptor ideState =
  PluginDescriptor { pluginId                 :: !PluginId
                   , pluginRules              :: !(Rules ())
                   , pluginCommands           :: ![PluginCommand ideState]
                   , pluginCodeActionProvider :: !(Maybe (CodeActionProvider ideState))
                   , pluginCodeLensProvider   :: !(Maybe (CodeLensProvider ideState))
                   , pluginDiagnosticProvider :: !(Maybe DiagnosticProvider)
                     -- ^ TODO: diagnostics are generally provided via rules,
                     -- this is probably redundant.
                   , pluginHoverProvider      :: !(Maybe (HoverProvider ideState))
                   , pluginSymbolsProvider    :: !(Maybe (SymbolsProvider ideState))
                   , pluginFormattingProvider :: !(Maybe (FormattingProvider ideState IO))
                   , pluginCompletionProvider :: !(Maybe (CompletionProvider ideState))
                   , pluginRenameProvider     :: !(Maybe (RenameProvider ideState))
                   }

defaultPluginDescriptor :: PluginId -> PluginDescriptor ideState
defaultPluginDescriptor plId =
  PluginDescriptor
    plId
    mempty
    mempty
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

-- instance Show PluginCommand where
--   show (PluginCommand i _ _) = "PluginCommand { name = " ++ show i ++ " }"

-- newtype CommandId = CommandId T.Text
--   deriving (Show, Read, Eq, Ord)
-- instance IsString CommandId where
--   fromString = CommandId . T.pack

-- data PluginCommand = forall a b. (FromJSON a, ToJSON b, Typeable b) =>
--   PluginCommand { commandId   :: CommandId
--                 , commandDesc :: T.Text
--                 , commandFunc :: a -> IO (Either ResponseError b)
--                 }

newtype CommandId = CommandId T.Text
  deriving (Show, Read, Eq, Ord)
instance IsString CommandId where
  fromString = CommandId . T.pack

data PluginCommand ideState = forall a. (FromJSON a) =>
  PluginCommand { commandId   :: CommandId
                , commandDesc :: T.Text
                , commandFunc :: CommandFunction ideState a
                }


-- ---------------------------------------------------------------------

type CommandFunction ideState a = LSP.LspFuncs Config
                       -> ideState
                       -> a
                       -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))

type CodeActionProvider ideState = LSP.LspFuncs Config
                        -> ideState
                        -> PluginId
                        -> TextDocumentIdentifier
                        -> Range
                        -> CodeActionContext
                        -> IO (Either ResponseError (List CAResult))

type CompletionProvider ideState = LSP.LspFuncs Config
                        -> ideState
                        -> CompletionParams
                        -> IO (Either ResponseError CompletionResponseResult)



type CodeLensProvider ideState = LSP.LspFuncs Config
                      -> ideState
                      -> PluginId
                      -> CodeLensParams
                      -> IO (Either ResponseError (List CodeLens))

type RenameProvider ideState = LSP.LspFuncs Config
                    -> ideState
                    -> RenameParams
                    -> IO (Either ResponseError WorkspaceEdit)

type DiagnosticProviderFuncSync
  = DiagnosticTrigger -> Uri
  -> IO (Either ResponseError (Map.Map Uri (S.Set Diagnostic)))

type DiagnosticProviderFuncAsync
  = DiagnosticTrigger -> Uri
  -> (Map.Map Uri (S.Set Diagnostic) -> IO ())
  -> IO (Either ResponseError ())

data DiagnosticProviderFunc
  = DiagnosticProviderSync  DiagnosticProviderFuncSync
  | DiagnosticProviderAsync DiagnosticProviderFuncAsync


data DiagnosticProvider = DiagnosticProvider
     { dpTrigger :: S.Set DiagnosticTrigger -- AZ:should this be a NonEmptyList?
     , dpFunc    :: DiagnosticProviderFunc
     }

data DiagnosticTrigger = DiagnosticOnOpen
                       | DiagnosticOnChange
                       | DiagnosticOnSave
                       deriving (Show,Ord,Eq)

-- type HoverProvider = Uri -> Position -> IO (Either ResponseError [Hover])
type HoverProvider ideState = ideState -> TextDocumentPositionParams -> IO (Either ResponseError (Maybe Hover))

type SymbolsProvider ideState = LSP.LspFuncs Config
                     -> ideState
                     -> DocumentSymbolParams
                     -> IO (Either ResponseError [DocumentSymbol])

type ExecuteCommandProvider ideState = ideState
                            -> ExecuteCommandParams
                            -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))

newtype WithSnippets = WithSnippets Bool

-- ---------------------------------------------------------------------

newtype PluginId = PluginId T.Text
  deriving (Show, Read, Eq, Ord)
instance IsString PluginId where
  fromString = PluginId . T.pack

-- ---------------------------------------------------------------------


-- | Format the given Text as a whole or only a @Range@ of it.
-- Range must be relative to the text to format.
-- To format the whole document, read the Text from the file and use 'FormatText'
-- as the FormattingType.
data FormattingType = FormatText
                    | FormatRange Range


-- | To format a whole document, the 'FormatText' @FormattingType@ can be used.
-- It is required to pass in the whole Document Text for that to happen, an empty text
-- and file uri, does not suffice.
type FormattingProvider ideState m
        = LSP.LspFuncs Config
        -> ideState
        -> FormattingType  -- ^ How much to format
        -> T.Text -- ^ Text to format
        -> NormalizedFilePath -- ^ location of the file being formatted
        -> FormattingOptions -- ^ Options for the formatter
        -> m (Either ResponseError (List TextEdit)) -- ^ Result of the formatting

noneProvider :: FormattingProvider ideState IO
noneProvider _ _ _ _ _ _ = return $ Right (List [])
