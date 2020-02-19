{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Ide.Types
    (
      IdePlugins(..)
    , PluginDescriptor(..)
    , PluginCommand(..)
    , PluginId(..)
    , CommandId(..)
    , DiagnosticProvider(..)
    , DiagnosticProviderFunc(..)
    , FormattingType(..)
    , FormattingProvider
    , HoverProvider
    , CodeActionProvider
    ) where

import           Data.Aeson                    hiding (defaultOptions)
import qualified Data.Map  as Map
import qualified Data.Set                      as S
import           Data.String
import qualified Data.Text                     as T
import           Data.Typeable
import           Development.IDE.Core.Rules
import           Development.IDE.Types.Diagnostics as D
import           Development.IDE.Types.Location
import           Language.Haskell.LSP.Types
import           Text.Regex.TDFA.Text()

-- ---------------------------------------------------------------------

newtype IdePlugins = IdePlugins
  { ipMap :: Map.Map PluginId PluginDescriptor
  }

-- ---------------------------------------------------------------------

data PluginDescriptor =
  PluginDescriptor { pluginId                 :: PluginId
                   , pluginCommands           :: [PluginCommand]
                   , pluginCodeActionProvider :: Maybe CodeActionProvider
                   , pluginDiagnosticProvider :: Maybe DiagnosticProvider
                   , pluginHoverProvider      :: Maybe HoverProvider
                   , pluginSymbolProvider     :: Maybe SymbolProvider
                   , pluginFormattingProvider :: Maybe (FormattingProvider IO)
                   }

instance Show PluginCommand where
  show (PluginCommand i _ _) = "PluginCommand { name = " ++ show i ++ " }"

newtype CommandId = CommandId T.Text
  deriving (Show, Read, Eq, Ord)
instance IsString CommandId where
  fromString = CommandId . T.pack

data PluginCommand = forall a b. (FromJSON a, ToJSON b, Typeable b) =>
  PluginCommand { commandId   :: CommandId
                , commandDesc :: T.Text
                , commandFunc :: a -> IO (Either ResponseError b)
                }

-- ---------------------------------------------------------------------

type CodeActionProvider =  IdeState
                        -> PluginId
                        -> TextDocumentIdentifier
                        -> Range
                        -> CodeActionContext
                        -> IO (Either ResponseError (List CAResult))

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
type HoverProvider = IdeState -> TextDocumentPositionParams -> IO (Either ResponseError (Maybe Hover))

type SymbolProvider = Uri -> IO (Either ResponseError [DocumentSymbol])

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
type FormattingProvider m
        = IdeState
        -> FormattingType  -- ^ How much to format
        -> T.Text -- ^ Text to format
        -> NormalizedFilePath -- ^ location of the file being formatted
        -> FormattingOptions -- ^ Options for the formatter
        -> m (Either ResponseError (List TextEdit)) -- ^ Result of the formatting

-- ---------------------------------------------------------------------
