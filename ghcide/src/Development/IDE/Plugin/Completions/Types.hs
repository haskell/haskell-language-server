{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedLabels   #-}
module Development.IDE.Plugin.Completions.Types (
  module Development.IDE.Plugin.Completions.Types
) where

import           Control.DeepSeq
import qualified Data.Map                     as Map
import qualified Data.Text                    as T
import           SrcLoc

import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Text                    (Text)
import           Development.IDE.Spans.Common
import           GHC.Generics                 (Generic)
import           Ide.Plugin.Config            (Config)
import           Ide.Plugin.Properties
import           Ide.PluginUtils              (usePropertyLsp)
import           Ide.Types                    (PluginId)
import           Language.LSP.Server          (MonadLsp)
import           Language.LSP.Types           (CompletionItemKind (..), Uri)

-- From haskell-ide-engine/src/Haskell/Ide/Engine/LSP/Completions.hs

data Backtick = Surrounded | LeftSide
  deriving (Eq, Ord, Show)

extendImportCommandId :: Text
extendImportCommandId = "extendImport"

properties :: Properties
  '[ 'PropertyKey "autoExtendOn" 'TBoolean,
     'PropertyKey "snippetsOn" 'TBoolean]
properties = emptyProperties
  & defineBooleanProperty #snippetsOn
    "Inserts snippets when using code completions"
    True
  & defineBooleanProperty #autoExtendOn
    "Extends the import list automatically when completing a out-of-scope identifier"
    True

getCompletionsConfig :: (MonadLsp Config m) => PluginId -> m CompletionsConfig
getCompletionsConfig pId =
  CompletionsConfig
    <$> usePropertyLsp #snippetsOn pId properties
    <*> usePropertyLsp #autoExtendOn pId properties


data CompletionsConfig = CompletionsConfig {
  enableSnippets   :: Bool,
  enableAutoExtend :: Bool
}

data ExtendImport = ExtendImport
  { doc         :: !Uri,
    newThing    :: !T.Text,
    thingParent :: !(Maybe T.Text),
    importName  :: !T.Text,
    importQual  :: !(Maybe T.Text)
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CompItem = CI
  { compKind            :: CompletionItemKind
  , insertText          :: T.Text         -- ^ Snippet for the completion
  , importedFrom        :: Either SrcSpan T.Text         -- ^ From where this item is imported from.
  , typeText            :: Maybe T.Text   -- ^ Available type information.
  , label               :: T.Text         -- ^ Label to display to the user.
  , isInfix             :: Maybe Backtick -- ^ Did the completion happen
                                   -- in the context of an infix notation.
  , docs                :: SpanDoc        -- ^ Available documentation.
  , isTypeCompl         :: Bool
  , additionalTextEdits :: Maybe ExtendImport
  }
  deriving (Eq, Show)

-- Associates a module's qualifier with its members
newtype QualCompls
  = QualCompls { getQualCompls :: Map.Map T.Text [CompItem] }
  deriving Show
instance Semigroup QualCompls where
  (QualCompls a) <> (QualCompls b) = QualCompls $ Map.unionWith (++) a b
instance Monoid QualCompls where
  mempty = QualCompls Map.empty
  mappend = (Prelude.<>)

-- | End result of the completions
data CachedCompletions = CC
  { allModNamesAsNS   :: [T.Text] -- ^ All module names in scope.
                                -- Prelude is a single module
  , unqualCompls      :: [CompItem]  -- ^ Unqualified completion items
  , qualCompls        :: QualCompls    -- ^ Completion items associated to
                                -- to a specific module name.
  , anyQualCompls     :: [Maybe T.Text -> CompItem] -- ^ Items associated to any qualifier
  }

instance Show CachedCompletions where show _ = "<cached completions>"

instance NFData CachedCompletions where
    rnf = rwhnf

instance Monoid CachedCompletions where
    mempty = CC mempty mempty mempty mempty

instance Semigroup CachedCompletions where
    CC a b c d <> CC a' b' c' d' =
        CC (a<>a') (b<>b') (c<>c') (d<>d')
