{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedLabels   #-}
{-# LANGUAGE TypeFamilies       #-}
module Development.IDE.Plugin.Completions.Types (
  module Development.IDE.Plugin.Completions.Types
) where

import           Control.DeepSeq
import qualified Data.Map                     as Map
import qualified Data.Text                    as T

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Hashable                (Hashable)
import           Data.Text                    (Text)
import           Development.IDE.GHC.Compat
import           Development.IDE.Graph        (RuleResult)
import           Development.IDE.Spans.Common ()
import           GHC.Generics                 (Generic)
import qualified GHC.Types.Name.Occurrence    as Occ
import           Ide.Plugin.Properties
import           Language.LSP.Protocol.Types  (CompletionItemKind (..), Uri)
import qualified Language.LSP.Protocol.Types  as J

-- | Produce completions info for a file
type instance RuleResult LocalCompletions = CachedCompletions
type instance RuleResult NonLocalCompletions = CachedCompletions

data LocalCompletions = LocalCompletions
    deriving (Eq, Show, Generic)
instance Hashable LocalCompletions
instance NFData   LocalCompletions

data NonLocalCompletions = NonLocalCompletions
    deriving (Eq, Show, Generic)
instance Hashable NonLocalCompletions
instance NFData   NonLocalCompletions

-- From haskell-ide-engine/src/Haskell/Ide/Engine/LSP/Completions.hs

data Backtick = Surrounded | LeftSide
  deriving (Eq, Ord, Show)

extendImportCommandId :: Text
extendImportCommandId = "extendImport"

properties :: Properties
  '[ 'PropertyKey "autoExtendOn" TBoolean,
     'PropertyKey "snippetsOn" TBoolean]
properties = emptyProperties
  & defineBooleanProperty #snippetsOn
    "Inserts snippets when using code completions"
    True
  & defineBooleanProperty #autoExtendOn
    "Extends the import list automatically when completing a out-of-scope identifier"
    True


data CompletionsConfig = CompletionsConfig {
  enableSnippets   :: Bool,
  enableAutoExtend :: Bool,
  maxCompletions   :: Int
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

data Provenance
    = ImportedFrom Text
    | DefinedIn Text
    | Local SrcSpan
    deriving (Eq, Ord, Show)

data CompItem = CI
  { compKind            :: CompletionItemKind
  , insertText          :: T.Text         -- ^ Snippet for the completion
  , provenance          :: Provenance     -- ^ From where this item is imported from.
  , label               :: T.Text         -- ^ Label to display to the user.
  , typeText            :: Maybe T.Text
  , isInfix             :: Maybe Backtick -- ^ Did the completion happen
                                   -- in the context of an infix notation.
  , isTypeCompl         :: Bool
  , additionalTextEdits :: Maybe ExtendImport
  , nameDetails         :: Maybe NameDetails -- ^ For resolving purposes
  , isLocalCompletion   :: Bool              -- ^ Is it from this module?
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
  , importableModules :: [T.Text] -- ^ All modules that may be imported.
  }

instance Show CachedCompletions where show _ = "<cached completions>"

instance NFData CachedCompletions where
    rnf = rwhnf

instance Monoid CachedCompletions where
    mempty = CC mempty mempty mempty mempty mempty

instance Semigroup CachedCompletions where
    CC a b c d e <> CC a' b' c' d' e' =
        CC (a<>a') (b<>b') (c<>c') (d<>d') (e<>e')


-- | Describes the line at the current cursor position
data PosPrefixInfo = PosPrefixInfo
  { fullLine    :: !T.Text
    -- ^ The full contents of the line the cursor is at

  , prefixScope :: !T.Text
    -- ^ If any, the module name that was typed right before the cursor position.
    --  For example, if the user has typed "Data.Maybe.from", then this property
    --  will be "Data.Maybe"
    -- If OverloadedRecordDot is enabled, "Shape.rect.width" will be
    -- "Shape.rect"

  , prefixText  :: !T.Text
    -- ^ The word right before the cursor position, after removing the module part.
    -- For example if the user has typed "Data.Maybe.from",
    -- then this property will be "from"
  , cursorPos   :: !J.Position
    -- ^ The cursor position
  } deriving (Show,Eq)


-- | This is a JSON serialisable representation of a GHC Name that we include in
-- completion responses so that we can recover the original name corresponding
-- to the completion item. This is used to resolve additional details on demand
-- about the item like its type and documentation.
data NameDetails
  = NameDetails Module OccName
  deriving (Eq)

-- NameSpace is abstract so need these
nsJSON :: NameSpace -> Value
nsJSON ns
  | isVarNameSpace ns = String "v"
  | isDataConNameSpace ns = String "c"
  | isTcClsNameSpace ns  = String "t"
  | isTvNameSpace ns = String "z"
  | otherwise = error "namespace not recognized"

parseNs :: Value -> Parser NameSpace
parseNs (String "v") = pure Occ.varName
parseNs (String "c") = pure dataName
parseNs (String "t") = pure tcClsName
parseNs (String "z") = pure tvName
parseNs _            = mempty

instance FromJSON NameDetails where
  parseJSON v@(Array _)
    = do
      [modname,modid,namesp,occname] <- parseJSON v
      mn  <- parseJSON modname
      mid <- parseJSON modid
      ns <- parseNs namesp
      occn <- parseJSON occname
      pure $ NameDetails (mkModule (stringToUnit mid) (mkModuleName mn)) (mkOccName ns occn)
  parseJSON _ = mempty
instance ToJSON NameDetails where
  toJSON (NameDetails mdl occ) = toJSON [toJSON mname,toJSON mid,nsJSON ns,toJSON occs]
    where
      mname = moduleNameString $ moduleName mdl
      mid = unitIdString $ moduleUnitId mdl
      ns = occNameSpace occ
      occs = occNameString occ
instance Show NameDetails where
  show = show . toJSON

-- | The data that is actually sent for resolve support
-- We need the URI to be able to reconstruct the GHC environment
-- in the file the completion was triggered in.
data CompletionResolveData = CompletionResolveData
  { itemFile      :: Uri
  , itemNeedsType :: Bool -- ^ Do we need to lookup a type for this item?
  , itemName      :: NameDetails
  }
  deriving stock Generic
  deriving anyclass (FromJSON, ToJSON)
