module Development.IDE.Plugin.Completions.Types (
  module Development.IDE.Plugin.Completions.Types
) where

import           Control.DeepSeq
import qualified Data.Map  as Map
import qualified Data.Text as T

import Development.IDE.Spans.Common
import Language.Haskell.LSP.Types (CompletionItemKind)

-- From haskell-ide-engine/src/Haskell/Ide/Engine/LSP/Completions.hs

data Backtick = Surrounded | LeftSide
  deriving (Eq, Ord, Show)

data CompItem = CI
  { compKind     :: CompletionItemKind
  , insertText   :: T.Text         -- ^ Snippet for the completion
  , importedFrom :: T.Text         -- ^ From where this item is imported from.
  , typeText     :: Maybe T.Text   -- ^ Available type information.
  , label        :: T.Text         -- ^ Label to display to the user.
  , isInfix      :: Maybe Backtick -- ^ Did the completion happen
                                   -- in the context of an infix notation.
  , docs         :: SpanDoc        -- ^ Available documentation.
  , isTypeCompl  :: Bool
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
  { allModNamesAsNS :: [T.Text] -- ^ All module names in scope.
                                -- Prelude is a single module
  , unqualCompls :: [CompItem]  -- ^ All Possible completion items
  , qualCompls :: QualCompls    -- ^ Completion items associated to
                                -- to a specific module name.
  , importableModules :: [T.Text] -- ^ All modules that may be imported.
  } deriving Show

instance NFData CachedCompletions where
    rnf = rwhnf

instance Monoid CachedCompletions where
    mempty = CC mempty mempty mempty mempty

instance Semigroup CachedCompletions where
    CC a b c d <> CC a' b' c' d' =
        CC (a<>a') (b<>b') (c<>c') (d<>d')
