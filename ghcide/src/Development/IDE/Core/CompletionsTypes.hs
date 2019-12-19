module Development.IDE.Core.CompletionsTypes (
  module Development.IDE.Core.CompletionsTypes
) where

import           Control.DeepSeq
import qualified Data.Map  as Map
import qualified Data.Text as T

import           GHC
import           Outputable
import           DynFlags

-- From haskell-ide-engine/src/Haskell/Ide/Engine/LSP/Completions.hs

showGhc :: Outputable a => a -> String
showGhc = showPpr unsafeGlobalDynFlags

data Backtick = Surrounded | LeftSide deriving Show
data CompItem = CI
  { origName     :: Name           -- ^ Original name, such as Maybe, //, or find.
  , importedFrom :: T.Text         -- ^ From where this item is imported from.
  , thingType    :: Maybe Type     -- ^ Available type information.
  , label        :: T.Text         -- ^ Label to display to the user.
  , isInfix      :: Maybe Backtick -- ^ Did the completion happen
                                   -- in the context of an infix notation.
  , docs         :: [T.Text]       -- ^ Available documentation.
  }
instance Show CompItem where
  show CI { .. } = "CompItem { origName = \"" ++ showGhc origName ++ "\""
                   ++ ", importedFrom = " ++ show importedFrom
                   ++ ", thingType = " ++ show (fmap showGhc thingType)
                   ++ ", label = " ++ show label
                   ++ ", isInfix = " ++ show isInfix 
                   ++ ", docs = " ++ show docs
                   ++ " } "
instance Eq CompItem where
  ci1 == ci2 = origName ci1 == origName ci2
instance Ord CompItem where
  compare ci1 ci2 = origName ci1 `compare` origName ci2

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