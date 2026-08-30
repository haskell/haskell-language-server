{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms           #-}
{-# LANGUAGE TypeFamilies              #-}

module Development.IDE.Core.RuleInput
    ( RuleInput
    , InputFingerprint(..)
    , ProjectHaskellInput(..)
    , NonProjectHaskellInput(..)
    , SomeHaskellInput(..)
    , CabalInput(..)
    , SomeFileInput(..)
    , NoInput(..)
    , SomeInput
    , IsInput(..)
    , fileInputFingerprint
    , isHaskellFilePath
    , isDependencyHaskellPath
    , IsFileInput(..)
    , inputUri
    , toProjectHaskellInput
    , toNonProjectHaskellInput
    , toCabalInput
    , toSomeHaskellInput
    , toSomeFileInput
    , classifyAsProjectHaskell
    , classifyAsDep
    , classifyAsCabal
    , classifyAsSomeHaskell
    , classifyAsSomeFile
    ) where

import           Control.DeepSeq
import           Control.Monad.Trans.Except  (ExceptT, throwE)
import           Data.Hashable
import           Data.List                   (isInfixOf)
import qualified Data.Text                   as T
import           Data.Typeable
import           GHC.Generics                (Generic)
import           Ide.Plugin.Error            (PluginError (..))
import           Language.LSP.Protocol.Types (NormalizedFilePath, Uri,
                                              filePathToUri,
                                              fromNormalizedFilePath,
                                              toNormalizedUri,
                                              uriToNormalizedFilePath)
import           System.FilePath             (splitDirectories, takeExtension)

-- | Associate a rule key @k@ with the type of input that identifies an
-- invocation of that rule.
--
-- Every rule key must define an instance of this open type family. For
-- example:
--
-- @
-- data GetParsedModule = GetParsedModule
-- type instance RuleInput GetParsedModule = ProjectHaskellInput
-- @
type family RuleInput k

-- | Identity of RuleInputs.
--
-- Used to efficiently compare and hash rule inputs.
data InputFingerprint
  = InputNoFile
  | InputFile !NormalizedFilePath
  | forall a. (Eq a, Hashable a, Typeable a) => InputValue a

instance Eq InputFingerprint where
  InputNoFile == InputNoFile = True
  InputFile p1 == InputFile p2 = p1 == p2
  InputValue a == InputValue b =
    case cast b of
      Just b' -> a == b'
      Nothing -> False
  _ == _ = False

instance Hashable InputFingerprint where
  hashWithSalt s InputNoFile    = hashWithSalt s (0 :: Int)
  hashWithSalt s (InputFile p)  = hashWithSalt s (1 :: Int, p)
  hashWithSalt s (InputValue a) = hashWithSalt s (2 :: Int, hash a)

{- Note [Rule input hierarchy]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'RuleInput' can be a node from this AST:

@
'SomeInput'                          --  any rule input
├── 'NoInput'                        --  global
└── 'SomeFileInput'                  --  has a 'NormalizedFilePath'
    ├── 'SomeHaskellInput'           --  all Haskell files
    │   ├── 'NonProjectHaskellInput' --  files in .hls/dependencies
    │   └── 'ProjectHaskellInput'    --  haskell files of your project
    └── 'CabalInput'                 --  all .cabal files
@

Upcasting wraps a child in each parent constructor, then uses 'toInput' for 'SomeInput';
for example, 'ProjectHaskellInput' -> 'SomeHaskellInput' -> 'SomeFileInput' -> 'SomeInput'.
We can downcast a rule input via 'fromInput'.
-}

-- | Types that can be used as rule inputs.
--
-- 'toInput' packs a value into the existential 'SomeInput' wrapper.
-- 'fromInput' attempts to recover a supported input type
-- 'inputFingerprint' defines the identity used when comparing and hashing wrapped inputs.
--
-- See Note [Rule input hierarchy].
class (Typeable i, Hashable i, Eq i, Show i, NFData i) => IsInput i where
  toInput :: i -> SomeInput
  toInput = SomeInput

  fromInput :: SomeInput -> Maybe i
  fromInput (SomeInput i) = cast i

  inputFingerprint :: i -> InputFingerprint
  inputFingerprint i = InputValue i

-- | Stores any value that implements 'IsInput'.
data SomeInput = forall i. IsInput i => SomeInput i

instance Eq SomeInput where
  SomeInput a == SomeInput b = inputFingerprint a == inputFingerprint b

instance Hashable SomeInput where
  hashWithSalt salt (SomeInput i) = hashWithSalt salt (inputFingerprint i)

instance Show SomeInput where
  show (SomeInput i) = show i

instance NFData SomeInput where
  rnf (SomeInput i) = rnf i

instance IsInput SomeInput where
  toInput = id
  fromInput = Just
  inputFingerprint (SomeInput i) = inputFingerprint i

-- | A Rule Input that has no file associated with it.
-- Rules with 'RuleInput' 'NoInput' ruletype must be treated as global rules.
data NoInput = NoInput
  deriving (Eq, Ord, Show, Generic)

instance Hashable NoInput

instance NFData NoInput

instance IsInput NoInput where
  fromInput input = case inputFingerprint input of
    InputNoFile -> Just NoInput
    _           -> Nothing

  inputFingerprint :: NoInput -> InputFingerprint
  inputFingerprint _ = InputNoFile

-- | A Rule Input that has some file (Haskell, cabal etc.) associated with it.
class IsInput i => IsFileInput i where
  inputFilePath :: i -> NormalizedFilePath

data SomeFileInput
  = SomeFileHaskellInput SomeHaskellInput
  | SomeFileCabalInput CabalInput
  | SomeFileNormalizedFilePath NormalizedFilePath
  deriving (Generic)

instance Eq SomeFileInput where
  a == b = fileInputFingerprint a == fileInputFingerprint b

instance Hashable SomeFileInput where
  hashWithSalt salt = hashWithSalt salt . fileInputFingerprint

instance Show SomeFileInput where
  show (SomeFileHaskellInput input) = "SomeFileInput (" <> show input <> ")"
  show (SomeFileCabalInput input) = "SomeFileInput (" <> show input <> ")"
  show (SomeFileNormalizedFilePath input) = "SomeFileInput (" <> show input <> ")"

instance NFData SomeFileInput

instance IsInput SomeFileInput where
  fromInput input = toSomeFileInput <$> someInputFilePathMaybe input
  inputFingerprint = fileInputFingerprint

instance IsFileInput SomeFileInput where
  inputFilePath (SomeFileHaskellInput input)       = inputFilePath input
  inputFilePath (SomeFileCabalInput input)         = inputFilePath input
  inputFilePath (SomeFileNormalizedFilePath input) = inputFilePath input

instance IsInput NormalizedFilePath where
  fromInput = someInputFilePathMaybe
  inputFingerprint = InputFile

instance IsFileInput NormalizedFilePath where
  inputFilePath = id

-- | Fingerprint a file input by its normalized file path.
fileInputFingerprint :: IsFileInput i => i -> InputFingerprint
fileInputFingerprint input = InputFile (inputFilePath input)

-- | Convert a file input to a URI.
inputUri :: IsFileInput i => i -> Uri
inputUri = filePathToUri . fromNormalizedFilePath . inputFilePath

-- | Leaf Type which represents a cabal file.
newtype CabalInput = CabalInput NormalizedFilePath
  deriving (Eq, Ord, Show, Generic)

instance Hashable CabalInput

instance NFData CabalInput

instance IsInput CabalInput where
  fromInput input = someInputFilePathMaybe input >>= toCabalInput
  inputFingerprint = fileInputFingerprint

instance IsFileInput CabalInput where
  inputFilePath (CabalInput path) = path

-- | Mark an input as a validated Haskell source file input.
class IsFileInput i => IsHaskellInput i

data SomeHaskellInput
  = SomeProjectHaskellInput ProjectHaskellInput
  | SomeNonProjectHaskellInput NonProjectHaskellInput
  deriving (Generic)

instance Eq SomeHaskellInput where
  a == b = fileInputFingerprint a == fileInputFingerprint b

instance Hashable SomeHaskellInput where
  hashWithSalt salt = hashWithSalt salt . fileInputFingerprint

instance Show SomeHaskellInput where
  show (SomeProjectHaskellInput input) = "SomeHaskellInput (" <> show input <> ")"
  show (SomeNonProjectHaskellInput input) = "SomeHaskellInput (" <> show input <> ")"

instance NFData SomeHaskellInput

instance IsInput SomeHaskellInput where
  fromInput input = someInputFilePathMaybe input >>= toSomeHaskellInput
  inputFingerprint = fileInputFingerprint

instance IsFileInput SomeHaskellInput where
  inputFilePath (SomeProjectHaskellInput input)    = inputFilePath input
  inputFilePath (SomeNonProjectHaskellInput input) = inputFilePath input

instance IsHaskellInput SomeHaskellInput

-- | Leaf Type representing a Haskell file inside project directory.
newtype ProjectHaskellInput = ProjectHaskellInput NormalizedFilePath
  deriving (Eq, Ord, Show, Generic)

instance Hashable ProjectHaskellInput

instance NFData ProjectHaskellInput

instance IsInput ProjectHaskellInput where
  fromInput input = someInputFilePathMaybe input >>= toProjectHaskellInput
  inputFingerprint = fileInputFingerprint

instance IsFileInput ProjectHaskellInput where
  inputFilePath (ProjectHaskellInput path) = path

instance IsHaskellInput ProjectHaskellInput

-- | Leaf Type representing a Haskell file inside project .hls/dependencies directory.

newtype NonProjectHaskellInput = NonProjectHaskellInput NormalizedFilePath
  deriving (Eq, Ord, Show, Generic)

instance Hashable NonProjectHaskellInput

instance NFData NonProjectHaskellInput

instance IsInput NonProjectHaskellInput where
  fromInput input = someInputFilePathMaybe input >>= toNonProjectHaskellInput
  inputFingerprint = fileInputFingerprint

instance IsFileInput NonProjectHaskellInput where
  inputFilePath (NonProjectHaskellInput path) = path

instance IsHaskellInput NonProjectHaskellInput

-- ----------------------------------------------------------------------------
-- Classify NFP as RuleInputs
-- ----------------------------------------------------------------------------
isCabalInput :: NormalizedFilePath -> Bool
isCabalInput = (== ".cabal") . takeExtension . fromNormalizedFilePath

-- TODO:  needs to be unified with optExtensions
isHaskellFilePath :: NormalizedFilePath -> Bool
isHaskellFilePath fp = takeExtension (fromNormalizedFilePath fp) `elem`
  [".hs", ".lhs", ".hs-boot", ".lhs-boot"]

isNonProjectHaskellInput :: NormalizedFilePath -> Bool
isNonProjectHaskellInput fp = isHaskellFilePath fp && isDependencyHaskellPath fp

isProjectHaskellInput :: NormalizedFilePath -> Bool
isProjectHaskellInput fp = isHaskellFilePath fp && not (isDependencyHaskellPath fp)

isDependencyHaskellPath :: NormalizedFilePath -> Bool
isDependencyHaskellPath =
    isInfixOf [".hls", "dependencies"]
      . splitDirectories
      . fromNormalizedFilePath

-- | Returns the underlying Normalised File Path of a Typed Rule ONLY if it exists.
someInputFilePathMaybe :: SomeInput -> Maybe NormalizedFilePath
someInputFilePathMaybe input =
  case inputFingerprint input of
    InputFile path -> Just path
    _              -> Nothing

toProjectHaskellInput :: NormalizedFilePath -> Maybe ProjectHaskellInput
toProjectHaskellInput nfp = case toSomeFileInput nfp of
  SomeFileHaskellInput (SomeProjectHaskellInput input) -> Just input
  _                                                    -> Nothing

toNonProjectHaskellInput :: NormalizedFilePath -> Maybe NonProjectHaskellInput
toNonProjectHaskellInput nfp = case toSomeFileInput nfp of
  SomeFileHaskellInput (SomeNonProjectHaskellInput input) -> Just input
  _                                                       -> Nothing

toCabalInput :: NormalizedFilePath -> Maybe CabalInput
toCabalInput nfp = case toSomeFileInput nfp of
  SomeFileCabalInput input -> Just input
  _                        -> Nothing

toSomeHaskellInput :: NormalizedFilePath -> Maybe SomeHaskellInput
toSomeHaskellInput nfp = case toSomeFileInput nfp of
  SomeFileHaskellInput input -> Just input
  _                          -> Nothing

toSomeFileInput :: NormalizedFilePath -> SomeFileInput
toSomeFileInput nfp
  | isCabalInput nfp = SomeFileCabalInput (CabalInput nfp)
  | isProjectHaskellInput nfp = SomeFileHaskellInput (SomeProjectHaskellInput (ProjectHaskellInput nfp))
  | isNonProjectHaskellInput nfp = SomeFileHaskellInput (SomeNonProjectHaskellInput (NonProjectHaskellInput nfp))
  | otherwise = SomeFileNormalizedFilePath nfp

-- ----------------------------------------------------------------------------
-- Classify URI as RuleInputs
-- ----------------------------------------------------------------------------
classifyUri :: Monad m => Uri -> ExceptT PluginError m NormalizedFilePath
classifyUri uri =
  case uriToNormalizedFilePath (toNormalizedUri uri) of
      Just nfp -> pure nfp
      Nothing  -> throwE (PluginUnsupportedUriType uri)

classifyAs
  :: Monad m
  => String
  -> (NormalizedFilePath -> Maybe i)
  -> Uri
  -> ExceptT PluginError m i
classifyAs expected classifier uri = do
  nfp <- classifyUri uri
  case classifier nfp of
    Just input -> pure input
    Nothing ->
        throwE (PluginInvalidParams (T.pack ("Expected " <> expected <> " URI: " <> show uri)))

classifyAsProjectHaskell :: Monad m => Uri -> ExceptT PluginError m ProjectHaskellInput
classifyAsProjectHaskell = classifyAs "project Haskell" toProjectHaskellInput

classifyAsDep :: Monad m => Uri -> ExceptT PluginError m NonProjectHaskellInput
classifyAsDep = classifyAs "dependency Haskell" toNonProjectHaskellInput

classifyAsCabal :: Monad m => Uri -> ExceptT PluginError m CabalInput
classifyAsCabal = classifyAs "cabal" toCabalInput

classifyAsSomeHaskell :: Monad m => Uri -> ExceptT PluginError m SomeHaskellInput
classifyAsSomeHaskell = classifyAs "Haskell" toSomeHaskellInput

classifyAsSomeFile :: Monad m => Uri -> ExceptT PluginError m SomeFileInput
classifyAsSomeFile uri = toSomeFileInput <$> classifyUri uri
