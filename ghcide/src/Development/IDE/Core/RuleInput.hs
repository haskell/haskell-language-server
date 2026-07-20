-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Development.IDE.Core.RuleInput
    ( RuleInput
    , InputFingerprint(..)
    , ProjectHaskellInput
    , NonProjectHaskellInput
    , SomeHaskellInput
    , CabalInput
    , SomeFileInput
    , NoInput
    , SomeInput
    , IsInput(..)
    , fileInputFingerprint
    , isHaskellFilePath
    , isDependencyHaskellPath
    , someInputFilePath
    , toProjectHaskellInput
    , toNonProjectHaskellInput
    , toCabalInput
    , toSomeHaskellInput
    , toSomeFileInput
    , classifyAsHaskell
    , classifyAsDep
    , classifyAsCabal
    , classifyAsSomeHaskell
    , classifyAsSomeFile
    ) where

import           Control.DeepSeq
import           Control.Monad.Trans.Except            (ExceptT, throwE)
import           Data.Hashable
import           Data.List                             (isInfixOf)
import qualified Data.Text                             as T
import           Data.Typeable
import           GHC.Generics                          (Generic)
import           Ide.Plugin.Error                      (PluginError (..))
import           Language.LSP.Protocol.Types           (NormalizedFilePath,
                                                        Uri,
                                                        emptyNormalizedFilePath,
                                                        fromNormalizedFilePath,
                                                        toNormalizedUri,
                                                        uriToNormalizedFilePath)
import           System.FilePath                       (normalise,
                                                        takeExtension)

-- | Map each rule with an input type consumed by that rule
type family RuleInput k

-- | Describes identity which is used to compare and hash rule inputs
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
  hashWithSalt s InputNoFile = hashWithSalt s (0 :: Int)
  hashWithSalt s (InputFile p) = hashWithSalt s (1 :: Int, p)
  hashWithSalt s (InputValue a) = hashWithSalt s (2 :: Int, hash a)

-- RuleInput : IsInput and SomeInput
-- | Defines a valid rule input with a fingerprint
class (Typeable i, Hashable i, Eq i, Show i, NFData i) => IsInput i where
    toInput :: i -> SomeInput
    toInput = SomeInput

    fromInput :: SomeInput -> Maybe i
    fromInput (SomeInput i) = cast i


    -- should not exist
    inputFingerprint :: i -> InputFingerprint
    inputFingerprint i = InputValue i

-- | Stores value that implements 'IsInput'.
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

-- RuleInput : NoInput
-- | A valid Rule Input that has no file associated with it.
data NoInput = NoInput
    deriving (Eq, Show, Generic)
instance Hashable NoInput
instance NFData NoInput

instance IsInput NoInput where
    inputFingerprint _ = InputNoFile

-- RuleInput : IsFileInput and SomeFileInput
-- | A Rule Input that has some file (Haskell, cabal etc.) associated with it.
class IsInput i => IsFileInput i where
    fileInputPath :: i -> NormalizedFilePath

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
    inputFingerprint = fileInputFingerprint
instance IsFileInput SomeFileInput where
    fileInputPath (SomeFileHaskellInput input) = fileInputPath input
    fileInputPath (SomeFileCabalInput input) = fileInputPath input
    fileInputPath (SomeFileNormalizedFilePath input) = fileInputPath input
instance IsInput NormalizedFilePath where
    inputFingerprint = InputFile
instance IsFileInput NormalizedFilePath where
    fileInputPath = id

-- | Fingerprint a file input by its normalized file path.
fileInputFingerprint :: IsFileInput i => i -> InputFingerprint
fileInputFingerprint input = InputFile (fileInputPath input)

-- RuleInput : CabalInput
-- | Leaf Type which represents a cabal file.
newtype CabalInput = CabalInput NormalizedFilePath
  deriving (Eq, Show, Generic)
instance Hashable CabalInput
instance NFData CabalInput

instance IsInput CabalInput where
    inputFingerprint = fileInputFingerprint
instance IsFileInput CabalInput where
  fileInputPath (CabalInput path) = path

-- RuleInput : IsHaskellInput and SomeHaskellInput
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
    inputFingerprint = fileInputFingerprint
instance IsFileInput SomeHaskellInput where
  fileInputPath (SomeProjectHaskellInput input) = fileInputPath input
  fileInputPath (SomeNonProjectHaskellInput input) = fileInputPath input

instance IsHaskellInput SomeHaskellInput
-- RuleInput : ProjectHaskellInput
-- | Leaf Type representing a Haskell file inside project directory.
newtype ProjectHaskellInput = ProjectHaskellInput NormalizedFilePath
  deriving (Eq, Show, Generic)
instance Hashable ProjectHaskellInput
instance NFData ProjectHaskellInput

instance IsInput ProjectHaskellInput where
    inputFingerprint = fileInputFingerprint
instance IsFileInput ProjectHaskellInput where
  fileInputPath (ProjectHaskellInput path) = path

instance IsHaskellInput ProjectHaskellInput
-- RuleInput : NonProjectHaskellInput
-- | Leaf Type representing a Haskell file inside project .hls/dependencies directory.
newtype NonProjectHaskellInput = NonProjectHaskellInput NormalizedFilePath
  deriving (Eq, Show, Generic)
instance Hashable NonProjectHaskellInput
instance NFData NonProjectHaskellInput

instance IsInput NonProjectHaskellInput where
    inputFingerprint = fileInputFingerprint
instance IsFileInput NonProjectHaskellInput where
  fileInputPath (NonProjectHaskellInput path) = path

instance IsHaskellInput NonProjectHaskellInput

-- Helpers to classify NormalizedFilePaths to appropriate Typed Rule.
isCabalInput :: NormalizedFilePath -> Bool
isCabalInput = (== ".cabal") . takeExtension . fromNormalizedFilePath

isHaskellFilePath :: NormalizedFilePath -> Bool
isHaskellFilePath fp = takeExtension (fromNormalizedFilePath fp) `elem`
    [".hs", ".lhs", ".hs-boot", ".lhs-boot"]

isNonProjectHaskellInput :: NormalizedFilePath -> Bool
isNonProjectHaskellInput fp = isHaskellFilePath fp && isDependencyHaskellPath fp

isProjectHaskellInput :: NormalizedFilePath -> Bool
isProjectHaskellInput fp = isHaskellFilePath fp && not (isDependencyHaskellPath fp)

isDependencyHaskellPath :: NormalizedFilePath -> Bool
isDependencyHaskellPath = (".hls/dependencies" `isInfixOf`) . normalise . fromNormalizedFilePath

-- | Returns the underlying Normalised File Path of a Typed Rules ONLY if it exists
someInputFilePath :: SomeInput -> Maybe NormalizedFilePath
someInputFilePath input =
    case inputFingerprint input of
        InputFile path -> Just path
        _ -> Nothing

-- | Returns the underlying Normalised File Path of a Typed Rules.
-- Returns Empty nfp if no such path found.
-- Not reccomended to use !
someInputFilePath' :: SomeInput -> NormalizedFilePath
someInputFilePath' input =
    case inputFingerprint input of
        InputFile path -> path
        _ -> emptyNormalizedFilePath

-- Helpers that convert NFP to typed rules
toProjectHaskellInput :: NormalizedFilePath -> Maybe ProjectHaskellInput
toProjectHaskellInput nfp
    | isProjectHaskellInput nfp = Just (ProjectHaskellInput nfp)
    | otherwise = Nothing

toNonProjectHaskellInput :: NormalizedFilePath -> Maybe NonProjectHaskellInput
toNonProjectHaskellInput nfp
    | isNonProjectHaskellInput nfp = Just (NonProjectHaskellInput nfp)
    | otherwise = Nothing

toCabalInput :: NormalizedFilePath -> Maybe CabalInput
toCabalInput nfp
    | isCabalInput nfp = Just (CabalInput nfp)
    | otherwise = Nothing

toSomeHaskellInput :: NormalizedFilePath -> Maybe SomeHaskellInput
toSomeHaskellInput nfp
    | isNonProjectHaskellInput nfp = Just (SomeNonProjectHaskellInput (NonProjectHaskellInput nfp))
    | isProjectHaskellInput nfp = Just (SomeProjectHaskellInput (ProjectHaskellInput nfp))
    | otherwise = Nothing

toSomeFileInput :: NormalizedFilePath -> SomeFileInput
toSomeFileInput nfp
    | isCabalInput nfp = SomeFileCabalInput (CabalInput nfp)
    | isProjectHaskellInput nfp = SomeFileHaskellInput (SomeProjectHaskellInput (ProjectHaskellInput nfp))
    | isNonProjectHaskellInput nfp = SomeFileHaskellInput (SomeNonProjectHaskellInput (NonProjectHaskellInput nfp))
    | otherwise = SomeFileNormalizedFilePath nfp

-- Helpers that classify URIs to typed rules
classifyUri :: Monad m => Uri -> ExceptT PluginError m NormalizedFilePath
classifyUri uri =
    case uriToNormalizedFilePath (toNormalizedUri uri) of
        Just nfp -> pure nfp
        Nothing -> throwE (PluginUnsupportedUriType uri)

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

classifyAsHaskell :: Monad m => Uri -> ExceptT PluginError m ProjectHaskellInput
classifyAsHaskell = classifyAs "project Haskell" toProjectHaskellInput

classifyAsDep :: Monad m => Uri -> ExceptT PluginError m NonProjectHaskellInput
classifyAsDep = classifyAs "dependency Haskell" toNonProjectHaskellInput

classifyAsCabal :: Monad m => Uri -> ExceptT PluginError m CabalInput
classifyAsCabal = classifyAs "cabal" toCabalInput

classifyAsSomeHaskell :: Monad m => Uri -> ExceptT PluginError m SomeHaskellInput
classifyAsSomeHaskell = classifyAs "Haskell" toSomeHaskellInput

classifyAsSomeFile :: Monad m => Uri -> ExceptT PluginError m SomeFileInput
classifyAsSomeFile uri = toSomeFileInput <$> classifyUri uri
