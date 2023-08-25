{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Ide.Plugin.Cabal.CodeActions where

import           Control.Lens                                 ((^.))
import           Data.Foldable                                (Foldable (foldl'))
import           Data.Foldable.Extra                          (asum)
import qualified Data.Map                                     as Map
import           Data.Maybe                                   (fromMaybe,
                                                               mapMaybe)
import qualified Data.Text                                    as T
import           Ide.Logger
import           Ide.Plugin.Cabal.Completion.Completer.Module (fpToExposedModulePath)
import qualified Language.LSP.Protocol.Lens                   as L

import qualified Language.LSP.Protocol.Types                  as CA (CodeAction (..),
                                                                     CodeActionKind (..))
import qualified Language.LSP.Protocol.Types                  as J
import qualified Language.LSP.Protocol.Types                  as P (Position (..),
                                                                    Range (..))
import qualified Language.LSP.Protocol.Types                  as TE (TextEdit (..))
import qualified Language.LSP.Protocol.Types                  as WE (WorkspaceEdit (..))
import           Safe                                         (headMay)

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except
import           Data.Char                                    (isUpper)
import           Data.List.Extra                              (notNull)
import           Development.IDE
import           Development.IDE.Core.PluginUtils
import           Ide.Plugin.Error
import qualified System.FilePath                              as FP
import           Text.Cabal.Types                             as Parser

------------------------------------------
-- Types
------------------------------------------

data Log = LogString
  deriving (Ord, Eq, Show)

instance Pretty Log where
    pretty = \case
      LogString -> ""

{- | Contains information on how the new module path
  should be inserted into the cabal file.
-}
data ModulePathInsertionInfo = ModulePathInsertionInfo
  { insertionPosition :: Maybe P.Position
  -- ^ The position to insert the path at.
  , insertionMode     :: InsertionMode
  -- ^ Whether we are inserting directly in front or after another value.
  }
  deriving (Ord, Eq, Show)

{- | Whether the module path is going to be inserted
  in front or after another value.
-}
data InsertionMode = Before | After
  deriving (Ord, Eq, Show)

{- | Whether the module path is going to be inserted into
  its own line or in the same line as the other module paths
  in the field.

  If the path is to be inserted into its own line,
  the indentation of the path to be inserted is also stored.
-}
data LineInformation = OwnLine Indentation | SameLine
  deriving (Ord, Eq, Show)

type Indentation = Int

-- | Relevant data needed to add a module to a cabal file.
data ModuleInsertionConfig = ModuleInsertionConfig
  { targetFile      :: FilePath
  -- ^ The file we want to insert information about the new module into.
  , moduleToInsert  :: T.Text
  -- ^ The text to insert into the targetFile at the insertionPosition.
  , insertionPos    :: P.Position
  -- ^ The position where the module path will be inserted into the targetFile.
  , insertionStanza :: T.Text
  -- ^ A label which describes which stanza the module will be inserted into.
  , insertionField  :: T.Text
  -- ^ A label which describes which field the module will be inserted into.
  }
  deriving (Show, Eq, Ord)

--------------------------------------------
-- Implementation
--------------------------------------------

{- | Takes a path to a cabal file, a module path in exposed module syntax
  and the contents of the cabal file and generates all possible
  code actions for inserting the module path into the cabal file
  with the given contents.
-}
collectModuleInsertionOptions ::
  (MonadIO m) =>
  Recorder (WithPriority Log) ->
  FilePath ->
  Uri ->
  CabalAST ->
  ExceptT PluginError m [J.CodeAction]
collectModuleInsertionOptions _ srcPath modulePathUri ast = do
  modulePath <- uriToFilePathE modulePathUri
  let configs = concatMap (mkModuleInsertionConfig srcPath modulePath) (stanzas ast)
  pure $ map makeCodeActionForModulePath configs

{- | Takes the path to the cabal file to insert the module into,
  the module path to be inserted, and a stanza representation.

  Returns a list of module insertion configs where each config
  represents a position the module path could be inserted in the
  given stanza.
-}
mkModuleInsertionConfig :: FilePath -> FilePath -> StanzaItem -> [ModuleInsertionConfig]
mkModuleInsertionConfig srcPath modulePath (Parser.Stanza (StanzaDecl (StanzaType sType _) sNameM _) (StanzaElements fields _) _) = do
  case getModulePathForStanza fields srcPath modulePath of
    Just processedModPath ->
      mapMaybe
        ( \sField ->
            valuesToInsertionConfig
              ( case sField of
                  (StanzaField field) ->
                    if isModuleField field then Just field else Nothing
                  _ -> Nothing
                  -- TODO: some conditionals may apply so we may want to
                  -- choose these fields as well in the future
              )
              srcPath
              processedModPath
              label
        )
        fields
    _ -> []
 where
  label =
    sType <> case sNameM of
      Just (StanzaName sName _) -> " " <> sName
      Nothing ->
        ""

{- | Takes a list of stanza elements, a source path and the module path to add
  and returns a module path to insert which is relative to some source directory in the
  given stanza.
  If the module is contained in one of the stanza's fields' source directories,
  then a module path to insert is returned.

  For each source directory occurring in the given stanza we try create the module path
  using the source directory and if it matches we return the first matching one, in
  module path syntax and relative to the source directory.
-}
getModulePathForStanza :: [StanzaElement] -> FilePath -> FilePath -> Maybe T.Text
getModulePathForStanza stanzaElements sourceDir modPath =
  case mkModulePathM stanzaSourceDirs sourceDir modPath of
    Just fp ->
      if all (maybe False isUpper . headMay) $ FP.splitDirectories fp
        then Just $ fpToExposedModulePath sourceDir fp
        else Nothing
    Nothing -> Nothing
 where
  -- a list of all possible source directories of the stanza,
  -- either all values in the hs-source-dir field
  -- or simple the directory of the cabal file in case there are no
  -- hs-source-dir-fields
  stanzaSourceDirs =
    if notNull $ filterSourceDirs stanzaElements
      then map (\(Value val _) -> T.unpack val) $ concat $ filterSourceDirs stanzaElements
      else [FP.takeDirectory sourceDir]

  -- gathers all source directory value value items in the given stanza elements
  filterSourceDirs :: [StanzaElement] -> [[ValueItem]]
  filterSourceDirs sElems =
    mapMaybe
      ( \case
          (StanzaField field@(Field _ (Values vals _) _)) -> if hasFieldType "hs-source-dirs" field then Just vals else Nothing
          _ -> Nothing
      )
      sElems

{- | Takes a list of source subdirectories, a source path and a module path
  and returns a file path to the module relative to one of the subdirectories in
  case the module is contained within one of them.
-}
mkModulePathM :: [FilePath] -> FilePath -> FilePath -> Maybe FilePath
mkModulePathM filepaths srcPath' modPath =
  asum $
    map
      ( \srcDir -> do
            let relMP = FP.makeRelative (FP.normalise (srcPath FP.</> srcDir)) modPath
            if relMP == modPath then Nothing else Just relMP
      )
      filepaths
 where
  srcPath = FP.takeDirectory srcPath'

{- | Takes a possible field item, a cabal file path where the module is to be inserted,
  a module path to insert and a label to use to label the text edit and constructs the
  ModuleInsertionConfig.
-}
valuesToInsertionConfig ::
  Maybe FieldItem ->
  FilePath ->
  T.Text ->
  T.Text ->
  Maybe ModuleInsertionConfig
valuesToInsertionConfig Nothing _ _ _ = Nothing
valuesToInsertionConfig
  (Just (Field (Parser.KeyWord kw (Annotation _ kwRange)) (Values vals (Annotation _ range)) _))
  fp
  modPath
  label' =
    Just $
      ModuleInsertionConfig
        { targetFile = fp
        , moduleToInsert = paddedModulePath
        , insertionPos =
            case insertionPosition mps of
              Just pos -> pos
              Nothing  -> range ^. L.start
        , insertionStanza = label'
        , insertionField = kw
        }
   where
    paddedModulePath =
      case mkLineInformation vals kwRange of
        SameLine ->
          case insertionMode mps of
            Before -> modPath <> " "
            After  -> " " <> modPath
        OwnLine indents ->
          case insertionMode mps of
            Before -> modPath <> "\n" <> genIndentation indents
            After  -> "\n" <> genIndentation indents <> modPath

    mps = modulePathInsertionPosition vals modPath

    genIndentation :: Int -> T.Text
    genIndentation = (`T.replicate` " ") . fromIntegral

-- | Returns a list of all stanza items in the given AST.
stanzas :: CabalAST -> [StanzaItem]
stanzas (CabalAST ast _) =
  mapMaybe
    ( \case
        (StanzaItem x) -> Just x
        _ -> Nothing
    )
    ast

{- | Takes a list of value items and the range of the values' keyword item
  and returns the line information for that list.

  The line information is set to OwnLine iff all values in the list are in
  different lines and the indentation is set to the indentation of the first
  value in the list.
-}
mkLineInformation :: [ValueItem] -> P.Range -> LineInformation
mkLineInformation vs kwRange
  | snd allDiffLines =
      case headMay vs of
        Just (Value _ (Annotation _ r)) -> OwnLine (fromIntegral $ r ^. (L.start . L.character))
        Nothing -> OwnLine $ 2 + fromIntegral (kwRange ^. (L.start . L.character))
  | otherwise = SameLine
 where
  allDiffLines =
    foldl'
      ( \(sPos, allDiff) (Value _ (Annotation _ r1)) ->
          let curLine = r1 ^. (L.start . L.line)
           in (curLine, allDiff && (curLine /= sPos))
      )
      (-1, True)
      vs

{- | Takes a list of value items and a module path and finds the correct position in the list
  to add the path in ascending alphabetical order and whether to add the value directly before
  the next value in the list or directly after the previous value in the list.

  In most cases the value will be added after the previous value in the list, unless
  it is the first value in the list, then it will be added directly before the
  second value in the list.

  Assumes the existing list of values is already sorted in ascending alphabetical order,
  if not, the value is just added before the first found item which is alphabetically larger.
-}
modulePathInsertionPosition :: [ValueItem] -> T.Text -> ModulePathInsertionInfo
modulePathInsertionPosition [] _ = ModulePathInsertionInfo{insertionPosition = Nothing, insertionMode = After}
modulePathInsertionPosition [v1@(Value val1 _)] modPath
  | getModuleName val1 <= modPath = mkModulePathInsertionInfo v1 After
  | otherwise = mkModulePathInsertionInfo v1 Before
modulePathInsertionPosition (v1@(Value val1 _) : v2@(Value val2 _) : xs) modPath
  | modPath <= getModuleName val1 = mkModulePathInsertionInfo v1 Before
  | getModuleName val1 <= modPath && modPath < getModuleName val2 = mkModulePathInsertionInfo v1 After
  | otherwise = modulePathInsertionPosition (v2 : xs) modPath

{- | Builds a Module Path Insertion Info from the received value item and insertion mode.

  Depending on the insertion mode, will set the insertion position right before the received
  value or right after the received value.
-}
mkModulePathInsertionInfo :: ValueItem -> InsertionMode -> ModulePathInsertionInfo
mkModulePathInsertionInfo (Value _ (Annotation _ r)) iMode =
  ModulePathInsertionInfo
    { insertionPosition =
        case iMode of
          Before -> Just $ r ^. L.start
          After  -> Just $ r ^. L.end
    , insertionMode = iMode
    }

{- | Removes anything that is not the module name from a given value
  which can contain a comma at the start or end and spaces.
-}
getModuleName :: T.Text -> T.Text
getModuleName mPath = T.strip $ (T.dropWhile (== ',') . T.dropWhileEnd (== ',')) mPath

-- | Returns whether the given field item represents a field where a module path can be inserted.
isModuleField :: FieldItem -> Bool
isModuleField f =
  any (\modKw -> hasFieldType modKw f) ["exposed-modules", "other-modules"]

makeCodeActionForModulePath :: ModuleInsertionConfig -> J.CodeAction
makeCodeActionForModulePath insertionConfig =
  J.CodeAction
    { CA._title = "Add to " <> label <> " as " <> fieldDescription <> " in " <> (T.pack $ FP.takeFileName cabalFilePath)
    , CA._kind = Just CA.CodeActionKind_Refactor
    , CA._diagnostics = Nothing
    , CA._isPreferred = Nothing
    , CA._disabled = Nothing
    , CA._edit = Just workSpaceEdit
    , CA._command = Nothing
    , CA._data_ = Nothing
    }
 where
  fieldName = insertionField insertionConfig
  fieldDescription = fromMaybe fieldName $ T.stripSuffix "s:" fieldName
  cabalFilePath = targetFile insertionConfig
  pos = insertionPos insertionConfig
  label = insertionStanza insertionConfig
  moduleToAdd = moduleToInsert insertionConfig
  workSpaceEdit =
    WE.WorkspaceEdit
      { WE._changes =
          Just $
            Map.singleton
              (J.filePathToUri cabalFilePath)
              [TE.TextEdit{TE._range = P.Range pos pos, TE._newText = moduleToAdd}]
      , WE._documentChanges = Nothing
      , WE._changeAnnotations = Nothing
      }
