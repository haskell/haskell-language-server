-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.Import.DependencyInformation
  ( DependencyInformation(..)
  , ModuleImports(..)
  , RawDependencyInformation(..)
  , NodeError(..)
  , ModuleParseError(..)
  , TransitiveDependencies(..)
  , FilePathId(..)
  , NamedModuleDep(..)

  , PathIdMap
  , emptyPathIdMap
  , getPathId
  , lookupPathToId
  , insertImport
  , pathToId
  , idToPath
  , reachableModules
  , processDependencyInformation
  , transitiveDeps

  , BootIdMap
  , insertBootId
  ) where

import Control.DeepSeq
import Data.Bifunctor
import Data.Coerce
import Data.List
import Data.Tuple.Extra hiding (first, second)
import Development.IDE.GHC.Orphans()
import Data.Either
import Data.Graph
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.IntMap.Lazy as IntMapLazy
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)

import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.IDE.Import.FindImports (ArtifactsLocation(..))

import GHC
import Module

-- | The imports for a given module.
data ModuleImports = ModuleImports
    { moduleImports :: ![(Located ModuleName, Maybe FilePathId)]
    -- ^ Imports of a module in the current package and the file path of
    -- that module on disk (if we found it)
    , packageImports :: !(Set InstalledUnitId)
    -- ^ Transitive package dependencies unioned for all imports.
    } deriving Show

-- | For processing dependency information, we need lots of maps and sets of
-- filepaths. Comparing Strings is really slow, so we work with IntMap/IntSet
-- instead and only convert at the edges.
newtype FilePathId = FilePathId { getFilePathId :: Int }
  deriving (Show, NFData, Eq, Ord)

-- | Map from 'FilePathId'
type FilePathIdMap = IntMap

-- | Set of 'FilePathId's
type FilePathIdSet = IntSet

data PathIdMap = PathIdMap
  { idToPathMap :: !(FilePathIdMap ArtifactsLocation)
  , pathToIdMap :: !(HashMap NormalizedFilePath FilePathId)
  }
  deriving (Show, Generic)

instance NFData PathIdMap

emptyPathIdMap :: PathIdMap
emptyPathIdMap = PathIdMap IntMap.empty HMS.empty

getPathId :: ArtifactsLocation -> PathIdMap -> (FilePathId, PathIdMap)
getPathId path m@PathIdMap{..} =
    case HMS.lookup (artifactFilePath path) pathToIdMap of
        Nothing ->
            let !newId = FilePathId $ HMS.size pathToIdMap
            in (newId, insertPathId path newId m)
        Just id -> (id, m)

insertPathId :: ArtifactsLocation -> FilePathId -> PathIdMap -> PathIdMap
insertPathId path id PathIdMap{..} =
    PathIdMap (IntMap.insert (getFilePathId id) path idToPathMap) (HMS.insert (artifactFilePath path) id pathToIdMap)

insertImport :: FilePathId -> Either ModuleParseError ModuleImports -> RawDependencyInformation -> RawDependencyInformation
insertImport (FilePathId k) v rawDepInfo = rawDepInfo { rawImports = IntMap.insert k v (rawImports rawDepInfo) }

pathToId :: PathIdMap -> NormalizedFilePath -> FilePathId
pathToId PathIdMap{pathToIdMap} path = pathToIdMap HMS.! path

lookupPathToId :: PathIdMap -> NormalizedFilePath -> Maybe FilePathId
lookupPathToId PathIdMap{pathToIdMap} path = HMS.lookup path pathToIdMap

idToPath :: PathIdMap -> FilePathId -> NormalizedFilePath
idToPath pathIdMap filePathId = artifactFilePath $ idToModLocation pathIdMap filePathId

idToModLocation :: PathIdMap -> FilePathId -> ArtifactsLocation
idToModLocation PathIdMap{idToPathMap} (FilePathId id) = idToPathMap IntMap.! id

type BootIdMap = FilePathIdMap FilePathId

insertBootId :: FilePathId -> FilePathId -> BootIdMap -> BootIdMap
insertBootId k = IntMap.insert (getFilePathId k)

-- | Unprocessed results that we find by following imports recursively.
data RawDependencyInformation = RawDependencyInformation
    { rawImports :: !(FilePathIdMap (Either ModuleParseError ModuleImports))
    , rawPathIdMap :: !PathIdMap
    -- The rawBootMap maps the FilePathId of a hs-boot file to its
    -- corresponding hs file. It is used when topologically sorting as we
    -- need to add edges between .hs-boot and .hs so that the .hs files
    -- appear later in the sort.
    , rawBootMap :: !BootIdMap
    } deriving Show

pkgDependencies :: RawDependencyInformation -> FilePathIdMap (Set InstalledUnitId)
pkgDependencies RawDependencyInformation{..} =
    IntMap.map (either (const Set.empty) packageImports) rawImports

data DependencyInformation =
  DependencyInformation
    { depErrorNodes :: !(FilePathIdMap (NonEmpty NodeError))
    -- ^ Nodes that cannot be processed correctly.
    , depModuleNames :: !(FilePathIdMap ShowableModuleName)
    , depModuleDeps :: !(FilePathIdMap FilePathIdSet)
    -- ^ For a non-error node, this contains the set of module immediate dependencies
    -- in the same package.
    , depPkgDeps :: !(FilePathIdMap (Set InstalledUnitId))
    -- ^ For a non-error node, this contains the set of immediate pkg deps.
    , depPathIdMap :: !PathIdMap
    -- ^ Map from FilePath to FilePathId
    , depBootMap :: !BootIdMap
    -- ^ Map from hs-boot file to the corresponding hs file
    } deriving (Show, Generic)

newtype ShowableModuleName =
  ShowableModuleName {showableModuleName :: ModuleName}
  deriving NFData

instance Show ShowableModuleName where show = moduleNameString . showableModuleName

reachableModules :: DependencyInformation -> [NormalizedFilePath]
reachableModules DependencyInformation{..} =
    map (idToPath depPathIdMap . FilePathId) $ IntMap.keys depErrorNodes <> IntMap.keys depModuleDeps

instance NFData DependencyInformation

-- | This does not contain the actual parse error as that is already reported by GetParsedModule.
data ModuleParseError = ModuleParseError
  deriving (Show, Generic)

instance NFData ModuleParseError

-- | Error when trying to locate a module.
data LocateError = LocateError [Diagnostic]
  deriving (Eq, Show, Generic)

instance NFData LocateError

-- | An error attached to a node in the dependency graph.
data NodeError
  = PartOfCycle (Located ModuleName) [FilePathId]
  -- ^ This module is part of an import cycle. The module name corresponds
  -- to the import that enters the cycle starting from this module.
  -- The list of filepaths represents the elements
  -- in the cycle in unspecified order.
  | FailedToLocateImport (Located ModuleName)
  -- ^ This module has an import that couldn’t be located.
  | ParseError ModuleParseError
  | ParentOfErrorNode (Located ModuleName)
  -- ^ This module is the parent of a module that cannot be
  -- processed (either it cannot be parsed, is part of a cycle
  -- or the parent of another error node).
  deriving (Show, Generic)

instance NFData NodeError where
  rnf (PartOfCycle m fs) = m `seq` rnf fs
  rnf (FailedToLocateImport m) = m `seq` ()
  rnf (ParseError e) = rnf e
  rnf (ParentOfErrorNode m) = m `seq` ()

-- | A processed node in the dependency graph. If there was any error
-- during processing the node or any of its dependencies, this is an
-- `ErrorNode`. Otherwise it is a `SuccessNode`.
data NodeResult
  = ErrorNode (NonEmpty NodeError)
  | SuccessNode [(Located ModuleName, FilePathId)]
  deriving Show

partitionNodeResults
    :: [(a, NodeResult)]
    -> ([(a, NonEmpty NodeError)], [(a, [(Located ModuleName, FilePathId)])])
partitionNodeResults = partitionEithers . map f
  where f (a, ErrorNode errs) = Left (a, errs)
        f (a, SuccessNode imps) = Right (a, imps)

instance Semigroup NodeResult where
   ErrorNode errs <> ErrorNode errs' = ErrorNode (errs <> errs')
   ErrorNode errs <> SuccessNode _ = ErrorNode errs
   SuccessNode _ <> ErrorNode errs = ErrorNode errs
   SuccessNode a <> SuccessNode _ = SuccessNode a

processDependencyInformation :: RawDependencyInformation -> DependencyInformation
processDependencyInformation rawDepInfo@RawDependencyInformation{..} =
  DependencyInformation
    { depErrorNodes = IntMap.fromList errorNodes
    , depModuleDeps = moduleDeps
    , depModuleNames = IntMap.fromList $ coerce moduleNames
    , depPkgDeps = pkgDependencies rawDepInfo
    , depPathIdMap = rawPathIdMap
    , depBootMap = rawBootMap
    }
  where resultGraph = buildResultGraph rawImports
        (errorNodes, successNodes) = partitionNodeResults $ IntMap.toList resultGraph
        moduleNames :: [(FilePathId, ModuleName)]
        moduleNames =
          [ (fId, modName) | (_, imports) <- successNodes, (L _ modName, fId) <- imports]
        successEdges :: [(FilePathId, FilePathId, [FilePathId])]
        successEdges =
            map
              (\(file, imports) -> (FilePathId file, FilePathId file, map snd imports))
              successNodes
        moduleDeps =
          IntMap.fromList $
          map (\(_, FilePathId v, vs) -> (v, IntSet.fromList $ coerce vs))
            successEdges

-- | Given a dependency graph, buildResultGraph detects and propagates errors in that graph as follows:
-- 1. Mark each node that is part of an import cycle as an error node.
-- 2. Mark each node that has a parse error as an error node.
-- 3. Mark each node whose immediate children could not be located as an error.
-- 4. Recursively propagate errors to parents if they are not already error nodes.
buildResultGraph :: FilePathIdMap (Either ModuleParseError ModuleImports) -> FilePathIdMap NodeResult
buildResultGraph g = propagatedErrors
    where
        sccs = stronglyConnComp (graphEdges g)
        (_, cycles) = partitionSCC sccs
        cycleErrors :: IntMap NodeResult
        cycleErrors = IntMap.unionsWith (<>) $ map errorsForCycle cycles
        errorsForCycle :: [FilePathId] -> IntMap NodeResult
        errorsForCycle files =
          IntMap.fromListWith (<>) $ coerce $ concatMap (cycleErrorsForFile files) files
        cycleErrorsForFile :: [FilePathId] -> FilePathId -> [(FilePathId,NodeResult)]
        cycleErrorsForFile cycle f =
          let entryPoints = mapMaybe (findImport f) cycle
          in map (\imp -> (f, ErrorNode (PartOfCycle imp cycle :| []))) entryPoints
        otherErrors = IntMap.map otherErrorsForFile g
        otherErrorsForFile :: Either ModuleParseError ModuleImports -> NodeResult
        otherErrorsForFile (Left err) = ErrorNode (ParseError err :| [])
        otherErrorsForFile (Right ModuleImports{moduleImports}) =
          let toEither (imp, Nothing) = Left imp
              toEither (imp, Just path) = Right (imp, path)
              (errs, imports') = partitionEithers (map toEither moduleImports)
          in case nonEmpty errs of
            Nothing -> SuccessNode imports'
            Just errs' -> ErrorNode (NonEmpty.map FailedToLocateImport errs')

        unpropagatedErrors = IntMap.unionWith (<>) cycleErrors otherErrors
        -- The recursion here is fine since we use a lazy map and
        -- we only recurse on SuccessNodes. In particular, we do not recurse
        -- on nodes that are part of a cycle as they are already marked as
        -- error nodes.
        propagatedErrors =
          IntMapLazy.map propagate unpropagatedErrors
        propagate :: NodeResult -> NodeResult
        propagate n@(ErrorNode _) = n
        propagate n@(SuccessNode imps) =
          let results = map (\(imp, FilePathId dep) -> (imp, propagatedErrors IntMap.! dep)) imps
              (errs, _) = partitionNodeResults results
          in case nonEmpty errs of
               Nothing -> n
               Just errs' -> ErrorNode (NonEmpty.map (ParentOfErrorNode . fst) errs')
        findImport :: FilePathId -> FilePathId -> Maybe (Located ModuleName)
        findImport (FilePathId file) importedFile =
          case g IntMap.! file of
            Left _ -> error "Tried to call findImport on a module with a parse error"
            Right ModuleImports{moduleImports} ->
              fmap fst $ find (\(_, resolvedImp) -> resolvedImp == Just importedFile) moduleImports

graphEdges :: FilePathIdMap (Either ModuleParseError ModuleImports) -> [(FilePathId, FilePathId, [FilePathId])]
graphEdges g =
  map (\(k, v) -> (FilePathId k, FilePathId k, deps v)) $ IntMap.toList g
  where deps :: Either e ModuleImports -> [FilePathId]
        deps (Left _) = []
        deps (Right ModuleImports{moduleImports}) = mapMaybe snd moduleImports

partitionSCC :: [SCC a] -> ([a], [[a]])
partitionSCC (CyclicSCC xs:rest) = second (xs:) $ partitionSCC rest
partitionSCC (AcyclicSCC x:rest) = first (x:)   $ partitionSCC rest
partitionSCC []                  = ([], [])


transitiveDeps :: DependencyInformation -> NormalizedFilePath -> Maybe TransitiveDependencies
transitiveDeps DependencyInformation{..} file = do
  let !fileId = pathToId depPathIdMap file
  reachableVs <-
      -- Delete the starting node
      IntSet.delete (getFilePathId fileId) .
      IntSet.fromList . map (fst3 . fromVertex) .
      reachable g <$> toVertex (getFilePathId fileId)
  let transitiveModuleDepIds =
        filter (\v -> v `IntSet.member` reachableVs) $ map (fst3 . fromVertex) vs
  let transitivePkgDeps =
          Set.toList $ Set.unions $
          map (\f -> IntMap.findWithDefault Set.empty f depPkgDeps) $
          getFilePathId fileId : transitiveModuleDepIds
  let transitiveModuleDeps =
        map (idToPath depPathIdMap . FilePathId) transitiveModuleDepIds
  let transitiveNamedModuleDeps =
        [ NamedModuleDep (idToPath depPathIdMap (FilePathId fid)) mn artifactModLocation
        | (fid, ShowableModuleName mn) <- IntMap.toList depModuleNames
        , let ArtifactsLocation{artifactModLocation} = idToPathMap depPathIdMap IntMap.! fid
        ]
  pure TransitiveDependencies {..}
  where
    (g, fromVertex, toVertex) = graphFromEdges edges
    edges = map (\(f, fs) -> (f, f, IntSet.toList fs ++ boot_edge f)) $ IntMap.toList depModuleDeps

    -- Need to add an edge between the .hs and .hs-boot file if it exists
    -- so the .hs file gets loaded after the .hs-boot file and the right
    -- stuff ends up in the HPT. If you don't have this check then GHC will
    -- fail to work with ghcide.
    boot_edge f = [getFilePathId f' | Just f' <- [IntMap.lookup f depBootMap]]

    vs = topSort g

data TransitiveDependencies = TransitiveDependencies
  { transitiveModuleDeps :: [NormalizedFilePath]
  -- ^ Transitive module dependencies in topological order.
  -- The module itself is not included.
  , transitiveNamedModuleDeps :: [NamedModuleDep]
  -- ^ Transitive module dependencies in topological order.
  -- The module itself is not included.
  , transitivePkgDeps :: [InstalledUnitId]
  -- ^ Transitive pkg dependencies in unspecified order.
  } deriving (Eq, Show, Generic)

instance NFData TransitiveDependencies

data NamedModuleDep = NamedModuleDep {
  nmdFilePath :: !NormalizedFilePath,
  nmdModuleName :: !ModuleName,
  nmdModLocation :: !ModLocation
  }
  deriving Generic

instance Eq NamedModuleDep where
  a == b = nmdFilePath a == nmdFilePath b

instance NFData NamedModuleDep where
  rnf NamedModuleDep{..} =
    rnf nmdFilePath `seq`
    rnf nmdModuleName `seq`
    -- 'ModLocation' lacks an 'NFData' instance
    rwhnf nmdModLocation

instance Show NamedModuleDep where
  show NamedModuleDep{..} = show nmdFilePath

