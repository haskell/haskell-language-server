-- Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

module Development.IDE.Functions.DependencyInformation
  ( DependencyInformation(..)
  , RawDependencyInformation(..)
  , NodeError(..)
  , ModuleParseError(..)
  , TransitiveDependencies(..)
  , processDependencyInformation
  , transitiveDeps
  ) where

import Control.DeepSeq
import Data.Bifunctor
import Development.IDE.Orphans()
import Data.Either
import Data.Foldable
import Data.Graph
import Data.List.NonEmpty (NonEmpty(..), nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map.Strict as MS
import qualified Data.Map.Lazy as ML
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (fst3)
import GHC.Generics (Generic)

import Development.IDE.Types.Diagnostics
import Development.IDE.Types.Location
import Development.IDE.UtilGHC ()

import GHC
import Module

-- | Unprocessed results that we get from following all imports recursively starting from a module.
data RawDependencyInformation = RawDependencyInformation
  { moduleDependencies :: Map NormalizedFilePath (Either ModuleParseError [(Located ModuleName, Maybe NormalizedFilePath)])
  , pkgDependencies :: Map NormalizedFilePath (Set InstalledUnitId)
  -- ^ Transitive dependencies on pkgs of this file, i.e. immidiate package dependencies and the
  -- transitive package dependencies of those packages.
  }

data DependencyInformation =
  DependencyInformation
    { depErrorNodes :: Map NormalizedFilePath (NonEmpty NodeError)
    -- ^ Nodes that cannot be processed correctly.
    , depModuleDeps :: Map NormalizedFilePath (Set NormalizedFilePath)
    -- ^ For a non-error node, this contains the set of module immediate dependencies
    -- in the same package.
    , depPkgDeps :: Map NormalizedFilePath (Set InstalledUnitId)
    -- ^ For a non-error node, this contains the set of immediate pkg deps.
    } deriving (Show, Generic)

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
  = PartOfCycle (Located ModuleName) [NormalizedFilePath]
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
  | SuccessNode [(Located ModuleName, NormalizedFilePath)]
  deriving Show

partitionNodeResults
    :: [(a, NodeResult)]
    -> ([(a, NonEmpty NodeError)], [(a, [(Located ModuleName, NormalizedFilePath)])])
partitionNodeResults = partitionEithers . map f
  where f (a, ErrorNode errs) = Left (a, errs)
        f (a, SuccessNode imps) = Right (a, imps)

instance Semigroup NodeResult where
   ErrorNode errs <> ErrorNode errs' = ErrorNode (errs <> errs')
   ErrorNode errs <> SuccessNode _ = ErrorNode errs
   SuccessNode _ <> ErrorNode errs = ErrorNode errs
   SuccessNode a <> SuccessNode _ = SuccessNode a

processDependencyInformation :: RawDependencyInformation -> DependencyInformation
processDependencyInformation rawResults =
  DependencyInformation
    { depErrorNodes = MS.mapMaybe errorNode resultGraph
    , depModuleDeps = moduleDeps
    , depPkgDeps = pkgDependencies rawResults
    }
  where resultGraph = buildResultGraph rawResults
        successEdges :: [(NormalizedFilePath, NormalizedFilePath, [NormalizedFilePath])]
        successEdges = map (\(k,ks) -> (k,k,ks)) $ MS.toList $
          MS.map (map snd) $ MS.mapMaybe successNode resultGraph
        moduleDeps =
          MS.fromList $ map (\(_, v, vs) -> (v, Set.fromList vs)) successEdges
        errorNode (ErrorNode errs) = Just errs
        errorNode _ = Nothing
        successNode (SuccessNode fs) = Just fs
        successNode _ = Nothing

-- | Given a dependency graph, buildResultGraph detects and propagates errors in that graph as follows:
-- 1. Mark each node that is part of an import cycle as an error node.
-- 2. Mark each node that has a parse error as an error node.
-- 3. Mark each node whose immediate children could not be located as an error.
-- 4. Recursively propagate errors to parents if they are not already error nodes.
buildResultGraph :: RawDependencyInformation -> Map NormalizedFilePath NodeResult
buildResultGraph g = propagatedErrors
    where
        sccs = stronglyConnComp (graphEdges g)
        (_, cycles) = partitionSCC sccs
        cycleErrors :: Map NormalizedFilePath NodeResult
        cycleErrors = MS.unionsWith (<>) $ map errorsForCycle cycles
        errorsForCycle :: [NormalizedFilePath] -> Map NormalizedFilePath NodeResult
        errorsForCycle files =
          MS.fromListWith (<>) (concatMap (cycleErrorsForFile files) files)
        cycleErrorsForFile :: [NormalizedFilePath] -> NormalizedFilePath -> [(NormalizedFilePath,NodeResult)]
        cycleErrorsForFile cycle f =
          let entryPoints = mapMaybe (findImport f) cycle
          in map (\imp -> (f, ErrorNode (PartOfCycle imp cycle :| []))) entryPoints
        otherErrors = MS.map otherErrorsForFile (moduleDependencies g)
        otherErrorsForFile :: Either ModuleParseError [(Located ModuleName, Maybe NormalizedFilePath)] -> NodeResult
        otherErrorsForFile (Left err) = ErrorNode (ParseError err :| [])
        otherErrorsForFile (Right imports) =
          let toEither (imp, Nothing) = Left imp
              toEither (imp, Just path) = Right (imp, path)
              (errs, imports') = partitionEithers (map toEither imports)
          in case nonEmpty errs of
            Nothing -> SuccessNode imports'
            Just errs' -> ErrorNode (NonEmpty.map FailedToLocateImport errs')

        unpropagatedErrors = MS.unionWith (<>) cycleErrors otherErrors
        -- The recursion here is fine since we use a lazy map and
        -- we only recurse on SuccessNodes. In particular, we do not recurse
        -- on nodes that are part of a cycle as they are already marked as
        -- error nodes.
        propagatedErrors =
          ML.map propagate unpropagatedErrors
        propagate :: NodeResult -> NodeResult
        propagate n@(ErrorNode _) = n
        propagate n@(SuccessNode imps) =
          let results = map (\(imp, dep) -> (imp, propagatedErrors MS.! dep)) imps
              (errs, _) = partitionNodeResults results
          in case nonEmpty errs of
               Nothing -> n
               Just errs' -> ErrorNode (NonEmpty.map (ParentOfErrorNode . fst) errs')
        findImport :: NormalizedFilePath -> NormalizedFilePath -> Maybe (Located ModuleName)
        findImport file importedFile =
          case moduleDependencies g MS.! file of
            Left _ -> error "Tried to call findImport on a module with a parse error"
            Right imports ->
              fmap fst $ find (\(_, resolvedImp) -> resolvedImp == Just importedFile) imports

graphEdges :: RawDependencyInformation -> [(NormalizedFilePath, NormalizedFilePath, [NormalizedFilePath])]
graphEdges g =
  map (\(k, ks) -> (k, k, ks)) $ MS.toList $ MS.map deps $ moduleDependencies g
  where deps :: Either e [(i, Maybe NormalizedFilePath)] -> [NormalizedFilePath]
        deps (Left _) = []
        deps (Right imports) = mapMaybe snd imports

partitionSCC :: [SCC a] -> ([a], [[a]])
partitionSCC (CyclicSCC xs:rest) = second (xs:) $ partitionSCC rest
partitionSCC (AcyclicSCC x:rest) = first (x:)   $ partitionSCC rest
partitionSCC []                  = ([], [])

transitiveDeps :: DependencyInformation -> NormalizedFilePath -> Maybe TransitiveDependencies
transitiveDeps DependencyInformation{..} f = do
  reachableVs <- Set.delete f . Set.fromList . map (fst3 . fromVertex) . reachable g <$> toVertex f
  let transitiveModuleDeps = filter (\v -> v `Set.member` reachableVs) $ map (fst3 . fromVertex) vs
  let transitivePkgDeps = Set.toList $ foldMap (\f -> MS.findWithDefault Set.empty f depPkgDeps) (f : transitiveModuleDeps)
  pure TransitiveDependencies {..}
  where (g, fromVertex, toVertex) = graphFromEdges (map (\(f, fs) -> (f, f, Set.toList fs)) $ MS.toList depModuleDeps)
        vs = topSort g

data TransitiveDependencies = TransitiveDependencies
  { transitiveModuleDeps :: [NormalizedFilePath]
  -- ^ Transitive module dependencies in topological order.
  -- The module itself is not included.
  , transitivePkgDeps :: [InstalledUnitId]
  -- ^ Transitive pkg dependencies in unspecified order.
  } deriving (Eq, Show, Generic)

instance NFData TransitiveDependencies
