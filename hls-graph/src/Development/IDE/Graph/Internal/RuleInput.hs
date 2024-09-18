{-# LANGUAGE TypeFamilies #-}
module Development.IDE.Graph.Internal.RuleInput where

data ValidInputs
    = ProjectHaskellFilesOnly
    | AllHaskellFiles
    | NoFiles

data Input
    = ProjectHaskellFile
    | DependencyHaskellFile
    | NoFile

type family RuleInput k :: ValidInputs

class HasInput (i :: Input) (is :: ValidInputs)

instance HasInput ProjectHaskellFile ProjectHaskellFilesOnly

instance HasInput ProjectHaskellFile AllHaskellFiles

instance HasInput DependencyHaskellFile AllHaskellFiles

instance HasInput NoFile NoFiles
