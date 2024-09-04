{-# LANGUAGE TypeFamilies #-}
module Development.IDE.Graph.Internal.RuleInput where

type ValidInputs = [Input]

data Input
    = ProjectHaskellFile
    | DependencyHaskellFile

type family RuleInput k :: ValidInputs

class HasInput (i :: Input) (is :: ValidInputs)

instance HasInput i (i : is)

instance {-# OVERLAPPABLE #-}
    HasInput i is => HasInput i (j : is)
