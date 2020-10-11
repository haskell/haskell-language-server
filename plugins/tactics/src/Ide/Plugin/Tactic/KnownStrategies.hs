{-# LANGUAGE LambdaCase #-}

module Ide.Plugin.Tactic.KnownStrategies where

import Control.Monad.Error.Class
import Ide.Plugin.Tactic.Context (getCurrentDefinitions)
import Ide.Plugin.Tactic.Tactics
import Ide.Plugin.Tactic.Types
import OccName (mkVarOcc)
import Refinery.Tactic
import Ide.Plugin.Tactic.Machinery (tracing)


knownStrategies :: TacticsM ()
knownStrategies = choice
  [ deriveFmap
  ]


known :: String -> TacticsM () -> TacticsM ()
known name t = do
  getCurrentDefinitions >>= \case
    [(def, _)] | def == mkVarOcc name ->
      tracing ("known " <> name) t
    _ -> throwError NoApplicableTactic


deriveFmap :: TacticsM ()
deriveFmap = known "fmap" $ do
  try intros
  overAlgebraicTerms homo
  choice
    [ overFunctions apply >> auto' 2
    , assumption
    , recursion
    ]

