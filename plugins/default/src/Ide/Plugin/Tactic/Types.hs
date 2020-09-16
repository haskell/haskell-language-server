{-# LANGUAGE OverloadedStrings #-}
module Ide.Plugin.Tactic.Types
  ( tacticTitle
  , TacticCommand (..)
  ) where

import qualified Data.Text as T

------------------------------------------------------------------------------
-- | The list of tactics exposed to the outside world. These are attached to
-- actual tactics via 'commandTactic' and are contextually provided to the
-- editor via 'commandProvider'.
data TacticCommand
  = Auto
  | Split
  | Intro
  | Intros
  | Destruct
  | Homomorphism
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Generate a title for the command.
tacticTitle :: TacticCommand -> T.Text -> T.Text
tacticTitle Auto _ = "Auto"
tacticTitle Split _ = "Auto"
tacticTitle Intro _ = "Intro"
tacticTitle Intros _ = "Introduce lambda"
tacticTitle Destruct var = "Case split on " <> var
tacticTitle Homomorphism var = "Homomorphic case split on " <> var
