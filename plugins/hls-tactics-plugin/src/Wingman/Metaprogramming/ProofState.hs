{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Wingman.Metaprogramming.ProofState where

import Data.Text.Prettyprint.Doc
import Wingman.Types
import Data.Bool (bool)
import Wingman.Judgements (jLocalHypothesis, isDisallowed)
import Data.Functor ((<&>))
import Data.Text.Prettyprint.Doc.Render.String (renderString)
import qualified Data.Text as T
import Language.LSP.Types (sectionSeparator)

forceMarkdownNewlines :: String -> String
forceMarkdownNewlines = unlines . fmap (<> "  ") . lines

layout :: Doc ann -> String
layout
  = flip mappend (T.unpack sectionSeparator)
  . forceMarkdownNewlines
  . renderString
  . layoutPretty (LayoutOptions $ AvailablePerLine 80 0.6)

proofState :: RunTacticResults -> Doc ann
proofState RunTacticResults{rtr_extract, rtr_subgoals} =
  vsep
    $ (countFinished (viaShow rtr_extract) "goal" $ length rtr_subgoals)
    : fmap prettySubgoal rtr_subgoals


prettySubgoal :: Judgement -> Doc ann
prettySubgoal jdg =
  vsep
    [ mempty
    , prettyHypothesis $ jLocalHypothesis jdg
    , "⊢" <+> prettyType (_jGoal jdg)
    ]


prettyHypothesis :: Hypothesis CType -> Doc ann
prettyHypothesis hy =
  vsep $ filter (not . isDisallowed . hi_provenance) (unHypothesis hy) <&> \hi ->
    prettyHyInfo hi

prettyHyInfo :: HyInfo CType -> Doc ann
prettyHyInfo hi = viaShow (hi_name hi) <+> "::" <+> prettyType (hi_type hi)


prettyType :: CType -> Doc ann
prettyType (CType ty) = viaShow ty


countFinished :: Doc ann -> Doc ann -> Int -> Doc ann
countFinished finished _ 0 = finished
countFinished _ thing n    = count thing n

count :: Doc ann -> Int -> Doc ann
count thing n =
  pretty n <+> thing <> bool "" "s" (n /= 1)

