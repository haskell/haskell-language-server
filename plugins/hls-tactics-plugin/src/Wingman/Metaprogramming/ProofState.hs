{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Wingman.Metaprogramming.ProofState where

import           Data.Bool (bool)
import           Data.Functor ((<&>))
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Util.Panic
import           Language.LSP.Types (sectionSeparator)
import           Wingman.Judgements (jHypothesis)
import           Wingman.Types

renderSimplyDecorated
    :: Monoid out
    => (T.Text -> out) -- ^ Render plain 'Text'
    -> (ann -> out)  -- ^ How to render an annotation
    -> (ann -> out)  -- ^ How to render the removed annotation
    -> SimpleDocStream ann
    -> out
renderSimplyDecorated text push pop = go []
  where
    go _        SFail               = panicUncaughtFail
    go []       SEmpty              = mempty
    go (_:_)    SEmpty              = panicInputNotFullyConsumed
    go st       (SChar c rest)      = text (T.singleton c) <> go st rest
    go st       (SText _l t rest)   = text t <> go st rest
    go st       (SLine i rest)      =
      text (T.singleton '\n') <> text (textSpaces i) <> go st rest
    go st       (SAnnPush ann rest) = push ann <> go (ann : st) rest
    go (ann:st) (SAnnPop rest)      = pop ann <> go st rest
    go []       SAnnPop{}           = panicUnpairedPop
{-# INLINE renderSimplyDecorated #-}


data Ann
  = Goal
  | Hypoth
  | Status
  deriving (Eq, Ord, Show, Enum, Bounded)

forceMarkdownNewlines :: String -> String
forceMarkdownNewlines = unlines . fmap (<> "  ") . lines

layout :: Doc Ann -> String
layout
  = forceMarkdownNewlines
  . T.unpack
  . renderSimplyDecorated id
    renderAnn
    renderUnann
  . layoutPretty (LayoutOptions $ AvailablePerLine 80 0.6)

renderAnn :: Ann -> T.Text
renderAnn Goal = "<span style='color:#ef4026;'>"
renderAnn Hypoth = "```haskell\n"
renderAnn Status = "<span style='color:#6495ED;'>"

renderUnann :: Ann -> T.Text
renderUnann Goal = "</span>"
renderUnann Hypoth = "\n```\n"
renderUnann Status = "</span>"

proofState :: RunTacticResults -> Doc Ann
proofState RunTacticResults{rtr_subgoals} =
  vsep
    $ ( annotate Status
      . countFinished "goals accomplished 🎉" "goal"
      $ length rtr_subgoals
      )
    : pretty sectionSeparator
    : fmap prettySubgoal rtr_subgoals


prettySubgoal :: Judgement -> Doc Ann
prettySubgoal jdg =
  vsep $
    [ mempty | has_hy] <>
    [ annotate Hypoth $ prettyHypothesis hy | has_hy] <>
    [ "⊢" <+> annotate Goal (prettyType (_jGoal jdg))
    , pretty sectionSeparator
    ]
  where
    hy = jHypothesis jdg
    has_hy = not $ null $ unHypothesis hy


prettyHypothesis :: Hypothesis CType -> Doc Ann
prettyHypothesis hy =
  vsep $ unHypothesis hy <&> \hi ->
    prettyHyInfo hi

prettyHyInfo :: HyInfo CType -> Doc Ann
prettyHyInfo hi = viaShow (hi_name hi) <+> "::" <+> prettyType (hi_type hi)


prettyType :: CType -> Doc Ann
prettyType (CType ty) = viaShow ty


countFinished :: Doc Ann -> Doc Ann -> Int -> Doc Ann
countFinished finished _ 0 = finished
countFinished _ thing n    = count thing n

count :: Doc Ann -> Int -> Doc Ann
count thing n =
  pretty n <+> thing <> bool "" "s" (n /= 1)

textSpaces :: Int -> T.Text
textSpaces n = T.replicate n $ T.singleton ' '


