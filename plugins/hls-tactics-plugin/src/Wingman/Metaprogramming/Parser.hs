{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Wingman.Metaprogramming.Parser where

import qualified Control.Monad.Combinators.Expr as P
import           Data.Function
import           Data.Functor
import           Data.List (foldl')
import           Development.IDE.GHC.Compat (alphaTyVars, LHsExpr, GhcPs)
import           GhcPlugins (mkTyVarTy, mkFunTys, mkListTy, mkVarOcc)
import qualified Refinery.Tactic as R
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C
import           Wingman.Auto
import           Wingman.Judgements (mkFirstJudgement)
import           Wingman.LanguageServer.TacticProviders (useNameFromHypothesis)
import           Wingman.Metaprogramming.Lexer
import           Wingman.Tactics
import           Wingman.Types
import qualified Data.Text as T


tactic :: Parser (TacticsM ())
tactic = flip P.makeExprParser operators $  P.choice
    [ braces tactic
    , named  "assumption" assumption
    , named' "assume" assume
    , named  "intros" intros
    -- , named' "intro" intro
    , named "destruct_all" destructAll
    , named' "destruct" $ useNameFromHypothesis destruct
    , named' "homo" $ useNameFromHypothesis homo
    , named' "apply" $ useNameFromHypothesis apply
    , named'  "split" userSplit
    , named  "auto" auto
    , R.try <$> (keyword "try" *> tactic)
    ]

bindOne :: TacticsM a -> TacticsM a -> TacticsM a
bindOne t t1 = t R.<@> [t1]

operators :: [[P.Operator Parser (TacticsM ())]]
operators =
    [ [ P.Prefix (symbol "*" $> R.many_) ]
    , [ P.InfixR (symbol "|" $> (R.<%>) )]
    , [ P.InfixL (symbol ";" $> (>>))
      , P.InfixL (symbol "," $> bindOne)
      ]
    ]


skolems :: [Type]
skolems = fmap mkTyVarTy alphaTyVars

a_skolem, b_skolem, c_skolem :: Type
(a_skolem : b_skolem : c_skolem : _) = skolems


attempt_it :: Context -> Judgement -> String -> Either String (LHsExpr GhcPs)
attempt_it ctx jdg program =
  case P.runParser (sc *> tactic <* P.eof) "<splice>" (T.pack program) of
    Left peb -> Left $ P.errorBundlePretty peb
    Right tt -> do
      case runTactic
             ctx
             jdg
             tt
        of
          Left tes -> Left $ show tes
          Right rtr -> Right $ rtr_extract rtr

