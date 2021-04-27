{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Wingman.Metaprogramming.Parser where

import qualified Control.Monad.Combinators.Expr as P
import           Data.Functor
import qualified Data.Text as T
import           Development.IDE.GHC.Compat (LHsExpr, GhcPs)
import qualified Refinery.Tactic as R
import qualified Text.Megaparsec as P
import           Wingman.Auto
import           Wingman.LanguageServer.TacticProviders (useNameFromHypothesis)
import           Wingman.Metaprogramming.Lexer
import           Wingman.Tactics
import           Wingman.Types


nullary :: T.Text -> TacticsM () -> Parser (TacticsM ())
nullary name tac = identifier name $> tac

unary_occ :: T.Text -> (OccName -> TacticsM ()) -> Parser (TacticsM ())
unary_occ name tac = tac <$> (identifier name *> variable)

variadic_occ :: T.Text -> ([OccName] -> TacticsM ()) -> Parser (TacticsM ())
variadic_occ name tac = tac <$> (identifier name *> P.many variable)

oneTactic :: Parser (TacticsM ())
oneTactic =
  P.choice
    [ braces tactic
      -- TODO(sandy): lean uses braces for control flow, but i always forget
      -- and want to use parens. is there a semantic difference?
    , parens tactic
    , nullary   "assumption" assumption
    , unary_occ "assume" assume
    , variadic_occ   "intros" $ \case
        []    -> intros
        names -> intros' $ Just names
    , unary_occ  "intro" $ intros' . Just . pure
    , nullary   "destruct_all" destructAll
    , unary_occ "destruct" $ useNameFromHypothesis destruct
    , unary_occ "homo" $ useNameFromHypothesis homo
    , nullary   "application" application
    , unary_occ "apply" $ useNameFromHypothesis apply
    , nullary   "split" split
    , unary_occ "ctor" userSplit
    , nullary   "obvious" obvious
    , nullary   "auto" auto
    ]


tactic :: Parser (TacticsM ())
tactic = flip P.makeExprParser operators oneTactic

bindOne :: TacticsM a -> TacticsM a -> TacticsM a
bindOne t t1 = t R.<@> [t1]

operators :: [[P.Operator Parser (TacticsM ())]]
operators =
    [ [ P.Prefix (symbol "*"   $> R.many_) ]
    , [ P.Prefix (symbol "try" $> R.try) ]
    , [ P.InfixR (symbol "|"   $> (R.<%>) )]
    , [ P.InfixL (symbol ";"   $> (>>))
      , P.InfixL (symbol ","   $> bindOne)
      ]
    ]


attempt_it :: Context -> Judgement -> String -> Either String (LHsExpr GhcPs)
attempt_it ctx jdg program =
  case P.runParser (sc *> tactic <* P.eof) "<splice>" $ T.pack program of
    Left peb -> Left $ P.errorBundlePretty peb
    Right tt -> do
      case runTactic
             ctx
             jdg
             tt
        of
          Left tes -> Left $ show tes
          Right rtr -> Right $ rtr_extract rtr

