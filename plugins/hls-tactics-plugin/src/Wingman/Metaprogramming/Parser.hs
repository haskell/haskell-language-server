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
import           Development.IDE.GHC.Compat (alphaTyVars)
import           GhcPlugins (mkTyVarTy, mkFunTys, mkListTy, mkVarOcc)
import qualified Refinery.Tactic as R
import qualified Text.Megaparsec as P
import           Wingman.Auto
import           Wingman.Judgements (mkFirstJudgement)
import           Wingman.LanguageServer.TacticProviders (useNameFromHypothesis)
import           Wingman.Metaprogramming.Lexer
import           Wingman.Tactics
import           Wingman.Types


tactic :: Parser (TacticsM ())
tactic = flip P.makeExprParser operators $  P.choice
    [ named  "assumption" assumption
    , named' "assume" assume
    , named  "intros" intros
    -- , named' "intro" intro
    , named' "destruct" $ useNameFromHypothesis destruct
    , named' "homo" $ useNameFromHypothesis homo
    , named' "apply" $ useNameFromHypothesis apply
    -- , named  "split" $ useNameFromHypothesis split
    , named  "auto" auto
    , R.try <$> (keyword "try" *> tactics)
    ]

multitactic :: Parser (TacticsM () -> TacticsM ())
multitactic = P.choice
    [ (flip (R.<@>)) <$> brackets (P.sepBy1 tactic (symbol ";"))
    , (flip (>>)) <$> tactic
    ]

operators :: [[P.Operator Parser (TacticsM ())]]
operators =
    [ [ P.Prefix (symbol "*" $> R.many_) ]
    , [ P.InfixR (symbol "|" $> (R.<%>) )]
    ]

tactics :: Parser (TacticsM ())
tactics = do
    t <- tactic
    ts <- P.many ((symbol ",") *> multitactic)
    pure $ foldl' (&) t ts


skolems :: [Type]
skolems = fmap mkTyVarTy alphaTyVars

a_skolem, b_skolem, c_skolem :: Type
(a_skolem : b_skolem : c_skolem : _) = skolems


main :: IO ()
main = do
  case P.runParser (tactics <* P.eof)
          "<splice>"
          "intros, destruct as, [ assumption; auto ]" of
    Left peb -> putStrLn $ P.errorBundlePretty peb
    Right tt -> do
      let ty = mkFunTys
                [ mkFunTys [a_skolem, b_skolem] b_skolem
                , b_skolem
                , mkListTy a_skolem
                ] b_skolem
      case runTactic
             emptyContext { ctxDefiningFuncs = [(mkVarOcc "hello", CType ty)] }
             (mkFirstJudgement mempty True ty)
             tt
        of
          Left tes -> putStrLn $ "failed: " <> show tes
          Right rtr -> print $ rtr_extract rtr

