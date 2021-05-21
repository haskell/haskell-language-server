{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Wingman.Metaprogramming.Parser where

import qualified Control.Monad.Combinators.Expr as P
import qualified Control.Monad.Error.Class as E
import           Control.Monad.Reader (ReaderT, ask, MonadIO (liftIO), asks)
import           Data.Functor
import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import           GhcPlugins (occNameString)
import qualified Refinery.Tactic as R
import qualified Text.Megaparsec as P
import           Wingman.Auto
import           Wingman.Context (getCurrentDefinitions)
import           Wingman.Machinery (useNameFromHypothesis, getOccNameType, createImportedHyInfo, useNameFromContext, lookupNameInContext)
import           Wingman.Metaprogramming.Lexer
import           Wingman.Metaprogramming.ProofState (proofState, layout)
import           Wingman.Tactics
import           Wingman.Types


nullary :: T.Text -> TacticsM () -> Parser (TacticsM ())
nullary name tac = identifier name $> tac

unary_occ :: T.Text -> (OccName -> TacticsM ()) -> Parser (TacticsM ())
unary_occ name tac = tac <$> (identifier name *> variable)

unary_occM :: T.Text -> (OccName -> Parser (TacticsM ())) -> Parser (TacticsM ())
unary_occM name tac = tac =<< (identifier name *> variable)

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
    , unary_occ "apply_module" $ useNameFromContext apply
    , unary_occ "apply" $ useNameFromHypothesis apply
    , nullary   "split" split
    , unary_occ "ctor" userSplit
    , nullary   "obvious" obvious
    , nullary   "auto" auto
    , nullary   "sorry" sorry
    , nullary   "unary" $ nary 1
    , nullary   "binary" $ nary 2
    , nullary   "recursion" $
        fmap listToMaybe getCurrentDefinitions >>= \case
          Just (self, _) -> useNameFromContext apply self
          Nothing -> E.throwError $ TacticPanic "no defining function"
    , unary_occM "use" $ \occ -> do
        ctx <- asks ps_context
        ty <- case lookupNameInContext occ ctx of
          Just ty -> pure ty
          Nothing -> CType <$> getOccTy occ
        pure $ apply $ createImportedHyInfo occ ty
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


tacticProgram :: Parser (TacticsM ())
tacticProgram = do
  sc
  r <- tactic P.<|> pure (pure ())
  P.eof
  pure r


wrapError :: String -> String
wrapError err = "```\n" <> err <> "\n```\n"


attempt_it
    :: Context
    -> Judgement
    -> String
    -> ReaderT ParserContext IO (Either String String)
attempt_it ctx jdg program =
  P.runParserT tacticProgram "<splice>" (T.pack program) <&> \case
      Left peb -> Left $ wrapError $ P.errorBundlePretty peb
      Right tt -> do
        case runTactic
              ctx
              jdg
              tt
          of
            Left tes -> Left $ wrapError $ show tes
            Right rtr -> Right $ layout $ proofState rtr


parseMetaprogram :: T.Text -> ReaderT ParserContext IO (TacticsM ())
parseMetaprogram
    = fmap (either (const $ pure ()) id)
    . P.runParserT tacticProgram "<splice>"


getOccTy :: OccName -> Parser Type
getOccTy occ = do
  ParserContext hscenv rdrenv modul _ <- ask
  mty <- liftIO $ getOccNameType hscenv rdrenv modul occ
  maybe (fail $ occNameString occ <> " is not in scope") pure mty

