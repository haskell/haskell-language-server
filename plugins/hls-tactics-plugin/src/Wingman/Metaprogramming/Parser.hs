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
import Wingman.Metaprogramming.Parser.Documentation


nullary :: T.Text -> TacticsM () -> Parser (TacticsM ())
nullary name tac = identifier name $> tac


unary_occ :: T.Text -> (OccName -> TacticsM ()) -> Parser (TacticsM ())
unary_occ name tac = tac <$> (identifier name *> variable)


------------------------------------------------------------------------------
-- | Like 'unary_occ', but runs directly in the 'Parser' monad.
unary_occM :: T.Text -> (OccName -> Parser (TacticsM ())) -> Parser (TacticsM ())
unary_occM name tac = tac =<< (identifier name *> variable)


variadic_occ :: T.Text -> ([OccName] -> TacticsM ()) -> Parser (TacticsM ())
variadic_occ name tac = tac <$> (identifier name *> P.many variable)


commands :: [SomeMetaprogramCommand]
commands =
  [ command
      "assumption"
      Nullary
      "Use any term in the hypothesis that can unify with the current goal."
      (pure assumption)
      [ Example
          Nothing
          []
          [EHI "some_a_val" "a"]
          (Just "a")
          "some_a_val"
      ]

  , command
      "assume"
      (Ref One)
      "Use the given term from the hypothesis, unifying it with the current goal"
      (pure . assume)
      [ Example
          Nothing
          ["some_a_val"]
          [EHI "some_a_val" "a"]
          (Just "a")
          "some_a_val"
      ]

  , command
      "intros"
      (Bind Many)
      ( mconcat
          [ "Construct a lambda expression, using the specific names if given, "
          , "generating unique names otherwise. When no arguments are given, "
          , "all of the function arguments will be bound; otherwise, this "
          , "tactic will bind only enough to saturate the given names. Extra "
          , "names are ignored."
          ])
      (pure . \case
        []    -> intros
        names -> intros' $ Just names
      )
      [ Example
          Nothing
          []
          []
          (Just "a -> b -> c -> d")
          "\\a b c -> (_ :: d)"
      , Example
          Nothing
          ["aye"]
          []
          (Just "a -> b -> c -> d")
          "\\aye -> (_ :: b -> c -> d)"
      , Example
          Nothing
          ["x", "y", "z", "w"]
          []
          (Just "a -> b -> c -> d")
          "\\x y z -> (_ :: d)"
      ]

  , command
      "intro"
      (Bind One)
      "Construct a lambda expression, binding an argument with the given name."
      (pure . intros' . Just . pure)
      [ Example
          Nothing
          ["aye"]
          []
          (Just "a -> b -> c -> d")
          "\\aye -> (_ :: b -> c -> d)"
      ]

  , command
      "destruct_all"
      Nullary
      "Pattern match on every function paramater, in original binding order."
      (pure destructAll)
      [ Example
          (Just "If `a` and `b` were bound via `f a b = _`, then:")
          []
          [EHI "a" "Bool", EHI "b" "Maybe Int"]
          Nothing $
          T.pack $ unlines
            [ "case a of"
            , "  False -> case b of"
            , "    Nothing -> _"
            , "    Just i -> _"
            , "  True -> case b of"
            , "    Nothing -> _"
            , "    Just i -> _"
            ]
      ]

  , command
      "destruct"
      (Ref One)
      "Pattern match on the argument."
      (pure . useNameFromHypothesis destruct)
      [ Example
          Nothing
          ["a"]
          [EHI "a" "Bool"]
          Nothing $
          T.pack $ unlines
            [ "case a of"
            , "  False -> _"
            , "  True -> _"
            ]
      ]

  ]



oneTactic :: Parser (TacticsM ())
oneTactic =
  P.choice
    [ braces tactic
      -- TODO(sandy): lean uses braces for control flow, but i always forget
      -- and want to use parens. is there a semantic difference?
    , parens tactic
    -- , nullary   "assumption" assumption
    -- , unary_occ "assume" assume
    -- , variadic_occ   "intros" $ \case
    --     []    -> intros
    --     names -> intros' $ Just names
    -- , unary_occ  "intro" $ intros' . Just . pure
    -- , nullary   "destruct_all" destructAll
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


------------------------------------------------------------------------------
-- | Attempt to run a metaprogram tactic, returning the proof state, or the
-- errors.
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


------------------------------------------------------------------------------
-- | Like 'getOccNameType', but runs in the 'Parser' monad.
getOccTy :: OccName -> Parser Type
getOccTy occ = do
  ParserContext hscenv rdrenv modul _ <- ask
  mty <- liftIO $ getOccNameType hscenv rdrenv modul occ
  maybe (fail $ occNameString occ <> " is not in scope") pure mty

