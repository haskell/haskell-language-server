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
import           Wingman.Metaprogramming.Parser.Documentation
import           Wingman.Metaprogramming.ProofState (proofState, layout)
import           Wingman.Tactics
import           Wingman.Types


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
  [ command "assumption" Nondeterministic Nullary
      "Use any term in the hypothesis that can unify with the current goal."
      (pure assumption)
      [ Example
          Nothing
          []
          [EHI "some_a_val" "a"]
          (Just "a")
          "some_a_val"
      ]

  , command "assume" Deterministic (Ref One)
      "Use the given term from the hypothesis, unifying it with the current goal"
      (pure . assume)
      [ Example
          Nothing
          ["some_a_val"]
          [EHI "some_a_val" "a"]
          (Just "a")
          "some_a_val"
      ]

  , command "intros" Deterministic (Bind Many)
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

  , command "intro" Deterministic (Bind One)
      "Construct a lambda expression, binding an argument with the given name."
      (pure . intros' . Just . pure)
      [ Example
          Nothing
          ["aye"]
          []
          (Just "a -> b -> c -> d")
          "\\aye -> (_ :: b -> c -> d)"
      ]

  , command "destruct_all" Deterministic Nullary
      "Pattern match on every function paramater, in original binding order."
      (pure destructAll)
      [ Example
          (Just "Assume `a` and `b` were bound via `f a b = _`.")
          []
          [EHI "a" "Bool", EHI "b" "Maybe Int"]
          Nothing $
          T.pack $ init $ unlines
            [ "case a of"
            , "  False -> case b of"
            , "    Nothing -> _"
            , "    Just i -> _"
            , "  True -> case b of"
            , "    Nothing -> _"
            , "    Just i -> _"
            ]
      ]

  , command "destruct" Deterministic (Ref One)
      "Pattern match on the argument."
      (pure . useNameFromHypothesis destruct)
      [ Example
          Nothing
          ["a"]
          [EHI "a" "Bool"]
          Nothing $
          T.pack $ init $ unlines
            [ "case a of"
            , "  False -> _"
            , "  True -> _"
            ]
      ]

  , command "homo" Deterministic (Ref One)
      ( mconcat
        [ "Pattern match on the argument, and fill the resulting hole in with "
        , "the same data constructor."
        ])
      (pure . useNameFromHypothesis homo)
      [ Example
          (Just $ mconcat
            [ "Only applicable when the type constructor of the argument is "
            , "the same as that of the hole."
            ])
          ["e"]
          [EHI "e" "Either a b"]
          (Just "Either x y") $
          T.pack $ init $ unlines
            [ "case e of"
            , "  Left a -> Left (_ :: x)"
            , "  Right b -> Right (_ :: y)"
            ]
      ]

  , command "application" Nondeterministic Nullary
      "Apply any function in the hypothesis that returns the correct type."
      (pure application)
      [ Example
          Nothing
          []
          [EHI "f" "a -> b"]
          (Just "b")
          "f (_ :: a)"
      ]

  , command "apply" Deterministic (Ref One)
      "Apply the given function from *local* scope."
      (pure . useNameFromHypothesis apply)
      [ Example
          Nothing
          ["f"]
          [EHI "f" "a -> b"]
          (Just "b")
          "f (_ :: a)"
      ]

  , command "split" Nondeterministic Nullary
      "Produce a data constructor for the current goal."
      (pure split)
      [ Example
          Nothing
          []
          []
          (Just "Either a b")
          "Right (_ :: b)"
      ]

  , command "ctor" Deterministic (Ref One)
      "Use the given data cosntructor."
      (pure . userSplit)
      [ Example
          Nothing
          ["Just"]
          []
          (Just "Maybe a")
          "Just (_ :: a)"
      ]

  , command "obvious" Nondeterministic Nullary
      "Produce a nullary data constructor for the current goal."
      (pure obvious)
      [ Example
          Nothing
          []
          []
          (Just "[a]")
          "[]"
      ]

  , command "auto" Nondeterministic Nullary
      ( mconcat
          [ "Repeatedly attempt to split, destruct, apply functions, and "
          , "recurse in an attempt to fill the hole."
          ])
      (pure auto)
      [ Example
          Nothing
          []
          [EHI "f" "a -> b", EHI "g" "b -> c"]
          (Just "a -> c")
          "g . f"
      ]

  , command "sorry" Deterministic Nullary
      "\"Solve\" the goal by leaving a hole."
      (pure sorry)
      [ Example
          Nothing
          []
          []
          (Just "b")
          "_ :: b"
      ]

  , command "unary" Deterministic Nullary
      ( mconcat
        [ "Produce a hole for a single-parameter function, as well as a hole for "
        , "its argument. The argument holes are completely unconstrained, and "
        , "will be solved before the function."
        ])
      (pure $ nary 1)
      [ Example
          (Just $ mconcat
            [ "In the example below, the variable `a` is free, and will unify "
            , "to the resulting extract from any subsequent tactic."
            ])
          []
          []
          (Just "Int")
          "(_2 :: a -> Int) (_1 :: a)"
      ]

  , command "binary" Deterministic Nullary
      ( mconcat
        [ "Produce a hole for a two-parameter function, as well as holes for "
        , "its arguments. The argument holes have the same type but are "
        , "otherwise unconstrained, and will be solved before the function."
        ])
      (pure $ nary 2)
      [ Example
          (Just $ mconcat
            [ "In the example below, the variable `a` is free, and will unify "
            , "to the resulting extract from any subsequent tactic."
            ])
          []
          []
          (Just "Int")
          "(_3 :: a -> a -> Int) (_1 :: a) (_2 :: a)"
      ]

  , command "recursion" Deterministic Nullary
      "Fill the current hole with a call to the defining function."
      ( pure $
          fmap listToMaybe getCurrentDefinitions >>= \case
            Just (self, _) -> useNameFromContext apply self
            Nothing -> E.throwError $ TacticPanic "no defining function"
      )
      [ Example
          (Just "In the context of `foo (a :: Int) (b :: b) = _`:")
          []
          []
          Nothing
          "foo (_ :: Int) (_ :: b)"
      ]

  , command "use" Deterministic (Ref One)
      "Apply the given function from *module* scope."
      ( \occ -> do
          ctx <- asks ps_context
          ty <- case lookupNameInContext occ ctx of
            Just ty -> pure ty
            Nothing -> CType <$> getOccTy occ
          pure $ apply $ createImportedHyInfo occ ty
      )
      [ Example
          (Just "`import Data.Char (isSpace)`")
          ["isSpace"]
          []
          (Just "Bool")
          "isSpace (_ :: Char)"
      ]

  , command "cata" Deterministic (Ref One)
      "Destruct the given term, recursing on every resulting binding."
      (pure . useNameFromHypothesis cata)
      [ Example
          (Just "Assume we're called in the context of a function `f.`")
          ["x"]
          [EHI "x" "(a, a)"]
          Nothing $
          T.pack $ init $ unlines
            [ "case x of"
            , "  (a1, a2) ->"
            , "    let a1_c = f a1"
            , "        a2_c = f a2"
            , "     in _"
            ]
      ]

  , command "collapse" Deterministic Nullary
      "Collapse every term in scope with the same type as the goal."
      (pure collapse)
      [ Example
          Nothing
          []
          [ EHI "a1" "a"
          , EHI "a2" "a"
          , EHI "a3" "a"
          ]
          (Just "a")
          "(_ :: a -> a -> a -> a) a1 a2 a3"
      ]

  ]



oneTactic :: Parser (TacticsM ())
oneTactic =
  P.choice
    [ parens tactic
    , makeParser commands
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


------------------------------------------------------------------------------
-- | Automatically generate the metaprogram command reference.
writeDocumentation :: IO ()
writeDocumentation =
  writeFile "plugins/hls-tactics-plugin/COMMANDS.md" $
    unlines
      [ "# Wingman Metaprogram Command Reference"
      , ""
      , prettyReadme commands
      ]

