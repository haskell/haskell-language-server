{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Wingman.Metaprogramming.Parser where

import qualified Control.Monad.Combinators.Expr as P
import           Data.Either (fromRight)
import           Data.Functor
import           Data.Maybe (listToMaybe)
import qualified Data.Text as T
import           Development.IDE.GHC.Compat (RealSrcLoc, srcLocLine, srcLocCol, srcLocFile)
import           Development.IDE.GHC.Compat.Util (unpackFS)
import           Refinery.Tactic (failure)
import qualified Refinery.Tactic as R
import qualified Text.Megaparsec as P
import           Wingman.Auto
import           Wingman.Machinery (useNameFromHypothesis, useNameFromContext, getCurrentDefinitions)
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
        names -> intros' $ IntroduceOnlyNamed names
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

  , command "idiom" Deterministic Tactic
      "Lift a tactic into idiom brackets."
      (pure . idiom)
      [ Example
          Nothing
          ["(apply f)"]
          [EHI "f" "a -> b -> Int"]
          (Just "Maybe Int")
          "f <$> (_ :: Maybe a) <*> (_ :: Maybe b)"
      ]

  , command "intro" Deterministic (Bind One)
      "Construct a lambda expression, binding an argument with the given name."
      (pure . intros' . IntroduceOnlyNamed . pure)
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

  , command "pointwise" Deterministic Tactic
      "Restrict the hypothesis in the holes of the given tactic to align up with the top-level bindings. This will ensure, eg, that the first hole can see only terms that came from the first position in any terms destructed from the top-level bindings."
      (pure . flip restrictPositionForApplication (pure ()))
      [ Example
          (Just "In the context of `f (a1, b1) (a2, b2) = _`. The resulting first hole can see only 'a1' and 'a2', and the second, only 'b1' and 'b2'.")
          ["(use mappend)"]
          []
          Nothing
          "mappend _ _"
      ]

  , command "apply" Deterministic (Ref One)
      "Apply the given function from *local* scope."
      (pure . useNameFromHypothesis (apply Saturated))
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
            Just (self, _) -> useNameFromContext (apply Saturated) self
            Nothing -> failure $ TacticPanic "no defining function"
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
      (pure . use Saturated)
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

  , command "let" Deterministic (Bind Many)
      "Create let-bindings for each binder given to this tactic."
      (pure . letBind)
      [ Example
          Nothing
          ["a", "b", "c"]
          [ ]
          (Just "x")
          $ T.pack $ unlines
            [ "let a = _1 :: a"
            , "    b = _2 :: b"
            , "    c = _3 :: c"
            , " in (_4 :: x)"
            ]
      ]

  , command "try" Nondeterministic Tactic
      "Simultaneously run and do not run a tactic. Subsequent tactics will bind on both states."
      (pure . R.try)
      [ Example
          Nothing
          ["(apply f)"]
          [ EHI "f" "a -> b"
          ]
          (Just "b")
          $ T.pack $ unlines
            [ "-- BOTH of:\n"
            , "f (_ :: a)"
            , "\n-- and\n"
            , "_ :: b"
            ]
      ]

  , command "nested" Nondeterministic (Ref One)
      "Nest the given function (in module scope) with itself arbitrarily many times. NOTE: The resulting function is necessarily unsaturated, so you will likely need `with_arg` to use this tactic in a saturated context."
      (pure . nested)
      [ Example
          Nothing
          ["fmap"]
          []
          (Just "[(Int, Either Bool a)] -> [(Int, Either Bool b)]")
          "fmap (fmap (fmap _))"
      ]

  , command "with_arg" Deterministic Nullary
      "Fill the current goal with a function application. This can be useful when you'd like to fill in the argument before the function, or when you'd like to use a non-saturated function in a saturated context."
      (pure with_arg)
      [ Example
          (Just "Where `a` is a new unifiable type variable.")
          []
          []
          (Just "r")
          "(_2 :: a -> r) (_1 :: a)"
      ]
  ]



oneTactic :: Parser (TacticsM ())
oneTactic =
  P.choice
    [ parens tactic
    , makeParser commands
    ]


tactic :: Parser (TacticsM ())
tactic = P.makeExprParser oneTactic operators

operators :: [[P.Operator Parser (TacticsM ())]]
operators =
    [ [ P.InfixR (symbol "|"   $> (R.<%>) )]
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


fixErrorOffset :: RealSrcLoc -> P.ParseErrorBundle a b -> P.ParseErrorBundle a b
fixErrorOffset rsl (P.ParseErrorBundle ne (P.PosState a n (P.SourcePos _ line col) pos s))
  = P.ParseErrorBundle ne
  $ P.PosState a n
      (P.SourcePos
        (unpackFS $ srcLocFile rsl)
        ((<>) line $ P.mkPos $ srcLocLine rsl - 1)
        ((<>) col  $ P.mkPos $ srcLocCol  rsl - 1 + length @[] "[wingman|")
      )
      pos
      s

------------------------------------------------------------------------------
-- | Attempt to run a metaprogram tactic, returning the proof state, or the
-- errors.
attempt_it
    :: RealSrcLoc
    -> Context
    -> Judgement
    -> String
    -> IO (Either String String)
attempt_it rsl ctx jdg program =
  case P.runParser tacticProgram "<splice>" (T.pack program) of
    Left peb -> pure $ Left $ wrapError $ P.errorBundlePretty $ fixErrorOffset rsl peb
    Right tt -> do
      res <- runTactic 2e6 ctx jdg tt
      pure $ case res of
          Left tes -> Left $ wrapError $ show tes
          Right rtr -> Right
                     $ layout (cfg_proofstate_styling $ ctxConfig ctx)
                     $ proofState rtr


parseMetaprogram :: T.Text -> TacticsM ()
parseMetaprogram
    = fromRight (pure ())
    . P.runParser tacticProgram "<splice>"


------------------------------------------------------------------------------
-- | Automatically generate the metaprogram command reference.
writeDocumentation :: IO ()
writeDocumentation =
  writeFile "COMMANDS.md" $
    unlines
      [ "# Wingman Metaprogram Command Reference"
      , ""
      , prettyReadme commands
      ]

