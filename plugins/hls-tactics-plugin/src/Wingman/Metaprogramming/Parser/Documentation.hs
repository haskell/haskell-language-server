{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Wingman.Metaprogramming.Parser.Documentation where

import           Data.Functor ((<&>))
import           Data.List (sortOn)
import           Data.String (IsString)
import           Data.Text (Text)
import           Data.Text.Prettyprint.Doc hiding (parens)
import           Data.Text.Prettyprint.Doc.Render.String (renderString)
import           Development.IDE.GHC.Compat (OccName)
import qualified Text.Megaparsec as P
import           Wingman.Metaprogramming.Lexer (Parser, identifier, variable, parens)
import           Wingman.Types (TacticsM)

import {-# SOURCE #-} Wingman.Metaprogramming.Parser (tactic)


------------------------------------------------------------------------------
-- | Is a tactic deterministic or not?
data Determinism
  = Deterministic
  | Nondeterministic

prettyDeterminism :: Determinism -> Doc b
prettyDeterminism Deterministic = "deterministic"
prettyDeterminism Nondeterministic = "non-deterministic"


------------------------------------------------------------------------------
-- | How many arguments does the tactic take?
data Count a where
  One  :: Count OccName
  Many :: Count [OccName]

prettyCount :: Count a -> Doc b
prettyCount One  = "single"
prettyCount Many = "variadic"


------------------------------------------------------------------------------
-- | What sorts of arguments does the tactic take? Currently there is no
-- distinction between 'Ref' and 'Bind', other than documentation.
--
-- The type index here is used for the shape of the function the parser should
-- take.
data Syntax a where
  Nullary :: Syntax (Parser (TacticsM ()))
  Ref     :: Count a -> Syntax (a -> Parser (TacticsM ()))
  Bind    :: Count a -> Syntax (a -> Parser (TacticsM ()))
  Tactic  :: Syntax (TacticsM () -> Parser (TacticsM ()))

prettySyntax :: Syntax a -> Doc b
prettySyntax Nullary   = "none"
prettySyntax (Ref co)  = prettyCount co <+> "reference"
prettySyntax (Bind co) = prettyCount co <+> "binding"
prettySyntax Tactic    = "tactic"


------------------------------------------------------------------------------
-- | An example for the documentation.
data Example = Example
  { ex_ctx    :: Maybe Text         -- ^ Specific context information about when the tactic is applicable
  , ex_args   :: [Var]              -- ^ Arguments the tactic was called with
  , ex_hyp    :: [ExampleHyInfo]    -- ^ The hypothesis
  , ex_goal   :: Maybe ExampleType  -- ^ Current goal. Nothing indicates it's uninteresting.
  , ex_result :: Text               -- ^ Resulting extract.
  }


------------------------------------------------------------------------------
-- | An example 'HyInfo'.
data ExampleHyInfo = EHI
  { ehi_name :: Var          -- ^ Name of the variable
  , ehi_type :: ExampleType  -- ^ Type of the variable
  }


------------------------------------------------------------------------------
-- | A variable
newtype Var = Var
  { getVar :: Text
  }
  deriving newtype (IsString, Pretty)


------------------------------------------------------------------------------
-- | A type
newtype ExampleType = ExampleType
  { getExampleType :: Text
  }
  deriving newtype (IsString, Pretty)


------------------------------------------------------------------------------
-- | A command to expose to the parser
data MetaprogramCommand a = MC
  { mpc_name        :: Text         -- ^ Name of the command. This is the token necessary to run the command.
  , mpc_syntax      :: Syntax a     -- ^ The command's arguments
  , mpc_det         :: Determinism  -- ^ Determinism of the command
  , mpc_description :: Text         -- ^ User-facing description
  , mpc_tactic      :: a            -- ^ Tactic to run
  , mpc_examples    :: [Example]    -- ^ Collection of documentation examples
  }

------------------------------------------------------------------------------
-- | Existentialize the pain away
data SomeMetaprogramCommand where
  SMC :: MetaprogramCommand a -> SomeMetaprogramCommand


------------------------------------------------------------------------------
-- | Run the 'Parser' of a 'MetaprogramCommand'
makeMPParser :: MetaprogramCommand a -> Parser (TacticsM ())
makeMPParser (MC name Nullary _ _ t _) = do
  identifier name
  t
makeMPParser (MC name (Ref One) _ _ t _) = do
  identifier name
  variable >>= t
makeMPParser (MC name (Ref Many) _ _ t _) = do
  identifier name
  P.many variable >>= t
makeMPParser (MC name (Bind One) _ _ t _) = do
  identifier name
  variable >>= t
makeMPParser (MC name (Bind Many) _ _ t _) = do
  identifier name
  P.many variable >>= t
makeMPParser (MC name Tactic _ _ t _) = do
  identifier name
  parens tactic >>= t


------------------------------------------------------------------------------
-- | Compile a collection of metaprogram commands into a parser.
makeParser :: [SomeMetaprogramCommand] -> Parser (TacticsM ())
makeParser ps = P.choice $ ps <&> \(SMC mp) -> makeMPParser mp


------------------------------------------------------------------------------
-- | Pretty print a command.
prettyCommand :: MetaprogramCommand a -> Doc b
prettyCommand (MC name syn det desc _ exs) = vsep
  [ "##" <+> pretty name
  , mempty
  , "arguments:" <+> prettySyntax syn <> ".  "
  , prettyDeterminism det <> "."
  , mempty
  , ">" <+> align (pretty desc)
  , mempty
  , vsep $ fmap (prettyExample name) exs
  , mempty
  ]


------------------------------------------------------------------------------
-- | Pretty print a hypothesis.
prettyHyInfo :: ExampleHyInfo -> Doc a
prettyHyInfo hi = pretty (ehi_name hi) <+> "::" <+> pretty (ehi_type hi)


------------------------------------------------------------------------------
-- | Append the given term only if the first argument has elements.
mappendIfNotNull :: [a] -> a -> [a]
mappendIfNotNull [] _ = []
mappendIfNotNull as a = as <> [a]


------------------------------------------------------------------------------
-- | Pretty print an example.
prettyExample :: Text -> Example -> Doc a
prettyExample name (Example m_txt args hys goal res) =
  align $ vsep
    [ mempty
    , "### Example"
    , maybe mempty ((line <>) . (<> line) . (">" <+>) . align . pretty) m_txt
    , "Given:"
    , mempty
    , codeFence $ vsep
        $ mappendIfNotNull (fmap prettyHyInfo hys) mempty
           <> [ "_" <+> maybe mempty (("::" <+>). pretty) goal
              ]
    , mempty
    , hsep
        [ "running "
        , enclose "`" "`" $ pretty name <> hsep (mempty : fmap pretty args)
        , "will produce:"
        ]
    , mempty
    , codeFence $ align $ pretty res
    ]


------------------------------------------------------------------------------
-- | Make a haskell code fence.
codeFence :: Doc a -> Doc a
codeFence d = align $ vsep
  [ "```haskell"
  , d
  , "```"
  ]


------------------------------------------------------------------------------
-- | Render all of the commands.
prettyReadme :: [SomeMetaprogramCommand] -> String
prettyReadme
  = renderString
  . layoutPretty defaultLayoutOptions
  . vsep
  . fmap (\case SMC c -> prettyCommand c)
  . sortOn (\case SMC c -> mpc_name c)



------------------------------------------------------------------------------
-- | Helper function to build a 'SomeMetaprogramCommand'.
command
    :: Text
    -> Determinism
    -> Syntax a
    -> Text
    -> a
    -> [Example]
    -> SomeMetaprogramCommand
command txt det syn txt' a exs = SMC $
  MC
    { mpc_name        = txt
    , mpc_det         = det
    , mpc_syntax      = syn
    , mpc_description = txt'
    , mpc_tactic      = a
    , mpc_examples    = exs
    }

