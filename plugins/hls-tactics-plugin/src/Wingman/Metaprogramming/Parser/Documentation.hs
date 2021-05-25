{-# LANGUAGE OverloadedStrings #-}

module Wingman.Metaprogramming.Parser.Documentation where

import           Data.Functor ((<&>))
import           Data.List (sortOn)
import           Data.String (IsString)
import           Data.Text (Text)
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.String (renderString)
import           GhcPlugins (OccName)
import qualified Text.Megaparsec as P
import           Wingman.Metaprogramming.Lexer (Parser, identifier, variable)
import           Wingman.Types (TacticsM)


data Determinism
  = Deterministic
  | Nondeterministic

prettyDeterminism :: Determinism -> Doc b
prettyDeterminism Deterministic = "deterministic"
prettyDeterminism Nondeterministic = "non-deterministic"

data Count a where
  One  :: Count OccName
  Many :: Count [OccName]

prettyCount :: Count a -> Doc b
prettyCount One  = "single"
prettyCount Many = "varadic"


data Syntax a where
  Nullary :: Syntax (Parser (TacticsM ()))
  Ref     :: Count a -> Syntax (a -> Parser (TacticsM ()))
  Bind    :: Count a -> Syntax (a -> Parser (TacticsM ()))

prettySyntax :: Syntax a -> Doc b
prettySyntax Nullary   = "none"
prettySyntax (Ref co)  = prettyCount co <+> "reference"
prettySyntax (Bind co) = prettyCount co <+> "binding"


data Example = Example
  { ex_ctx    :: Maybe Text
  , ex_args   :: [Var]
  , ex_hyp    :: [ExampleHyInfo]
  , ex_goal   :: Maybe ExampleType
  , ex_result :: Text
  }


data ExampleHyInfo = EHI
  { ehi_name :: Var
  , ehi_type :: ExampleType
  }

newtype Var = Var
  { getVar :: Text
  }
  deriving newtype (IsString, Pretty)

newtype ExampleType = ExampleType
  { getExampleType :: Text
  }
  deriving newtype (IsString, Pretty)


data MetaprogramCommand a = MC
  { mpc_name        :: Text
  , mpc_syntax      :: Syntax a
  , mpc_det         :: Determinism
  , mpc_description :: Text
  , mpc_tactic      :: a
  , mpc_examples    :: [Example]
  }

data SomeMetaprogramCommand where
  SMC :: MetaprogramCommand a -> SomeMetaprogramCommand


makeMPParser :: MetaprogramCommand a -> Parser (TacticsM ())
makeMPParser (MC name Nullary _ _ tactic _) = do
  identifier name
  tactic
makeMPParser (MC name (Ref One) _ _ tactic _) = do
  identifier name
  variable >>= tactic
makeMPParser (MC name (Ref Many) _ _ tactic _) = do
  identifier name
  P.many variable >>= tactic
makeMPParser (MC name (Bind One) _ _ tactic _) = do
  identifier name
  variable >>= tactic
makeMPParser (MC name (Bind Many) _ _ tactic _) = do
  identifier name
  P.many variable >>= tactic


makeParser :: [SomeMetaprogramCommand] -> Parser (TacticsM ())
makeParser ps = P.choice $ ps <&> \(SMC mp) -> makeMPParser mp


prettyCommand :: MetaprogramCommand a -> Doc b
prettyCommand (MC name syn det desc _ exs) = vsep
  [ "##" <+> pretty name
  , mempty
  , "arguments:" <+> prettySyntax syn <> "."
  , prettyDeterminism det <> "."
  , mempty
  , ">" <+> align (pretty desc)
  , mempty
  , vsep $ fmap (prettyExample name) exs
  ]


prettyHyInfo :: ExampleHyInfo -> Doc a
prettyHyInfo hi = pretty (ehi_name hi) <+> "::" <+> pretty (ehi_type hi)

mappendIfNotNull :: [a] -> a -> [a]
mappendIfNotNull [] _ = []
mappendIfNotNull as a = as <> [a]


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


codeFence :: Doc a -> Doc a
codeFence d = align $ vsep
  [ "```haskell"
  , d
  , "```"
  ]


prettyReadme :: [SomeMetaprogramCommand] -> String
prettyReadme
  = renderString
  . layoutPretty defaultLayoutOptions
  . vsep
  . fmap (\case SMC c -> prettyCommand c)
  . sortOn (\case SMC c -> mpc_name c)


dump :: SomeMetaprogramCommand -> String
dump (SMC mc)
  = renderString
  . layoutPretty defaultLayoutOptions
  $ prettyCommand mc


command :: Text -> Determinism -> Syntax a -> Text -> a -> [Example] -> SomeMetaprogramCommand
command txt det syn txt' a exs = SMC $
  MC
    { mpc_name        = txt
    , mpc_det         = det
    , mpc_syntax      = syn
    , mpc_description = txt'
    , mpc_tactic      = a
    , mpc_examples    = exs
    }

