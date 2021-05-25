{-# LANGUAGE OverloadedStrings #-}

module Wingman.Metaprogramming.Parser.Documentation where

import Data.String (IsString)
import Data.Text (Text)
import GhcPlugins (OccName)
import Wingman.Metaprogramming.Lexer (Parser, identifier, variable)
import Wingman.Types (TacticsM)
import qualified Text.Megaparsec as P
import Data.Functor ((<&>))
import           Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.String (renderString)


data Count a where
  One :: Count OccName
  Many :: Count [OccName]

data Syntax a where
  Nullary :: Syntax (Parser (TacticsM ()))
  Ref :: Count a -> Syntax (a -> Parser (TacticsM ()))
  Bind :: Count a -> Syntax (a -> Parser (TacticsM ()))

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
  , mpc_description :: Text
  , mpc_tactic      :: a
  , mpc_examples    :: [Example]
  }

data SomeMetaprogramCommand where
  SMC :: MetaprogramCommand a -> SomeMetaprogramCommand


makeMPParser :: MetaprogramCommand a -> Parser (TacticsM ())
makeMPParser (MC name Nullary _ tactic _) = do
  identifier name
  tactic
makeMPParser (MC name (Ref One) _ tactic _) = do
  identifier name
  variable >>= tactic
makeMPParser (MC name (Ref Many) _ tactic _) = do
  identifier name
  P.many variable >>= tactic
makeMPParser (MC name (Bind One) _ tactic _) = do
  identifier name
  variable >>= tactic
makeMPParser (MC name (Bind Many) _ tactic _) = do
  identifier name
  P.many variable >>= tactic


makeParser :: [SomeMetaprogramCommand] -> Parser (TacticsM ())
makeParser ps = P.choice $ ps <&> \(SMC mp) -> makeMPParser mp

prettyCommand :: MetaprogramCommand a -> Doc b
prettyCommand (MC name syn desc _ exs) = vsep
  [ "##" <+> pretty name
  , mempty
  , pretty desc
  , mempty
  , mempty
  , "### Examples"
  , mempty
  , concatWith (\a b -> vsep [a, "---", b]) $ fmap (prettyExample name) exs
  ]


prettyHyInfo :: ExampleHyInfo -> Doc a
prettyHyInfo hi = pretty (ehi_name hi) <+> "::" <+> pretty (ehi_type hi)

prettyExample :: Text -> Example -> Doc a
prettyExample name (Example m_txt args hys goal res) =
  align $ vsep
    [ maybe mempty ((<> line) . pretty) m_txt
    , codeFence $ vsep
        $ fmap prettyHyInfo hys
           <> [ mempty
              , "_" <+> maybe mempty (("::" <+>). pretty) goal
              ]
    , mempty
    , ">" <+> enclose "`" "`" (pretty name <+> hsep (fmap pretty args))
    , mempty
    , codeFence $ align $ pretty res
    ]

codeFence :: Doc a -> Doc a
codeFence d = align $ vsep
  [ "```haskell"
  , d
  , "```"
  ]

dump :: SomeMetaprogramCommand -> String
dump (SMC mc)
  = renderString
  . layoutPretty defaultLayoutOptions
  $ prettyCommand mc



-- makeReadme :: MetaprogramCommand a -> T.Text
-- m


command :: Text -> Syntax a -> Text -> a -> [Example] -> SomeMetaprogramCommand
command txt syn txt' a exs = SMC $
  MC
    { mpc_name        = txt
    , mpc_syntax      = syn
    , mpc_description = txt'
    , mpc_tactic      = a
    , mpc_examples    = exs
    }

