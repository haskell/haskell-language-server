{-# LANGUAGE OverloadedStrings #-}

module Text.Cabal.Types where

import qualified Data.Text                   as T
import           Data.Void                   (Void)
import           Language.LSP.Protocol.Types
import qualified Language.LSP.Protocol.Types as LSP (Position (..), Range (..))
import           Prettyprinter
import           Text.Megaparsec             (Parsec)
import           Text.Megaparsec.Error       (ParseErrorBundle,
                                              errorBundlePretty)

-----------------------------------------------
-- Parser
-----------------------------------------------

type Parser = Parsec Void T.Text

type ErrorBundle = ParseErrorBundle T.Text Void

-----------------------------------------------
-- Cabal File Structure Representation
-----------------------------------------------

{- | An abstract syntax tree representing a cabal file.

  The cabal file is represented by a list of AST items
  and the source range of the file (0,0 up until the end of the file).
-}
data CabalAST = CabalAST [ASTItem] Annotation
  deriving (Show, Eq, Ord)

{- | Represents an annotation any element in the ast can have.

  Meaning:
  * whether the element is wrapped in braces and, if so,
  where these braces are located,
  * the range of the element, i.e. where in the cabal file is it
  located.
-}
data Annotation = Annotation (Maybe Braces) LSP.Range
  deriving (Show, Eq, Ord)

data Braces = Braces
  { openingBrace :: LSP.Position
  , closingBrace :: LSP.Position
  }
  deriving (Show, Eq, Ord)

{- | An AST item which can either represent a field or
  stanza.
-}
data ASTItem = FieldItem FieldItem | StanzaItem StanzaItem
  deriving (Show, Eq, Ord)

{- | Represents a stanza in a cabal file and consists
  of the stanza's declaration and a list of its elements
  which can be conditionals or fields.

  Range: From the start of the stanza declaration
  up until the end of the last field item.
-}
data StanzaItem = Stanza StanzaDecl StanzaElements Annotation
  deriving (Show, Eq, Ord)

{- | Represents a list of elements in a stanza, these are either
  conditional blocks or fields and can be wrapped in braces.
-}
data StanzaElements = StanzaElements [StanzaElement] Annotation
  deriving (Show, Eq, Ord)

{- | Represents a stanza declaration, which consists of
  the stanza's type and possibly the stanza's name, in case
  it is named.

  Range: From the start of the stanza type until the and of the
  stanza name or until the end of the stanza type in case the
  stanza is unnamed.
-}
data StanzaDecl = StanzaDecl StanzaTypeItem (Maybe StanzaNameItem) Annotation
  deriving (Show, Eq, Ord)

{- | Represents a field in a cabal file which consists of
  a key word item and the values of the field.

  Range: From the start of the key word item until the
  end of the last value.
-}
data FieldItem = Field KeyWordItem ValueItems Annotation
  deriving (Show, Eq, Ord)

{- | Represents either a stanza's field or a conditional block
  inside the stanza.
-}
data StanzaElement = StanzaField FieldItem | StanzaConditional ConditionalItem
  deriving (Show, Eq, Ord)

{- | Represents a conditional block, containing the conditional's
  keyword and condition and the list of stanza elements that apply
  if the conditional is satisfied.

  Range: From the start of the conditional's keyword until the end of
  the last stanza element in the list.
-}
data ConditionalItem = Conditional ConditionItem StanzaElements Annotation
  deriving (Show, Eq, Ord)

{- | Represents either an if, elif or else keyword and the corresponding
  condition.

  Range: From the start of the keyword until the end of the condition
  formula.
-}
data ConditionItem
  = IfCondition IfConditionItem Condition Annotation
  | ElIfCondition ElIfConditionItem Condition Annotation
  | ElseCondition ElseConditionItem Annotation
  deriving (Show, Eq, Ord)

data IfConditionItem = If T.Text Annotation
  deriving (Show, Eq, Ord)

data ElIfConditionItem = ElIf T.Text Annotation
  deriving (Show, Eq, Ord)

data ElseConditionItem = Else T.Text Annotation
  deriving (Show, Eq, Ord)

{- | Represents a condition formula in a conditional
  block.

  Range: From the start of the formula until the end.
-}
data Condition = Cond T.Text Annotation
  deriving (Show, Eq, Ord)

{- | Represents the values of a field in a cabal file
  in the form of a list of value items.

  Can be wrapped in braces.
  Range: From the start of the first value up until
  the end of the last value in the list.
-}
data ValueItems = Values [ValueItem] Annotation
  deriving (Show, Eq, Ord)

{- | Represents a keyword in a cabal file,
  can be a key word in a stanza or in the top
  level.

  Range: From the start of the keyword up until
  after the semi-colon.
-}
data KeyWordItem = KeyWord T.Text Annotation
  deriving (Show, Eq, Ord)

{- | Represents a single value in a field.

  The values may contain some commas and white spaces,
  so these might need to be stripped when one wants to
  read the actual values.

  Range: From the start of the value up until the end.
-}
data ValueItem = Value T.Text Annotation
  deriving (Show, Eq, Ord)

{- | Represents the name of a named stanza.

  Range: From the start of the name up until the end.
-}
data StanzaNameItem = StanzaName T.Text Annotation
  deriving (Show, Ord, Eq)

{- | Represents the declaration of a stanza type.

  Range: From the start of the declared type up until the end.
-}
data StanzaTypeItem = StanzaType T.Text Annotation
  deriving (Show, Ord, Eq)

---------------------------------------------
-- Pretty implementations
---------------------------------------------

instance Pretty ASTItem where
  pretty (FieldItem item)  = pretty item
  pretty (StanzaItem item) = pretty item

instance Pretty Annotation where
  pretty (Annotation (Just bracesInfo) range) =
    vcat
      [ pretty bracesInfo
      , indent 2 $ prettyRange range
      ]
  pretty (Annotation Nothing range) =
    indent 2 $ prettyRange range

instance Pretty Braces where
  pretty bracesInfo =
      "In braces:" <+> (prettyRange $ rangeFromBraces bracesInfo)

instance Pretty StanzaItem where
  pretty (Stanza decl fields anno) =
    vcat
      [ pretty decl
      , indent 2 $ pretty fields
      , indent 4 $ pretty anno
      ]

instance Pretty StanzaElements where
  pretty (StanzaElements elems anno) =
    (vcat $ map pretty elems)
    <+> (indent 2 $ pretty anno)


instance Pretty StanzaElement where
  pretty (StanzaField fI)       = pretty fI
  pretty (StanzaConditional sC) = pretty sC

instance Pretty StanzaDecl where
  pretty (StanzaDecl sType sNameM anno) =
    pretty sType
      <+> maybe mempty pretty sNameM
      <+> pretty anno

instance Pretty FieldItem where
  pretty (Field kwItem values _) =
    vcat
      [ pretty kwItem
      , indent 2 $ pretty values
      ]

instance Pretty ConditionalItem where
  pretty (Conditional cond siItems r) =
    vcat $
      [pretty cond]
        <> [pretty siItems]
        <> [indent 2 $ pretty r]
instance Pretty ConditionItem where
  pretty (IfCondition cI c r) =
    vcat $
      [pretty cI <+> pretty c]
        <> [indent 2 $ pretty r]
  pretty (ElIfCondition cI c r) =
    vcat $
      [pretty cI <+> pretty c]
        <> [indent 2 $ pretty r]
  pretty (ElseCondition cI r) =
    vcat $
      [pretty cI]
        <> [indent 2 $ pretty r]

instance Pretty IfConditionItem where
  pretty (If txt r) =
    viaShow txt
      <+> pretty r

instance Pretty ElIfConditionItem where
  pretty (ElIf txt r) =
    viaShow txt
      <+> (indent 2 $ pretty r)

instance Pretty ElseConditionItem where
  pretty (Else txt r) =
    viaShow txt
      <+> (indent 2 $ pretty r)

instance Pretty Condition where
  pretty (Cond c r) =
    viaShow c
      <+> (indent 2 $ pretty r)

instance Pretty ValueItems where
  pretty (Values values anno) =
    vcat
      [ "["
      , vsep $ fmap (indent 2 . pretty) values
      , "]" <+> pretty anno
      ]

instance Pretty KeyWordItem where
  pretty (KeyWord kwName anno) =
    pretty kwName <+> pretty anno

instance Pretty ValueItem where
  pretty (Value val anno) =
    pretty val <+> pretty anno

instance Pretty CabalAST where
  pretty (CabalAST items anno) =
    vcat
      [ vcat $ map pretty items
      , indent 2 $ pretty anno
      ]

instance Pretty StanzaNameItem where
  pretty (StanzaName name anno) =
    pretty name <+> pretty anno

instance Pretty StanzaTypeItem where
  pretty (StanzaType t anno) =
    pretty t <+> pretty anno

rangeFromBraces :: Braces -> LSP.Range
rangeFromBraces braces' = LSP.Range (openingBrace braces') (closingBrace braces')

prettyRange :: Range -> Doc ann
prettyRange (LSP.Range start end) = parens (prettyPos start <> colon <> prettyPos end)

prettyPos :: Position -> Doc ann
prettyPos (Position l ch) = parens (viaShow l <> comma <> viaShow ch)

myPretty :: (Pretty a) => Either ErrorBundle a -> String
myPretty (Right ast) = show (pretty ast)
myPretty (Left err)  = errorBundlePretty err

-----------------------------
-- Utils
-----------------------------

{- | Returns whether the field item is a keyword of the given type.

  Independent of capitalization.

  Note that the given text should not have a semicolon at the end.
-}
hasFieldType :: T.Text -> FieldItem -> Bool
hasFieldType type' (Field (KeyWord kwTxt _) _ _) =
  T.isPrefixOf (T.toLower type') (T.toLower kwTxt)
