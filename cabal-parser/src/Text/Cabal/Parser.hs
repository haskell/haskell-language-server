{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Cabal.Parser where

import           Data.Functor         (void)
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe)
import qualified Data.Text            as T
import           Text.Cabal.Data
import           Text.Cabal.Types
import           Text.Cabal.Value
import           Text.Megaparsec
import           Text.Megaparsec.Char

--------------------------------
-- Parser
--------------------------------

parseCabalFile :: FilePath -> T.Text -> Either ErrorBundle CabalAST
parseCabalFile srcFile contents = do
  let parsed = parse cabalParser srcFile contents
  parsed

{- | Parses a cabal file into an AST structure of the cabal file's fields and stanzas.

 Requires the cabal file to be valid, otherwise the parser's behaviour is undefined.
 Ignores any comments and empty lines.
-}
cabalParser :: Parser CabalAST
cabalParser = do
  (items, loc) <- annotateSrcLoc $ do
    throwAwayLines
    items <-
      sepBy
        (stanzaParser <|> topLevelFieldParser)
        throwAwayLines
    throwAwayLines
    eof
    pure items
  pure $ CabalAST items (Annotation Nothing loc)

{- | Parses a top-level field which is a field with no indentation.
-}
topLevelFieldParser :: Parser ASTItem
topLevelFieldParser = do
  (_, field) <- fieldParser $ Just 0
  pure $ FieldItem field

{- | Parses a field with a possible indentation.

 Returns the indentation of the parsed field and the field.
 In order to parse a field, first a keyword has to be parsed
 then an arbitrary number of values which are parsed
 according to the keyword's value parser.
-}
fieldParser :: Maybe Int -> Parser (Maybe Int, FieldItem)
fieldParser indentation = do
  indentNum <-
    case indentation of
      Nothing -> do
        indents <- many $ char ' '
        pure $ length indents
      Just indentNum -> do
        indentationParser indentNum
        pure indentNum
  hspace
  ((kw, vals), fieldLoc) <- annotateSrcLoc $ do
    (keyword, kwLoc) <- annotateSrcLoc $ choice [keywordParser, externalKeywordParser]
    let valParser = fromMaybe defaultValueParser $ M.lookup keyword allKeywords
    values <- valuesParser valParser indentNum
    pure (KeyWord keyword (Annotation Nothing kwLoc), values)
  pure (Just indentNum, Field kw vals (Annotation Nothing fieldLoc))
 where
  -- any keyword can also be written in the cabal file,
  -- when prefixed with a 'x-', indicating it to be external
  externalKeywordParser :: Parser T.Text
  externalKeywordParser = do
    prefix <- string' "x-"
    kw <- keywordParser
    pure $ prefix <> kw

  -- a keyword is some word ending with a colon
  keywordParser :: Parser T.Text
  keywordParser = do
    kw <- many $ noneOf [':', '\r', '\n']
    suffix <- string ":"
    pure $ T.pack kw <> suffix

  -- tries to apply the value parser in braces or without them,
  -- the boolean signifies whether the called parser is in a braces context,
  -- i.e. whether indentation should be respected.
  -- when no values can be parsed with the first two options, we assume
  -- that there are no values
  valuesParser :: (Bool -> Parser [ValueItem]) -> Int -> Parser ValueItems
  valuesParser valParser' indentNum' =
    choice
      [ label "braces enclosed " $ try $ do
          ((parsedValues, valsLoc), braces) <- annotateBraces $ annotateSrcLoc $ valueLinesParser Nothing $ valParser' True
          pure $ Values parsedValues (Annotation (Just braces) valsLoc)
      , label "not enclosed" $ try $ do
          (parsedValues, valsLoc) <- annotateSrcLoc $ valueLinesParser (Just indentNum') $ valParser' False
          pure $ Values parsedValues (Annotation Nothing valsLoc)
      , do
          (_, loc) <- annotateSrcLoc $ pure ()
          pure $ Values [] (Annotation Nothing loc)
      ]

{- | Parses a field with a possible indentation.

 Returns the indentation of the parsed field and the field.
 In order to parse a field, first a known keyword has to be parsed
 and then a number of values which are parsed according to the keyword's value parser.
-}
stanzaFieldItemParser :: Maybe Int -> Parser (Maybe Int, StanzaElement)
stanzaFieldItemParser indentation = do
  (indentNum, sItem) <-
    choice
      [ try $ stanzaConditionalParser indentation
      , do
          (iNum, fI) <- fieldParser indentation
          pure (iNum, StanzaField fI)
      ]
  pure (indentNum, sItem)

{- | Parses a conditional block inside a stanza with a possible indentation.

  Returns the indentation of the parsed block and the conditional block itself.
  Parses either an if, ifelse or else block.
-}
stanzaConditionalParser :: Maybe Int -> Parser (Maybe Int, StanzaElement)
stanzaConditionalParser indentation = do
  indentNum <-
    case indentation of
      Nothing -> do
        indents <- many $ char ' '
        pure $ length indents
      Just indentNum -> do
        indentationParser indentNum
        pure indentNum
  hspace
  cond <-
    choice
      [ conditionalParser ifConditionParser
      , conditionalParser elifConditionParser
      , conditionalParser elseConditionParser
      ]
  pure (Just indentNum, StanzaConditional cond)

{- | Parses the conditional block corresponding to the given parser.

  A conditional block consists of a conditional declaration and possible condition
  (defined by the given parser) and a number of fields guarded by the condition.
-}
conditionalParser :: Parser ConditionItem -> Parser ConditionalItem
conditionalParser condParser = do
  ((condDecl, condFields), loc) <- annotateSrcLoc $ do
    condItem <- condParser
    throwAwayLines
    fields <- stanzaFieldsParser
    pure (condItem, fields)
  throwAwayLines
  pure $ Conditional condDecl condFields (Annotation Nothing loc)

-- | Parses an if declaration and condition with possible trailing spaces.
ifConditionParser :: Parser ConditionItem
ifConditionParser = do
  ((ifTxt, ifCond), loc) <- annotateSrcLoc $ do
    ifTxt' <- ifTextParser
    hspace
    ifCond' <- conditionParser
    pure (ifTxt', ifCond')
  pure $ IfCondition ifTxt ifCond (Annotation Nothing loc)

-- | Parses an elif declaration and condition with possible trailing spaces.
elifConditionParser :: Parser ConditionItem
elifConditionParser = do
  ((elifTxt, elifCond), loc) <- annotateSrcLoc $ do
    elifTxt' <- elifTextParser
    hspace
    elifCond' <- conditionParser
    pure (elifTxt', elifCond')
  pure $ ElIfCondition elifTxt elifCond (Annotation Nothing loc)

-- | Parses an else declaration with possible trailing spaces.
elseConditionParser :: Parser ConditionItem
elseConditionParser = do
  (txt, loc) <- annotateSrcLoc $ do
    txt' <- elseTextParser
    hspace
    pure txt'
  pure $ ElseCondition txt (Annotation Nothing loc)

{- | No white spaces before are parsed,
preserves capitalization of else in original file.
-}
elseTextParser :: Parser ElseConditionItem
elseTextParser = do
  (elifTxt, loc) <- annotateSrcLoc $ string' "else"
  pure $ Else elifTxt (Annotation Nothing loc)

{- | No white spaces before are parsed,
preserves capitalization of elif in original file.
-}
elifTextParser :: Parser ElIfConditionItem
elifTextParser = do
  -- preserves case of if in original file
  (elifTxt, loc) <- annotateSrcLoc $ string' "elif"
  pure $ ElIf elifTxt (Annotation Nothing loc)

{- | No white spaces before are parsed,
preserves capitalization of if in original file.
-}
ifTextParser :: Parser IfConditionItem
ifTextParser = do
  --
  (ifTxt, loc) <- annotateSrcLoc $ string' "if"
  pure $ If ifTxt (Annotation Nothing loc)

conditionParser :: Parser Condition
conditionParser = do
  (cond, loc) <-
    annotateSrcLoc $ many $ noneOf ['\n', '{','}']
  pure $ Cond (T.pack cond) (Annotation Nothing loc)

{- | Parses a stanza into a StanzaItem.

 First a stanza declaration is parsed and then
 an arbitrary number of fields with an indentation that is
 determined when parsing the first line of fields.
-}
stanzaParser :: Parser ASTItem
stanzaParser = do
  ((stanzaDecl, fieldItems), loc) <- annotateSrcLoc $ do
    stanzaDecl' <- stanzaDeclarationParser
    throwAwayLines
    fields <- stanzaFieldsParser
    pure (stanzaDecl', fields)
  pure $ StanzaItem (Stanza stanzaDecl fieldItems (Annotation Nothing loc))

stanzaFieldsParser :: Parser StanzaElements
stanzaFieldsParser =
  choice
    [ label "braces enclosed " $ try $ do
        ((parsedValues, valsLoc), braces) <- annotateBraces $ annotateSrcLoc $ stanzaFieldItemsParserIndentM Nothing
        throwAwayLines
        pure $ StanzaElements parsedValues (Annotation (Just braces) valsLoc)
    , label "not enclosed" $ try $ do
        (parsedValues, valsLoc) <- annotateSrcLoc indentedStanzaFieldItemsParser
        pure $ StanzaElements parsedValues (Annotation Nothing valsLoc)
    ]

{- | Parses a list of field items in a stanza.

  First the field parser is called with undecided indentation
  in order to figure out the indentation for fields in the stanza,
  after the indentation is determined, an arbitrary number of fields
  with this indentation is parsed.
-}
indentedStanzaFieldItemsParser :: Parser [StanzaElement]
indentedStanzaFieldItemsParser = do
  (indentNum, firstField) <- stanzaFieldItemParser Nothing
  stanzaFields <- stanzaFieldItemsParserIndentM indentNum
  pure $ [firstField] <> stanzaFields

stanzaFieldItemsParserIndentM :: Maybe Int -> Parser [StanzaElement]
stanzaFieldItemsParserIndentM indent = do
  stanzaFields <- many $ try $ do
    throwAwayLines
    (_, field) <- stanzaFieldItemParser indent
    pure field
  throwAwayLines
  pure stanzaFields

{- | Parses a stanza context declaration, which consists of
 a stanza type and a possible stanza name
 with an arbitrary number of spaces in-between.
-}
stanzaDeclarationParser :: Parser StanzaDecl
stanzaDeclarationParser = do
  ((sType, sName), loc) <- annotateSrcLoc $ do
    sType' <- stanzaTypeParser
    hspace
    sName' <- stanzaNameParser
    pure (sType', sName')
  throwAwayLines
  pure $ StanzaDecl sType sName (Annotation Nothing loc)
 where
  --  Parses a stanza type which has to match the list of existing stanza types
  --  based on the cabal specification, does not take care of spaces
  stanzaTypeParser :: Parser StanzaTypeItem
  stanzaTypeParser = do
    (sType, loc) <- annotateSrcLoc $ choice (map string' $ M.keys stanzaKeywordMap)
    pure $ StanzaType sType (Annotation Nothing loc)

  --  Parses a stanza name.
  stanzaNameParser :: Parser (Maybe StanzaNameItem)
  stanzaNameParser = do
    (sNameM, loc) <-
      annotateSrcLoc $ many (alphaNumChar <|> char '-' <|> char '_')
    case sNameM of
      "" -> pure Nothing
      _  -> pure $ Just $ StanzaName (T.pack sNameM) (Annotation Nothing loc)

{- | Parses an arbitrary number of lines with the given indentation.
-}
valueLinesParser :: Maybe Int -> Parser [ValueItem] -> Parser [ValueItem]
valueLinesParser indentNumM p = do
  r <- optional p
  res <- valueLineParser
  throwAwayLines
  pure $ fromMaybe [] r <> res
 where
  --  Parses a single line with possibly multiple values.
  valueLineParser :: Parser [ValueItem]
  valueLineParser = do
    res <- many $ try $ do
      throwAwayLines
      case indentNumM of
        Just indentNum -> do
          indentationParser indentNum
          void $ char ' '
        Nothing -> pure ()
      r <- p
      pure r
    pure $ concat res

-- | Discards the given number of spaces
indentationParser :: Int -> Parser ()
indentationParser indentNum = do
  void $ string $ T.replicate indentNum " "
