-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE CPP #-}
#include "ghc-api-version.h"

-- | Go to the definition of a variable.
module Development.IDE.LSP.CodeAction
    ( setHandlersCodeAction
    , setHandlersCodeLens
    ) where

import           Language.Haskell.LSP.Types
import Development.IDE.GHC.Compat
import Development.IDE.Core.Rules
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Shake
import Development.IDE.GHC.Error
import Development.IDE.LSP.Server
import Development.IDE.Types.Location
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Messages
import qualified Data.Rope.UTF16 as Rope
import Data.Aeson.Types (toJSON, fromJSON, Value(..), Result(..))
import Control.Monad.Trans.Maybe
import Data.Char
import Data.Maybe
import Data.List.Extra
import qualified Data.Text as T
import Text.Regex.TDFA ((=~), (=~~))
import Text.Regex.TDFA.Text()

-- | Generate code actions.
codeAction
    :: LSP.LspFuncs ()
    -> IdeState
    -> CodeActionParams
    -> IO (List CAResult)
codeAction lsp _ CodeActionParams{_textDocument=TextDocumentIdentifier uri,_context=CodeActionContext{_diagnostics=List xs}} = do
    -- disable logging as its quite verbose
    -- logInfo (ideLogger ide) $ T.pack $ "Code action req: " ++ show arg
    contents <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    let text = Rope.toText . (_text :: VirtualFile -> Rope.Rope) <$> contents
    pure $ List
        [ CACodeAction $ CodeAction title (Just CodeActionQuickFix) (Just $ List [x]) (Just edit) Nothing
        | x <- xs, (title, tedit) <- suggestAction text x
        , let edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
        ]

-- | Generate code lenses.
codeLens
    :: LSP.LspFuncs ()
    -> IdeState
    -> CodeLensParams
    -> IO (List CodeLens)
codeLens _lsp ideState CodeLensParams{_textDocument=TextDocumentIdentifier uri} = do
    case uriToFilePath' uri of
      Just (toNormalizedFilePath -> filePath) -> do
        _ <- runAction ideState $ runMaybeT $ useE TypeCheck filePath
        diag <- getDiagnostics ideState
        hDiag <- getHiddenDiagnostics ideState
        pure $ List
          [ CodeLens _range (Just (Command title "typesignature.add" (Just $ List [toJSON edit]))) Nothing
          | (dFile, _, dDiag@Diagnostic{_range=_range@Range{..},..}) <- diag ++ hDiag
          , dFile == filePath
          , (title, tedit) <- suggestSignature False dDiag
          , let edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
          ]
      Nothing -> pure $ List []

-- | Execute the "typesignature.add" command.
executeAddSignatureCommand
    :: LSP.LspFuncs ()
    -> IdeState
    -> ExecuteCommandParams
    -> IO (Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
executeAddSignatureCommand _lsp _ideState ExecuteCommandParams{..}
    | _command == "typesignature.add"
    , Just (List [edit]) <- _arguments
    , Success wedit <- fromJSON edit 
    = return (Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams wedit))
    | otherwise
    = return (Null, Nothing)

suggestAction  :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestAction text diag = concat
    [ suggestAddExtension diag
    , suggestExtendImport text diag
    , suggestFillHole diag
    , suggestFillTypeWildcard diag
    , suggestFixConstructorImport text diag
    , suggestModuleTypo diag
    , suggestRemoveRedundantImport text diag
    , suggestReplaceIdentifier text diag
    , suggestSignature True diag
    ]


suggestRemoveRedundantImport :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestRemoveRedundantImport contents Diagnostic{_range=_range@Range{..},..}
--     The qualified import of ‘many’ from module ‘Control.Applicative’ is redundant
    | Just [_, bindings] <- matchRegex _message "The( qualified)? import of ‘([^’]*)’ from module [^ ]* is redundant"
    , Just c <- contents
    , importLine <- textInRange _range c
    = [( "Remove " <> bindings <> " from import"
       , [TextEdit _range (dropBindingsFromImportLine (T.splitOn "," bindings) importLine)])]

-- File.hs:16:1: warning:
--     The import of `Data.List' is redundant
--       except perhaps to import instances from `Data.List'
--     To import instances alone, use: import Data.List()
    | _message =~ ("The( qualified)? import of [^ ]* is redundant" :: String)
        = [("Remove import", [TextEdit (extendToWholeLineIfPossible contents _range) ""])]
    | otherwise = []

suggestReplaceIdentifier :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestReplaceIdentifier contents Diagnostic{_range=_range@Range{..},..}
-- File.hs:52:41: error:
--     * Variable not in scope:
--         suggestAcion :: Maybe T.Text -> Range -> Range
--     * Perhaps you meant ‘suggestAction’ (line 83)
-- File.hs:94:37: error:
--     Not in scope: ‘T.isPrfixOf’
--     Perhaps you meant one of these:
--       ‘T.isPrefixOf’ (imported from Data.Text),
--       ‘T.isInfixOf’ (imported from Data.Text),
--       ‘T.isSuffixOf’ (imported from Data.Text)
--     Module ‘Data.Text’ does not export ‘isPrfixOf’.
    | renameSuggestions@(_:_) <- extractRenamableTerms _message
        = [ ("Replace with ‘" <> name <> "’", [mkRenameEdit contents _range name]) | name <- renameSuggestions ]
    | otherwise = []

suggestFillTypeWildcard :: Diagnostic -> [(T.Text, [TextEdit])]
suggestFillTypeWildcard Diagnostic{_range=_range@Range{..},..}
-- Foo.hs:3:8: error:
--     * Found type wildcard `_' standing for `p -> p1 -> p'

    | "Found type wildcard" `T.isInfixOf` _message
    , " standing for " `T.isInfixOf` _message
    , typeSignature <- extractWildCardTypeSignature _message
        =  [("Use type signature: ‘" <> typeSignature <> "’", [TextEdit _range typeSignature])]
    | otherwise = []

suggestAddExtension :: Diagnostic -> [(T.Text, [TextEdit])]
suggestAddExtension Diagnostic{_range=_range@Range{..},..}
-- File.hs:22:8: error:
--     Illegal lambda-case (use -XLambdaCase)
-- File.hs:22:6: error:
--     Illegal view pattern:  x -> foo
--     Use ViewPatterns to enable view patterns
-- File.hs:26:8: error:
--     Illegal `..' in record pattern
--     Use RecordWildCards to permit this
-- File.hs:53:28: error:
--     Illegal tuple section: use TupleSections
-- File.hs:238:29: error:
--     * Can't make a derived instance of `Data FSATrace':
--         You need DeriveDataTypeable to derive an instance for this class
--     * In the data declaration for `FSATrace'
-- C:\Neil\shake\src\Development\Shake\Command.hs:515:31: error:
--     * Illegal equational constraint a ~ ()
--       (Use GADTs or TypeFamilies to permit this)
--     * In the context: a ~ ()
--       While checking an instance declaration
--       In the instance declaration for `Unit (m a)'
    | exts@(_:_) <- filter (`Set.member` ghcExtensions) $ T.split (not . isAlpha) $ T.replace "-X" "" _message
        = [("Add " <> x <> " extension", [TextEdit (Range (Position 0 0) (Position 0 0)) $ "{-# LANGUAGE " <> x <> " #-}\n"]) | x <- exts]
    | otherwise = []

suggestModuleTypo :: Diagnostic -> [(T.Text, [TextEdit])]
suggestModuleTypo Diagnostic{_range=_range@Range{..},..}
-- src/Development/IDE/Core/Compile.hs:58:1: error:
--     Could not find module ‘Data.Cha’
--     Perhaps you meant Data.Char (from base-4.12.0.0)
    | "Could not find module" `T.isInfixOf` _message
    , "Perhaps you meant"     `T.isInfixOf` _message = let
      findSuggestedModules = map (head . T.words) . drop 2 . T.lines
      proposeModule mod = ("replace with " <> mod, [TextEdit _range mod])
      in map proposeModule $ nubOrd $ findSuggestedModules _message
    | otherwise = []

suggestFillHole :: Diagnostic -> [(T.Text, [TextEdit])]
suggestFillHole Diagnostic{_range=_range@Range{..},..}
--  ...Development/IDE/LSP/CodeAction.hs:103:9: warning:
--   * Found hole: _ :: Int -> String
--   * In the expression: _
--     In the expression: _ a
--     In an equation for ‘foo’: foo a = _ a
--   * Relevant bindings include
--       a :: Int
--         (bound at ...Development/IDE/LSP/CodeAction.hs:103:5)
--       foo :: Int -> String
--         (bound at ...Development/IDE/LSP/CodeAction.hs:103:1)
--     Valid hole fits include
--       foo :: Int -> String
--         (bound at ...Development/IDE/LSP/CodeAction.hs:103:1)
--       show :: forall a. Show a => a -> String
--         with show @Int
--         (imported from ‘Prelude’ at ...Development/IDE/LSP/CodeAction.hs:7:8-37
--          (and originally defined in ‘GHC.Show’))
--       mempty :: forall a. Monoid a => a
--         with mempty @(Int -> String)
--         (imported from ‘Prelude’ at ...Development/IDE/LSP/CodeAction.hs:7:8-37
--          (and originally defined in ‘GHC.Base’)) (lsp-ui)

    | topOfHoleFitsMarker `T.isInfixOf` _message = let
      findSuggestedHoleFits :: T.Text -> [T.Text]
      findSuggestedHoleFits = extractFitNames . selectLinesWithFits . dropPreceding . T.lines
      proposeHoleFit name = ("replace hole `" <> holeName <>  "` with " <> name, [TextEdit _range name])
      holeName = T.strip $ last $ T.splitOn ":" $ head . T.splitOn "::" $ head $ filter ("Found hole" `T.isInfixOf`) $ T.lines _message
      dropPreceding       = dropWhile (not . (topOfHoleFitsMarker `T.isInfixOf`))
      selectLinesWithFits = filter ("::" `T.isInfixOf`)
      extractFitNames     = map (T.strip . head . T.splitOn " :: ")
      in map proposeHoleFit $ nubOrd $ findSuggestedHoleFits _message

    | otherwise = []

suggestExtendImport :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestExtendImport contents Diagnostic{_range=_range,..}
    | Just [binding, mod, srcspan] <-
      matchRegex _message
      "Perhaps you want to add ‘([^’]*)’ to the import list in the import of ‘([^’]*)’ *\\((.*)\\).$"
    , Just c <- contents
    = let range = case [ x | (x,"") <- readSrcSpan (T.unpack srcspan)] of
            [s] -> let x = srcSpanToRange s
                   in x{_end = (_end x){_character = succ (_character (_end x))}}
            _ -> error "bug in srcspan parser"
          importLine = textInRange range c
        in [("Add " <> binding <> " to the import list of " <> mod
        , [TextEdit range (addBindingToImportList binding importLine)])]
    | otherwise = []

suggestFixConstructorImport :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestFixConstructorImport _ Diagnostic{_range=_range,..}
    -- ‘Success’ is a data constructor of ‘Result’
    -- To import it use
    -- import Data.Aeson.Types( Result( Success ) )
    -- or
    -- import Data.Aeson.Types( Result(..) ) (lsp-ui)
  | Just [constructor, typ] <-
    matchRegex _message
    "‘([^’]*)’ is a data constructor of ‘([^’]*)’ To import it use"
  = let fixedImport = typ <> "(" <> constructor <> ")"
    in [("Fix import of " <> fixedImport, [TextEdit _range fixedImport])]
  | otherwise = []

suggestSignature :: Bool -> Diagnostic -> [(T.Text, [TextEdit])]
suggestSignature isQuickFix Diagnostic{_range=_range@Range{..},..}
    | "Top-level binding with no type signature" `T.isInfixOf` _message = let
      filterNewlines = T.concat  . T.lines
      unifySpaces    = T.unwords . T.words
      signature      = T.strip $ unifySpaces $ last $ T.splitOn "type signature: " $ filterNewlines _message
      startOfLine    = Position (_line _start) 0
      beforeLine     = Range startOfLine startOfLine
      title          = if isQuickFix then "add signature: " <> signature else signature
      action         = TextEdit beforeLine $ signature <> "\n"
      in [(title, [action])]
suggestSignature isQuickFix Diagnostic{_range=_range@Range{..},..}
    | "Polymorphic local binding with no type signature" `T.isInfixOf` _message = let
      filterNewlines = T.concat  . T.lines
      unifySpaces    = T.unwords . T.words
      signature      = removeInitialForAll
                     $ T.takeWhile (\x -> x/='*' && x/='•')
                     $ T.strip $ unifySpaces $ last $ T.splitOn "type signature: " $ filterNewlines _message
      startOfLine    = Position (_line _start) (_character _start)
      beforeLine     = Range startOfLine startOfLine
      title          = if isQuickFix then "add signature: " <> signature else signature
      action         = TextEdit beforeLine $ signature <> "\n" <> T.replicate (_character _start) " "
      in [(title, [action])]
    where removeInitialForAll :: T.Text -> T.Text
          removeInitialForAll (T.breakOnEnd " :: " -> (nm, ty))
              | "forall" `T.isPrefixOf` ty = nm <> T.drop 2 (snd (T.breakOn "." ty))
              | otherwise                  = nm <> ty
suggestSignature _ _ = []

topOfHoleFitsMarker :: T.Text
topOfHoleFitsMarker =
#if MIN_GHC_API_VERSION(8,6,0)
  "Valid hole fits include"
#else
  "Valid substitutions include"
#endif

mkRenameEdit :: Maybe T.Text -> Range -> T.Text -> TextEdit
mkRenameEdit contents range name =
    if fromMaybe False maybeIsInfixFunction
      then TextEdit range ("`" <> name <> "`")
      else TextEdit range name
  where
    maybeIsInfixFunction = do
      curr <- textInRange range <$> contents
      pure $ "`" `T.isPrefixOf` curr && "`" `T.isSuffixOf` curr

extractWildCardTypeSignature :: T.Text -> T.Text
extractWildCardTypeSignature =
  -- inferring when parens are actually needed around the type signature would
  -- require understanding both the precedence of the context of the _ and of
  -- the signature itself. Inserting them unconditionally is ugly but safe.
  ("(" `T.append`) . (`T.append` ")") .
  T.takeWhile (/='’') . T.dropWhile (=='‘') . T.dropWhile (/='‘') .
  snd . T.breakOnEnd "standing for "

extractRenamableTerms :: T.Text -> [T.Text]
extractRenamableTerms msg
  -- Account for both "Variable not in scope" and "Not in scope"
  | "ot in scope:" `T.isInfixOf` msg = extractSuggestions msg
  | otherwise = []
  where
    extractSuggestions = map getEnclosed
                       . concatMap singleSuggestions
                       . filter isKnownSymbol
                       . T.lines
    singleSuggestions = T.splitOn "), " -- Each suggestion is comma delimited
    isKnownSymbol t = " (imported from" `T.isInfixOf` t || " (line " `T.isInfixOf` t
    getEnclosed = T.dropWhile (== '‘')
                . T.dropWhileEnd (== '’')
                . T.dropAround (\c -> c /= '‘' && c /= '’')

-- | If a range takes up a whole line (it begins at the start of the line and there's only whitespace
-- between the end of the range and the next newline), extend the range to take up the whole line.
extendToWholeLineIfPossible :: Maybe T.Text -> Range -> Range
extendToWholeLineIfPossible contents range@Range{..} =
    let newlineAfter = maybe False (T.isPrefixOf "\n" . T.dropWhile (\x -> isSpace x && x /= '\n') . snd . splitTextAtPosition _end) contents
        extend = newlineAfter && _character _start == 0 -- takes up an entire line, so remove the whole line
    in if extend then Range _start (Position (_line _end + 1) 0) else range

-- | All the GHC extensions
ghcExtensions :: Set.HashSet T.Text
ghcExtensions = Set.fromList $ map (T.pack . show) ghcEnumerateExtensions

splitTextAtPosition :: Position -> T.Text -> (T.Text, T.Text)
splitTextAtPosition (Position row col) x
    | (preRow, mid:postRow) <- splitAt row $ T.splitOn "\n" x
    , (preCol, postCol) <- T.splitAt col mid
        = (T.intercalate "\n" $ preRow ++ [preCol], T.intercalate "\n" $ postCol : postRow)
    | otherwise = (x, T.empty)

-- | Returns [start .. end[
textInRange :: Range -> T.Text -> T.Text
textInRange (Range (Position startRow startCol) (Position endRow endCol)) text =
    case compare startRow endRow of
      LT ->
        let (linesInRangeBeforeEndLine, endLineAndFurtherLines) = splitAt (endRow - startRow) linesBeginningWithStartLine
            (textInRangeInFirstLine, linesBetween) = case linesInRangeBeforeEndLine of
              [] -> ("", [])
              firstLine:linesInBetween -> (T.drop startCol firstLine, linesInBetween)
            maybeTextInRangeInEndLine = T.take endCol <$> listToMaybe endLineAndFurtherLines
        in T.intercalate "\n" (textInRangeInFirstLine : linesBetween ++ maybeToList maybeTextInRangeInEndLine)
      EQ ->
        let line = fromMaybe "" (listToMaybe linesBeginningWithStartLine)
        in T.take (endCol - startCol) (T.drop startCol line)
      GT -> ""
    where
      linesBeginningWithStartLine = drop startRow (T.splitOn "\n" text)

-- | Drop all occurrences of a binding in an import line.
--   Preserves well-formedness but not whitespace between bindings.
--
-- >>> dropBindingsFromImportLine ["bA", "bC"] "import A(bA, bB,bC ,bA)"
--  "import A(bB)"
--
-- >>> dropBindingsFromImportLine ["+"] "import "P" qualified A as B ((+))"
--  "import "P" qualified A() as B hiding (bB)"
dropBindingsFromImportLine :: [T.Text] -> T.Text -> T.Text
dropBindingsFromImportLine bindings_ importLine =
      importPre <> "(" <> importRest'
    where
      bindings = map (wrapOperatorInParens . removeQualified) bindings_

      (importPre, importRest) = T.breakOn "(" importLine

      wrapOperatorInParens x = if isAlpha (T.head x) then x else "(" <> x <> ")"

      removeQualified x = case T.breakOn "." x of
        (_qualifier, T.uncons -> Just (_, unqualified)) -> unqualified
        _ -> x

      importRest' = case T.uncons importRest of
        Just (_, x) ->
          T.intercalate ","
            $ joinCloseParens
            $ mapMaybe (filtering . T.strip)
            $ T.splitOn "," x
        Nothing -> importRest

      filtering x = case () of
        () | x `elem` bindings -> Nothing
        () | x `elem` map (<> ")") bindings -> Just ")"
        _                      -> Just x

      joinCloseParens (x : ")" : rest) = (x <> ")") : joinCloseParens rest
      joinCloseParens (x       : rest) = x : joinCloseParens rest
      joinCloseParens []               = []

-- | Extends an import list with a new binding.
--   Assumes an import statement of the form:
--       import (qualified) A (..) ..
--   Places the new binding first, preserving whitespace.
--   Copes with multi-line import lists
addBindingToImportList :: T.Text -> T.Text -> T.Text
addBindingToImportList binding importLine = case T.breakOn "(" importLine of
    (pre, T.uncons -> Just (_, rest)) ->
      case T.uncons (T.dropWhile isSpace rest) of
        Just (')', _) -> T.concat [pre, "(", binding, rest]
        _             -> T.concat [pre, "(", binding, ", ", rest]
    _ ->
      error
        $  "importLine does not have the expected structure: "
        <> T.unpack importLine

-- | Returns Just (the submatches) for the first capture, or Nothing.
matchRegex :: T.Text -> T.Text -> Maybe [T.Text]
matchRegex message regex = case T.unwords (T.words message) =~~ regex of
    Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, bindings) -> Just bindings
    Nothing -> Nothing

setHandlersCodeAction :: PartialHandlers
setHandlersCodeAction = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.codeActionHandler = withResponse RspCodeAction codeAction
    }

setHandlersCodeLens :: PartialHandlers
setHandlersCodeLens = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.codeLensHandler = withResponse RspCodeLens codeLens,
    LSP.executeCommandHandler = withResponseAndRequest RspExecuteCommand ReqApplyWorkspaceEdit executeAddSignatureCommand
    }
