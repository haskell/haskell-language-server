-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE CPP                   #-}
#include "ghc-api-version.h"

-- | Go to the definition of a variable.
module Development.IDE.Plugin.CodeAction
    (
      plugin

    -- * For haskell-language-server
    , codeAction
    , codeLens
    , rulePackageExports
    , commandHandler

    -- * For testing
    , blockCommandId
    , typeSignatureCommandId
    , matchRegExMultipleImports
    ) where

import Control.Monad (join, guard)
import Development.IDE.Plugin
import Development.IDE.GHC.Compat
import Development.IDE.Core.Rules
import Development.IDE.Core.RuleTypes
import Development.IDE.Core.Service
import Development.IDE.Core.Shake
import Development.IDE.GHC.Error
import Development.IDE.LSP.Server
import Development.IDE.Plugin.CodeAction.PositionIndexed
import Development.IDE.Plugin.CodeAction.RuleTypes
import Development.IDE.Plugin.CodeAction.Rules
import Development.IDE.Types.Exports
import Development.IDE.Types.Location
import Development.IDE.Types.Options
import Development.Shake (Rules)
import qualified Data.HashMap.Strict as Map
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Messages
import Language.Haskell.LSP.Types
import qualified Data.Rope.UTF16 as Rope
import Data.Aeson.Types (toJSON, fromJSON, Value(..), Result(..))
import Data.Char
import Data.Maybe
import Data.List.Extra
import qualified Data.Text as T
import Text.Regex.TDFA (mrAfter, (=~), (=~~))
import Outputable (ppr, showSDocUnsafe)
import Data.Function
import Control.Arrow ((>>>))
import Data.Functor
import Control.Applicative ((<|>))
import Safe (atMay)
import Bag (isEmptyBag)
import qualified Data.HashSet as Set
import Control.Concurrent.Extra (threadDelay, readVar)

plugin :: Plugin c
plugin = codeActionPluginWithRules rules codeAction <> Plugin mempty setHandlersCodeLens

rules :: Rules ()
rules = rulePackageExports

-- | a command that blocks forever. Used for testing
blockCommandId :: T.Text
blockCommandId = "ghcide.command.block"

typeSignatureCommandId :: T.Text
typeSignatureCommandId = "typesignature.add"

-- | Generate code actions.
codeAction
    :: LSP.LspFuncs c
    -> IdeState
    -> TextDocumentIdentifier
    -> Range
    -> CodeActionContext
    -> IO (Either ResponseError [CAResult])
codeAction lsp state (TextDocumentIdentifier uri) _range CodeActionContext{_diagnostics=List xs} = do
    contents <- LSP.getVirtualFileFunc lsp $ toNormalizedUri uri
    let text = Rope.toText . (_text :: VirtualFile -> Rope.Rope) <$> contents
        mbFile = toNormalizedFilePath' <$> uriToFilePath uri
    diag <- fmap (\(_, _, d) -> d) . filter (\(p, _, _) -> mbFile == Just p) <$> getDiagnostics state
    (ideOptions, join -> parsedModule, join -> env) <- runAction "CodeAction" state $
      (,,) <$> getIdeOptions
            <*> getParsedModule `traverse` mbFile
            <*> use GhcSession `traverse` mbFile
    -- This is quite expensive 0.6-0.7s on GHC
    pkgExports <- runAction "CodeAction:PackageExports" state $ (useNoFile_ . PackageExports) `traverse` env
    localExports <- readVar (exportsMap $ shakeExtras state)
    let exportsMap = localExports <> fromMaybe mempty pkgExports
    pure . Right $
        [ CACodeAction $ CodeAction title (Just CodeActionQuickFix) (Just $ List [x]) (Just edit) Nothing
        | x <- xs, (title, tedit) <- suggestAction exportsMap ideOptions parsedModule text x
        , let edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
        ] <> caRemoveRedundantImports parsedModule text diag xs uri

-- | Generate code lenses.
codeLens
    :: LSP.LspFuncs c
    -> IdeState
    -> CodeLensParams
    -> IO (Either ResponseError (List CodeLens))
codeLens _lsp ideState CodeLensParams{_textDocument=TextDocumentIdentifier uri} = do
    commandId <- makeLspCommandId "typesignature.add"
    fmap (Right . List) $ case uriToFilePath' uri of
      Just (toNormalizedFilePath' -> filePath) -> do
        _ <- runAction "codeLens" ideState (use TypeCheck filePath)
        diag <- getDiagnostics ideState
        hDiag <- getHiddenDiagnostics ideState
        pure
          [ CodeLens _range (Just (Command title commandId (Just $ List [toJSON edit]))) Nothing
          | (dFile, _, dDiag@Diagnostic{_range=_range}) <- diag ++ hDiag
          , dFile == filePath
          , (title, tedit) <- suggestSignature False dDiag
          , let edit = WorkspaceEdit (Just $ Map.singleton uri $ List tedit) Nothing
          ]
      Nothing -> pure []

-- | Execute the "typesignature.add" command.
commandHandler
    :: LSP.LspFuncs c
    -> IdeState
    -> ExecuteCommandParams
    -> IO (Either ResponseError Value, Maybe (ServerMethod, ApplyWorkspaceEditParams))
commandHandler lsp _ideState ExecuteCommandParams{..}
    -- _command is prefixed with a process ID, because certain clients
    -- have a global command registry, and all commands must be
    -- unique. And there can be more than one ghcide instance running
    -- at a time against the same client.
    | T.isSuffixOf blockCommandId _command
    = do
        LSP.sendFunc lsp $ NotCustomServer $
            NotificationMessage "2.0" (CustomServerMethod "ghcide/blocking/command") Null
        threadDelay maxBound
        return (Right Null, Nothing)
    | T.isSuffixOf typeSignatureCommandId _command
    , Just (List [edit]) <- _arguments
    , Success wedit <- fromJSON edit
    = return (Right Null, Just (WorkspaceApplyEdit, ApplyWorkspaceEditParams wedit))
    | otherwise
    = return (Right Null, Nothing)

suggestAction
  :: ExportsMap
  -> IdeOptions
  -> Maybe ParsedModule
  -> Maybe T.Text
  -> Diagnostic
  -> [(T.Text, [TextEdit])]
suggestAction packageExports ideOptions parsedModule text diag = concat
   -- Order these suggestions by priority
    [ suggestSignature True diag
    , suggestExtendImport packageExports text diag
    , suggestFillTypeWildcard diag
    , suggestFixConstructorImport text diag
    , suggestModuleTypo diag
    , suggestReplaceIdentifier text diag
    , removeRedundantConstraints text diag
    , suggestAddTypeAnnotationToSatisfyContraints text diag
    ] ++ concat
    [  suggestConstraint pm text diag
    ++ suggestNewDefinition ideOptions pm text diag
    ++ suggestNewImport packageExports pm diag
    ++ suggestDeleteUnusedBinding pm text diag
    ++ suggestExportUnusedTopBinding text pm diag
    | Just pm <- [parsedModule]
    ] ++
    suggestFillHole diag                   -- Lowest priority


suggestRemoveRedundantImport :: ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestRemoveRedundantImport ParsedModule{pm_parsed_source = L _  HsModule{hsmodImports}} contents Diagnostic{_range=_range,..}
--     The qualified import of ‘many’ from module ‘Control.Applicative’ is redundant
    | Just [_, bindings] <- matchRegexUnifySpaces _message "The( qualified)? import of ‘([^’]*)’ from module [^ ]* is redundant"
    , Just (L _ impDecl) <- find (\(L l _) -> srcSpanToRange l == Just _range ) hsmodImports
    , Just c <- contents
    , ranges <- map (rangesForBinding impDecl . T.unpack) (T.splitOn ", " bindings)
    , ranges' <- extendAllToIncludeCommaIfPossible (indexedByPosition $ T.unpack c) (concat ranges)
    , not (null ranges')
    = [( "Remove " <> bindings <> " from import" , [ TextEdit r "" | r <- ranges' ] )]

-- File.hs:16:1: warning:
--     The import of `Data.List' is redundant
--       except perhaps to import instances from `Data.List'
--     To import instances alone, use: import Data.List()
    | _message =~ ("The( qualified)? import of [^ ]* is redundant" :: String)
        = [("Remove import", [TextEdit (extendToWholeLineIfPossible contents _range) ""])]
    | otherwise = []

caRemoveRedundantImports :: Maybe ParsedModule -> Maybe T.Text -> [Diagnostic] -> [Diagnostic] -> Uri -> [CAResult]
caRemoveRedundantImports m contents digs ctxDigs uri
  | Just pm <- m,
    r <- join $ map (\d -> repeat d `zip` suggestRemoveRedundantImport pm contents d) digs,
    allEdits <- [ e | (_, (_, edits)) <- r, e <- edits],
    caRemoveAll <- removeAll allEdits,
    ctxEdits <- [ x | x@(d, _) <- r, d `elem` ctxDigs],
    not $ null ctxEdits,
    caRemoveCtx <- map (\(d, (title, tedit)) -> removeSingle title tedit d) ctxEdits
      = caRemoveCtx ++ [caRemoveAll]
  | otherwise = []
  where
    removeSingle title tedit diagnostic = CACodeAction CodeAction{..} where
        _changes = Just $ Map.singleton uri $ List tedit
        _title = title
        _kind = Just CodeActionQuickFix
        _diagnostics = Just $ List [diagnostic]
        _documentChanges = Nothing
        _edit = Just WorkspaceEdit{..}
        _command = Nothing
    removeAll tedit = CACodeAction CodeAction {..} where
        _changes = Just $ Map.singleton uri $ List tedit
        _title = "Remove all redundant imports"
        _kind = Just CodeActionQuickFix
        _diagnostics = Nothing
        _documentChanges = Nothing
        _edit = Just WorkspaceEdit{..}
        _command = Nothing

suggestDeleteUnusedBinding :: ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestDeleteUnusedBinding
  ParsedModule{pm_parsed_source = L _ HsModule{hsmodDecls}}
  contents
  Diagnostic{_range=_range,..}
-- Foo.hs:4:1: warning: [-Wunused-binds] Defined but not used: ‘f’
    | Just [name] <- matchRegexUnifySpaces _message ".*Defined but not used: ‘([^ ]+)’"
    , Just indexedContent <- indexedByPosition . T.unpack <$> contents
      = let edits = flip TextEdit "" <$> relatedRanges indexedContent (T.unpack name)
        in ([("Delete ‘" <> name <> "’", edits) | not (null edits)])
    | otherwise = []
    where
      relatedRanges indexedContent name =
        concatMap (findRelatedSpans indexedContent name) hsmodDecls
      toRange = realSrcSpanToRange
      extendForSpaces = extendToIncludePreviousNewlineIfPossible

      findRelatedSpans :: PositionIndexedString -> String -> Located (HsDecl GhcPs) -> [Range]
      findRelatedSpans
        indexedContent
        name
        (L (RealSrcSpan l) (ValD _ (extractNameAndMatchesFromFunBind -> Just (lname, matches)))) =
        case lname of
          (L nLoc _name) | isTheBinding nLoc ->
            let findSig (L (RealSrcSpan l) (SigD _ sig)) = findRelatedSigSpan indexedContent name l sig
                findSig _ = []
            in
              [extendForSpaces indexedContent $ toRange l]
              ++ concatMap findSig hsmodDecls
          _ -> concatMap (findRelatedSpanForMatch indexedContent name) matches
      findRelatedSpans _ _ _ = []

      extractNameAndMatchesFromFunBind
        :: HsBind GhcPs
        -> Maybe (Located (IdP GhcPs), [LMatch GhcPs (LHsExpr GhcPs)])
      extractNameAndMatchesFromFunBind
        FunBind
          { fun_id=lname
          , fun_matches=MG {mg_alts=L _ matches}
          } = Just (lname, matches)
      extractNameAndMatchesFromFunBind _ = Nothing

      findRelatedSigSpan :: PositionIndexedString -> String -> RealSrcSpan -> Sig GhcPs -> [Range]
      findRelatedSigSpan indexedContent name l sig =
        let maybeSpan = findRelatedSigSpan1 name sig
        in case maybeSpan of
          Just (_span, True) -> pure $ extendForSpaces indexedContent $ toRange l -- a :: Int
          Just (RealSrcSpan span, False) -> pure $ toRange span -- a, b :: Int, a is unused
          _ -> []

      -- Second of the tuple means there is only one match
      findRelatedSigSpan1 :: String -> Sig GhcPs -> Maybe (SrcSpan, Bool)
      findRelatedSigSpan1 name (TypeSig _ lnames _) =
        let maybeIdx = findIndex (\(L _ id) -> isSameName id name) lnames
        in case maybeIdx of
            Nothing -> Nothing
            Just _ | length lnames == 1 -> Just (getLoc $ head lnames, True)
            Just idx ->
              let targetLname = getLoc $ lnames !! idx
                  startLoc = srcSpanStart targetLname
                  endLoc = srcSpanEnd targetLname
                  startLoc' = if idx == 0
                              then startLoc
                              else srcSpanEnd . getLoc $ lnames !! (idx - 1)
                  endLoc' = if idx == 0 && idx < length lnames - 1
                            then srcSpanStart . getLoc $ lnames !! (idx + 1)
                            else endLoc
              in Just (mkSrcSpan startLoc' endLoc', False)
      findRelatedSigSpan1 _ _ = Nothing

      -- for where clause
      findRelatedSpanForMatch
        :: PositionIndexedString
        -> String
        -> LMatch GhcPs (LHsExpr GhcPs)
        -> [Range]
      findRelatedSpanForMatch
        indexedContent
        name
        (L _ Match{m_grhss=GRHSs{grhssLocalBinds}}) = do
        case grhssLocalBinds of
          (L _ (HsValBinds _ (ValBinds _ bag lsigs))) ->
            if isEmptyBag bag
            then []
            else concatMap (findRelatedSpanForHsBind indexedContent name lsigs) bag
          _ -> []
      findRelatedSpanForMatch _ _ _ = []

      findRelatedSpanForHsBind
        :: PositionIndexedString
        -> String
        -> [LSig GhcPs]
        -> LHsBind GhcPs
        -> [Range]
      findRelatedSpanForHsBind
        indexedContent
        name
        lsigs
        (L (RealSrcSpan l) (extractNameAndMatchesFromFunBind -> Just (lname, matches))) =
        if isTheBinding (getLoc lname)
        then
          let findSig (L (RealSrcSpan l) sig) = findRelatedSigSpan indexedContent name l sig
              findSig _ = []
          in [extendForSpaces indexedContent $ toRange l] ++ concatMap findSig lsigs
        else concatMap (findRelatedSpanForMatch indexedContent name) matches
      findRelatedSpanForHsBind _ _ _ _ = []

      isTheBinding :: SrcSpan -> Bool
      isTheBinding span = srcSpanToRange span == Just _range

      isSameName :: IdP GhcPs -> String -> Bool
      isSameName x name = showSDocUnsafe (ppr x) == name

data ExportsAs = ExportName | ExportPattern | ExportAll
  deriving (Eq)

suggestExportUnusedTopBinding :: Maybe T.Text -> ParsedModule -> Diagnostic -> [(T.Text, [TextEdit])]
suggestExportUnusedTopBinding srcOpt ParsedModule{pm_parsed_source = L _ HsModule{..}} Diagnostic{..}
-- Foo.hs:4:1: warning: [-Wunused-top-binds] Defined but not used: ‘f’
-- Foo.hs:5:1: warning: [-Wunused-top-binds] Defined but not used: type constructor or class ‘F’
-- Foo.hs:6:1: warning: [-Wunused-top-binds] Defined but not used: data constructor ‘Bar’
  | Just source <- srcOpt
  , Just [name] <- matchRegexUnifySpaces _message ".*Defined but not used: ‘([^ ]+)’"
                   <|> matchRegexUnifySpaces _message ".*Defined but not used: type constructor or class ‘([^ ]+)’"
                   <|> matchRegexUnifySpaces _message ".*Defined but not used: data constructor ‘([^ ]+)’"
  , Just (exportType, _) <- find (matchWithDiagnostic _range . snd)
                            . mapMaybe
                                (\(L l b) -> if maybe False isTopLevel $ srcSpanToRange l
                                                then exportsAs b else Nothing)
                            $ hsmodDecls
  , Just pos <- fmap _end . getLocatedRange =<< hsmodExports
  , Just needComma <- needsComma source <$> hsmodExports
  , let exportName = (if needComma then "," else "") <> printExport exportType name
        insertPos = pos {_character = pred $ _character pos}
  = [("Export ‘" <> name <> "’", [TextEdit (Range insertPos insertPos) exportName])]
  | otherwise = []
  where
    -- we get the last export and the closing bracket and check for comma in that range
    needsComma :: T.Text -> Located [LIE GhcPs] -> Bool
    needsComma _ (L _ []) = False
    needsComma source (L (RealSrcSpan l) exports) =
      let closeParan = _end $ realSrcSpanToRange l
          lastExport = fmap _end . getLocatedRange $ last exports
      in case lastExport of
        Just lastExport -> not $ T.isInfixOf "," $ textInRange (Range lastExport closeParan) source
        _ -> False
    needsComma _ _ = False

    opLetter :: String
    opLetter = ":!#$%&*+./<=>?@\\^|-~"

    parenthesizeIfNeeds :: Bool -> T.Text -> T.Text
    parenthesizeIfNeeds needsTypeKeyword x
      | T.head x `elem` opLetter = (if needsTypeKeyword then "type " else "") <> "(" <> x <>")"
      | otherwise = x

    getLocatedRange :: Located a -> Maybe Range
    getLocatedRange = srcSpanToRange . getLoc

    matchWithDiagnostic :: Range -> Located (IdP GhcPs) -> Bool
    matchWithDiagnostic Range{_start=l,_end=r} x =
      let loc = fmap _start . getLocatedRange $ x
       in loc >= Just l && loc <= Just r

    printExport :: ExportsAs -> T.Text -> T.Text
    printExport ExportName x = parenthesizeIfNeeds False x
    printExport ExportPattern x = "pattern " <> x
    printExport ExportAll x = parenthesizeIfNeeds True x <> "(..)"

    isTopLevel :: Range -> Bool
    isTopLevel l = (_character . _start) l == 0

    exportsAs :: HsDecl p -> Maybe (ExportsAs, Located (IdP p))
    exportsAs (ValD _ FunBind {fun_id})          = Just (ExportName, fun_id)
    exportsAs (ValD _ (PatSynBind _ PSB {psb_id})) = Just (ExportPattern, psb_id)
    exportsAs (TyClD _ SynDecl{tcdLName})      = Just (ExportName, tcdLName)
    exportsAs (TyClD _ DataDecl{tcdLName})     = Just (ExportAll, tcdLName)
    exportsAs (TyClD _ ClassDecl{tcdLName})    = Just (ExportAll, tcdLName)
    exportsAs (TyClD _ FamDecl{tcdFam})        = Just (ExportAll, fdLName tcdFam)
    exportsAs _                                = Nothing

suggestAddTypeAnnotationToSatisfyContraints :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestAddTypeAnnotationToSatisfyContraints sourceOpt Diagnostic{_range=_range,..}
-- File.hs:52:41: warning:
--     * Defaulting the following constraint to type ‘Integer’
--        Num p0 arising from the literal ‘1’
--     * In the expression: 1
--       In an equation for ‘f’: f = 1
-- File.hs:52:41: warning:
--     * Defaulting the following constraints to type ‘[Char]’
--        (Show a0)
--          arising from a use of ‘traceShow’
--          at A.hs:228:7-25
--        (IsString a0)
--          arising from the literal ‘"debug"’
--          at A.hs:228:17-23
--     * In the expression: traceShow "debug" a
--       In an equation for ‘f’: f a = traceShow "debug" a
-- File.hs:52:41: warning:
--     * Defaulting the following constraints to type ‘[Char]’
--         (Show a0)
--          arising from a use of ‘traceShow’
--          at A.hs:255:28-43
--        (IsString a0)
--          arising from the literal ‘"test"’
--          at /Users/serhiip/workspace/ghcide/src/Development/IDE/Plugin/CodeAction.hs:255:38-43
--     * In the fourth argument of ‘seq’, namely ‘(traceShow "test")’
--       In the expression: seq "test" seq "test" (traceShow "test")
--       In an equation for ‘f’:
--          f = seq "test" seq "test" (traceShow "test")
    | Just [ty, lit] <- matchRegexUnifySpaces _message (pat False False True)
                        <|> matchRegexUnifySpaces _message (pat False False False)
            = codeEdit ty lit (makeAnnotatedLit ty lit)
    | Just source <- sourceOpt
    , Just [ty, lit] <- matchRegexUnifySpaces _message (pat True True False)
            = let lit' = makeAnnotatedLit ty lit;
                  tir = textInRange _range source
              in codeEdit ty lit (T.replace lit lit' tir)
    | otherwise = []
    where
      makeAnnotatedLit ty lit = "(" <> lit <> " :: " <> ty <> ")"
      pat multiple at inThe = T.concat [ ".*Defaulting the following constraint"
                                       , if multiple then "s" else ""
                                       , " to type ‘([^ ]+)’ "
                                       , ".*arising from the literal ‘(.+)’"
                                       , if inThe then ".+In the.+argument" else ""
                                       , if at then ".+at" else ""
                                       , ".+In the expression"
                                       ]
      codeEdit ty lit replacement =
        let title = "Add type annotation ‘" <> ty <> "’ to ‘" <> lit <> "’"
            edits = [TextEdit _range replacement]
        in  [( title, edits )]


suggestReplaceIdentifier :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestReplaceIdentifier contents Diagnostic{_range=_range,..}
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

suggestNewDefinition :: IdeOptions -> ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestNewDefinition ideOptions parsedModule contents Diagnostic{_message, _range}
--     * Variable not in scope:
--         suggestAcion :: Maybe T.Text -> Range -> Range
    | Just [name, typ] <- matchRegexUnifySpaces message "Variable not in scope: ([^ ]+) :: ([^*•]+)"
    = newDefinitionAction ideOptions parsedModule _range name typ
    | Just [name, typ] <- matchRegexUnifySpaces message "Found hole: _([^ ]+) :: ([^*•]+) Or perhaps"
    , [(label, newDefinitionEdits)] <- newDefinitionAction ideOptions parsedModule _range name typ
    = [(label, mkRenameEdit contents _range name : newDefinitionEdits)]
    | otherwise = []
    where
      message = unifySpaces _message

newDefinitionAction :: IdeOptions -> ParsedModule -> Range -> T.Text -> T.Text -> [(T.Text, [TextEdit])]
newDefinitionAction IdeOptions{..} parsedModule Range{_start} name typ
    | Range _ lastLineP : _ <-
      [ realSrcSpanToRange sp
      | (L l@(RealSrcSpan sp) _) <- hsmodDecls
      , _start `isInsideSrcSpan` l]
    , nextLineP <- Position{ _line = _line lastLineP + 1, _character = 0}
    = [ ("Define " <> sig
        , [TextEdit (Range nextLineP nextLineP) (T.unlines ["", sig, name <> " = error \"not implemented\""])]
        )]
    | otherwise = []
  where
    colon = if optNewColonConvention then " : " else " :: "
    sig = name <> colon <> T.dropWhileEnd isSpace typ
    ParsedModule{pm_parsed_source = L _ HsModule{hsmodDecls}} = parsedModule


suggestFillTypeWildcard :: Diagnostic -> [(T.Text, [TextEdit])]
suggestFillTypeWildcard Diagnostic{_range=_range,..}
-- Foo.hs:3:8: error:
--     * Found type wildcard `_' standing for `p -> p1 -> p'

    | "Found type wildcard" `T.isInfixOf` _message
    , " standing for " `T.isInfixOf` _message
    , typeSignature <- extractWildCardTypeSignature _message
        =  [("Use type signature: ‘" <> typeSignature <> "’", [TextEdit _range typeSignature])]
    | otherwise = []

suggestModuleTypo :: Diagnostic -> [(T.Text, [TextEdit])]
suggestModuleTypo Diagnostic{_range=_range,..}
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
suggestFillHole Diagnostic{_range=_range,..}
    | Just holeName <- extractHoleName _message
    , (holeFits, refFits) <- processHoleSuggestions (T.lines _message)
    = map (proposeHoleFit holeName False) holeFits
    ++ map (proposeHoleFit holeName True) refFits
    | otherwise = []
    where
      extractHoleName = fmap head . flip matchRegexUnifySpaces "Found hole: ([^ ]*)"
      proposeHoleFit holeName parenthise name =
          ( "replace " <> holeName <> " with " <> name
          , [TextEdit _range $ if parenthise then parens name else name])
      parens x = "(" <> x <> ")"

processHoleSuggestions :: [T.Text] -> ([T.Text], [T.Text])
processHoleSuggestions mm = (holeSuggestions, refSuggestions)
{-
    • Found hole: _ :: LSP.Handlers

      Valid hole fits include def
      Valid refinement hole fits include
        fromMaybe (_ :: LSP.Handlers) (_ :: Maybe LSP.Handlers)
        fromJust (_ :: Maybe LSP.Handlers)
        haskell-lsp-types-0.22.0.0:Language.Haskell.LSP.Types.Window.$sel:_value:ProgressParams (_ :: ProgressParams
                                                                                                        LSP.Handlers)
        T.foldl (_ :: LSP.Handlers -> Char -> LSP.Handlers)
                (_ :: LSP.Handlers)
                (_ :: T.Text)
        T.foldl' (_ :: LSP.Handlers -> Char -> LSP.Handlers)
                 (_ :: LSP.Handlers)
                 (_ :: T.Text)
-}
  where
    t = id @T.Text
    holeSuggestions = do
      -- get the text indented under Valid hole fits
      validHolesSection <-
        getIndentedGroupsBy (=~ t " *Valid (hole fits|substitutions) include") mm
      -- the Valid hole fits line can contain a hole fit
      holeFitLine <-
        mapHead
            (mrAfter . (=~ t " *Valid (hole fits|substitutions) include"))
            validHolesSection
      let holeFit = T.strip $ T.takeWhile (/= ':') holeFitLine
      guard (not $ T.null holeFit)
      return holeFit
    refSuggestions = do -- @[]
      -- get the text indented under Valid refinement hole fits
      refinementSection <-
        getIndentedGroupsBy (=~ t " *Valid refinement hole fits include") mm
      -- get the text for each hole fit
      holeFitLines <- getIndentedGroups (tail refinementSection)
      let holeFit = T.strip $ T.unwords holeFitLines
      guard $ not $ holeFit =~ t "Some refinement hole fits suppressed"
      return holeFit

    mapHead f (a:aa) = f a : aa
    mapHead _ [] = []

-- > getIndentedGroups [" H1", "  l1", "  l2", " H2", "  l3"] = [[" H1,", "  l1", "  l2"], [" H2", "  l3"]]
getIndentedGroups :: [T.Text] -> [[T.Text]]
getIndentedGroups [] = []
getIndentedGroups ll@(l:_) = getIndentedGroupsBy ((== indentation l) . indentation) ll
-- |
-- > getIndentedGroupsBy (" H" `isPrefixOf`) [" H1", "  l1", "  l2", " H2", "  l3"] = [[" H1", "  l1", "  l2"], [" H2", "  l3"]]
getIndentedGroupsBy :: (T.Text -> Bool) -> [T.Text] -> [[T.Text]]
getIndentedGroupsBy pred inp = case dropWhile (not.pred) inp of
    (l:ll) -> case span (\l' -> indentation l < indentation l') ll of
        (indented, rest) -> (l:indented) : getIndentedGroupsBy pred rest
    _ -> []

indentation :: T.Text -> Int
indentation = T.length . T.takeWhile isSpace

suggestExtendImport :: ExportsMap -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestExtendImport exportsMap contents Diagnostic{_range=_range,..}
    | Just [binding, mod, srcspan] <-
      matchRegexUnifySpaces _message
      "Perhaps you want to add ‘([^’]*)’ to the import list in the import of ‘([^’]*)’ *\\((.*)\\).$"
    , Just c <- contents
    = suggestions c binding mod srcspan
    | Just (binding, mod_srcspan) <-
      matchRegExMultipleImports _message
    , Just c <- contents
    = mod_srcspan >>= (\(x, y) -> suggestions c binding x y)
    | otherwise = []
    where
        suggestions c binding mod srcspan
          |  range <- case [ x | (x,"") <- readSrcSpan (T.unpack srcspan)] of
                [s] -> let x = realSrcSpanToRange s
                   in x{_end = (_end x){_character = succ (_character (_end x))}}
                _ -> error "bug in srcspan parser",
            importLine <- textInRange range c,
            Just ident <- lookupExportMap binding mod,
            Just result <- addBindingToImportList ident importLine
            = [("Add " <> renderIdentInfo ident <> " to the import list of " <> mod, [TextEdit range result])]
          | otherwise = []
        lookupExportMap binding mod
          | Just match <- Map.lookup binding (getExportsMap exportsMap)
          , [(ident, _)] <- filter (\(_,m) -> mod == m) (Set.toList match)
           = Just ident
          | otherwise = Nothing

suggestFixConstructorImport :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestFixConstructorImport _ Diagnostic{_range=_range,..}
    -- ‘Success’ is a data constructor of ‘Result’
    -- To import it use
    -- import Data.Aeson.Types( Result( Success ) )
    -- or
    -- import Data.Aeson.Types( Result(..) ) (lsp-ui)
  | Just [constructor, typ] <-
    matchRegexUnifySpaces _message
    "‘([^’]*)’ is a data constructor of ‘([^’]*)’ To import it use"
  = let fixedImport = typ <> "(" <> constructor <> ")"
    in [("Fix import of " <> fixedImport, [TextEdit _range fixedImport])]
  | otherwise = []

suggestSignature :: Bool -> Diagnostic -> [(T.Text, [TextEdit])]
suggestSignature isQuickFix Diagnostic{_range=_range@Range{..},..}
    | _message =~
      ("(Top-level binding|Polymorphic local binding|Pattern synonym) with no type signature" :: T.Text) = let
      signature      = removeInitialForAll
                     $ T.takeWhile (\x -> x/='*' && x/='•')
                     $ T.strip $ unifySpaces $ last $ T.splitOn "type signature: " $ filterNewlines _message
      startOfLine    = Position (_line _start) startCharacter
      beforeLine     = Range startOfLine startOfLine
      title          = if isQuickFix then "add signature: " <> signature else signature
      action         = TextEdit beforeLine $ signature <> "\n" <> T.replicate startCharacter " "
      in [(title, [action])]
    where removeInitialForAll :: T.Text -> T.Text
          removeInitialForAll (T.breakOnEnd " :: " -> (nm, ty))
              | "forall" `T.isPrefixOf` ty = nm <> T.drop 2 (snd (T.breakOn "." ty))
              | otherwise                  = nm <> ty
          startCharacter
            | "Polymorphic local binding" `T.isPrefixOf` _message
            = _character _start
            | otherwise
            = 0

suggestSignature _ _ = []

-- | Suggests a constraint for a declaration for which a constraint is missing.
suggestConstraint :: ParsedModule -> Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestConstraint parsedModule mContents diag@Diagnostic {..}
  | Just contents <- mContents
  , Just missingConstraint <- findMissingConstraint _message
  = let codeAction = if _message =~ ("the type signature for:" :: String)
                        then suggestFunctionConstraint parsedModule
                        else suggestInstanceConstraint contents
     in codeAction diag missingConstraint
  | otherwise = []
    where
      findMissingConstraint :: T.Text -> Maybe T.Text
      findMissingConstraint t =
        let regex = "(No instance for|Could not deduce) \\((.+)\\) arising from a use of"
         in matchRegexUnifySpaces t regex <&> last

normalizeConstraints :: T.Text -> T.Text -> T.Text
normalizeConstraints existingConstraints constraint =
  let constraintsInit = if "(" `T.isPrefixOf` existingConstraints
                           then T.dropEnd 1 existingConstraints
                           else "(" <> existingConstraints
   in constraintsInit <> ", " <> constraint <> ")"

-- | Suggests a constraint for an instance declaration for which a constraint is missing.
suggestInstanceConstraint :: T.Text -> Diagnostic -> T.Text -> [(T.Text, [TextEdit])]
suggestInstanceConstraint contents Diagnostic {..} missingConstraint
-- Suggests a constraint for an instance declaration with no existing constraints.
-- • No instance for (Eq a) arising from a use of ‘==’
--   Possible fix: add (Eq a) to the context of the instance declaration
-- • In the expression: x == y
--   In an equation for ‘==’: (Wrap x) == (Wrap y) = x == y
--   In the instance declaration for ‘Eq (Wrap a)’
  | Just [instanceDeclaration] <- matchRegexUnifySpaces _message "In the instance declaration for ‘([^`]*)’"
  = let instanceLine = contents
          & T.splitOn ("instance " <> instanceDeclaration)
          & head & T.lines & length
        startOfConstraint = Position instanceLine (length ("instance " :: String))
        range = Range startOfConstraint startOfConstraint
        newConstraint = missingConstraint <> " => "
     in [(actionTitle missingConstraint, [TextEdit range newConstraint])]

-- Suggests a constraint for an instance declaration with one or more existing constraints.
-- • Could not deduce (Eq b) arising from a use of ‘==’
--   from the context: Eq a
--     bound by the instance declaration at /path/to/Main.hs:7:10-32
--   Possible fix: add (Eq b) to the context of the instance declaration
-- • In the second argument of ‘(&&)’, namely ‘x' == y'’
--   In the expression: x == y && x' == y'
--   In an equation for ‘==’:
--       (Pair x x') == (Pair y y') = x == y && x' == y'
  | Just [instanceLineStr, constraintFirstCharStr]
    <- matchRegexUnifySpaces _message "bound by the instance declaration at .+:([0-9]+):([0-9]+)"
  = let existingConstraints = findExistingConstraints _message
        newConstraints = normalizeConstraints existingConstraints missingConstraint
        instanceLine = readPositionNumber instanceLineStr
        constraintFirstChar = readPositionNumber constraintFirstCharStr
        startOfConstraint = Position instanceLine constraintFirstChar
        endOfConstraint = Position instanceLine $
          constraintFirstChar + T.length existingConstraints
        range = Range startOfConstraint endOfConstraint
     in [(actionTitle missingConstraint, [TextEdit range newConstraints])]
  | otherwise = []
    where
      findExistingConstraints :: T.Text -> T.Text
      findExistingConstraints t =
        T.replace "from the context: " "" . T.strip $ T.lines t !! 1

      readPositionNumber :: T.Text -> Int
      readPositionNumber = T.unpack >>> read >>> pred

      actionTitle :: T.Text -> T.Text
      actionTitle constraint = "Add `" <> constraint
        <> "` to the context of the instance declaration"

findTypeSignatureName :: T.Text -> Maybe T.Text
findTypeSignatureName t = matchRegexUnifySpaces t "([^ ]+) :: " <&> head

findTypeSignatureLine :: T.Text -> T.Text -> Int
findTypeSignatureLine contents typeSignatureName =
  T.splitOn (typeSignatureName <> " :: ") contents & head & T.lines & length

-- | Suggests a constraint for a type signature with any number of existing constraints.
suggestFunctionConstraint :: ParsedModule -> Diagnostic -> T.Text -> [(T.Text, [TextEdit])]
suggestFunctionConstraint ParsedModule{pm_parsed_source = L _ HsModule{hsmodDecls}} Diagnostic{..} missingConstraint
-- • No instance for (Eq a) arising from a use of ‘==’
--   Possible fix:
--     add (Eq a) to the context of
--       the type signature for:
--         eq :: forall a. a -> a -> Bool
-- • In the expression: x == y
--   In an equation for ‘eq’: eq x y = x == y

-- • Could not deduce (Eq b) arising from a use of ‘==’
--   from the context: Eq a
--     bound by the type signature for:
--                eq :: forall a b. Eq a => Pair a b -> Pair a b -> Bool
--     at Main.hs:5:1-42
--   Possible fix:
--     add (Eq b) to the context of
--       the type signature for:
--         eq :: forall a b. Eq a => Pair a b -> Pair a b -> Bool
-- • In the second argument of ‘(&&)’, namely ‘y == y'’
--   In the expression: x == x' && y == y'
--   In an equation for ‘eq’:
--       eq (Pair x y) (Pair x' y') = x == x' && y == y'
  | Just typeSignatureName <- findTypeSignatureName _message
  = let mExistingConstraints = findExistingConstraints _message
        newConstraint = buildNewConstraints missingConstraint mExistingConstraints
     in case findRangeOfContextForFunctionNamed typeSignatureName of
       Just range -> [(actionTitle missingConstraint typeSignatureName, [TextEdit range newConstraint])]
       Nothing -> []
  | otherwise = []
    where
      findRangeOfContextForFunctionNamed :: T.Text -> Maybe Range
      findRangeOfContextForFunctionNamed typeSignatureName = do
          locatedType <- listToMaybe
              [ locatedType
              | L _ (SigD _ (TypeSig _ identifiers (HsWC _ (HsIB _ locatedType)))) <- hsmodDecls
              , any (`isSameName` T.unpack typeSignatureName) $ fmap unLoc identifiers
              ]
          srcSpanToRange $ case splitLHsQualTy locatedType of
            (L contextSrcSpan _ , _) ->
              if isGoodSrcSpan contextSrcSpan
                then contextSrcSpan -- The type signature has explicit context
                else -- No explicit context, return SrcSpan at the start of type sig where we can write context
                     let start = srcSpanStart $ getLoc locatedType in mkSrcSpan start start

      isSameName :: IdP GhcPs -> String -> Bool
      isSameName x name = showSDocUnsafe (ppr x) == name

      findExistingConstraints :: T.Text -> Maybe T.Text
      findExistingConstraints message =
        if message =~ ("from the context:" :: String)
           then fmap (T.strip . head) $ matchRegexUnifySpaces message "\\. ([^=]+)"
           else Nothing

      buildNewConstraints :: T.Text -> Maybe T.Text -> T.Text
      buildNewConstraints constraint mExistingConstraints =
        case mExistingConstraints of
          Just existingConstraints -> normalizeConstraints existingConstraints constraint
          Nothing -> constraint <> " => "

      actionTitle :: T.Text -> T.Text -> T.Text
      actionTitle constraint typeSignatureName = "Add `" <> constraint
        <> "` to the context of the type signature for `" <> typeSignatureName <> "`"

-- | Suggests the removal of a redundant constraint for a type signature.
removeRedundantConstraints :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
removeRedundantConstraints mContents Diagnostic{..}
-- • Redundant constraint: Eq a
-- • In the type signature for:
--      foo :: forall a. Eq a => a -> a
-- • Redundant constraints: (Monoid a, Show a)
-- • In the type signature for:
--      foo :: forall a. (Num a, Monoid a, Eq a, Show a) => a -> Bool
  | Just contents <- mContents
  -- Account for both "Redundant constraint" and "Redundant constraints".
  , True <- "Redundant constraint" `T.isInfixOf` _message
  , Just typeSignatureName <- findTypeSignatureName _message
  , Just redundantConstraintList <- findRedundantConstraints _message
  , Just constraints <- findConstraints contents typeSignatureName
  = let constraintList = parseConstraints constraints
        newConstraints = buildNewConstraints constraintList redundantConstraintList
        typeSignatureLine = findTypeSignatureLine contents typeSignatureName
        typeSignatureFirstChar = T.length $ typeSignatureName <> " :: "
        startOfConstraint = Position typeSignatureLine typeSignatureFirstChar
        endOfConstraint = Position typeSignatureLine $
          typeSignatureFirstChar + T.length (constraints <> " => ")
        range = Range startOfConstraint endOfConstraint
     in [(actionTitle redundantConstraintList typeSignatureName, [TextEdit range newConstraints])]
  | otherwise = []
    where
      parseConstraints :: T.Text -> [T.Text]
      parseConstraints t = t
        & (T.strip >>> stripConstraintsParens >>> T.splitOn ",")
        <&> T.strip

      stripConstraintsParens :: T.Text -> T.Text
      stripConstraintsParens constraints =
        if "(" `T.isPrefixOf` constraints
           then constraints & T.drop 1 & T.dropEnd 1 & T.strip
           else constraints

      findRedundantConstraints :: T.Text -> Maybe [T.Text]
      findRedundantConstraints t = t
        & T.lines
        & head
        & T.strip
        & (`matchRegexUnifySpaces` "Redundant constraints?: (.+)")
        <&> (head >>> parseConstraints)

      -- If the type signature is not formatted as expected (arbitrary number of spaces,
      -- line feeds...), just fail.
      findConstraints :: T.Text -> T.Text -> Maybe T.Text
      findConstraints contents typeSignatureName = do
        constraints <- contents
          & T.splitOn (typeSignatureName <> " :: ")
          & (`atMay` 1)
          >>= (T.splitOn " => " >>> (`atMay` 0))
        guard $ not $ "\n" `T.isInfixOf` constraints || T.strip constraints /= constraints
        return constraints

      formatConstraints :: [T.Text] -> T.Text
      formatConstraints [] = ""
      formatConstraints [constraint] = constraint
      formatConstraints constraintList = constraintList
        & T.intercalate ", "
        & \cs -> "(" <> cs <> ")"

      formatConstraintsWithArrow :: [T.Text] -> T.Text
      formatConstraintsWithArrow [] = ""
      formatConstraintsWithArrow cs = cs & formatConstraints & (<> " => ")

      buildNewConstraints :: [T.Text] -> [T.Text] -> T.Text
      buildNewConstraints constraintList redundantConstraintList =
        formatConstraintsWithArrow $ constraintList \\ redundantConstraintList

      actionTitle :: [T.Text] -> T.Text -> T.Text
      actionTitle constraintList typeSignatureName =
        "Remove redundant constraint" <> (if length constraintList == 1 then "" else "s") <> " `"
        <> formatConstraints constraintList
        <> "` from the context of the type signature for `" <> typeSignatureName <> "`"

-------------------------------------------------------------------------------------------------

suggestNewImport :: ExportsMap -> ParsedModule -> Diagnostic -> [(T.Text, [TextEdit])]
suggestNewImport packageExportsMap ParsedModule {pm_parsed_source = L _ HsModule {..}} Diagnostic{_message}
  | msg <- unifySpaces _message
  , Just thingMissing <- extractNotInScopeName msg
  , qual <- extractQualifiedModuleName msg
  , Just insertLine <- case hsmodImports of
        [] -> case srcSpanStart $ getLoc (head hsmodDecls) of
          RealSrcLoc s -> Just $ srcLocLine s - 1
          _ -> Nothing
        _ -> case srcSpanEnd $ getLoc (last hsmodImports) of
          RealSrcLoc s -> Just $ srcLocLine s
          _ -> Nothing
  , insertPos <- Position insertLine 0
  , extendImportSuggestions <- matchRegexUnifySpaces msg
    "Perhaps you want to add ‘[^’]*’ to the import list in the import of ‘([^’]*)’"
  = [(imp, [TextEdit (Range insertPos insertPos) (imp <> "\n")])
    | imp <- sort $ constructNewImportSuggestions packageExportsMap (qual, thingMissing) extendImportSuggestions
    ]
suggestNewImport _ _ _ = []

constructNewImportSuggestions
  :: ExportsMap -> (Maybe T.Text, NotInScope) -> Maybe [T.Text] -> [T.Text]
constructNewImportSuggestions exportsMap (qual, thingMissing) notTheseModules = nubOrd
  [ suggestion
  | Just name <- [T.stripPrefix (maybe "" (<> ".") qual) $ notInScope thingMissing]
  , (identInfo, m) <- maybe [] Set.toList $ Map.lookup name (getExportsMap exportsMap)
  , canUseIdent thingMissing identInfo
  , m `notElem` fromMaybe [] notTheseModules
  , suggestion <- renderNewImport identInfo m
  ]
 where
  renderNewImport identInfo m
    | Just q <- qual
    , asQ <- if q == m then "" else " as " <> q
    = ["import qualified " <> m <> asQ]
    | otherwise
    = ["import " <> m <> " (" <> renderIdentInfo identInfo <> ")"
      ,"import " <> m ]

canUseIdent :: NotInScope -> IdentInfo -> Bool
canUseIdent NotInScopeDataConstructor{} = isDatacon
canUseIdent _                           = const True

data NotInScope
    = NotInScopeDataConstructor T.Text
    | NotInScopeTypeConstructorOrClass T.Text
    | NotInScopeThing T.Text
    deriving Show

notInScope :: NotInScope -> T.Text
notInScope (NotInScopeDataConstructor t) = t
notInScope (NotInScopeTypeConstructorOrClass t) = t
notInScope (NotInScopeThing t) = t

extractNotInScopeName :: T.Text -> Maybe NotInScope
extractNotInScopeName x
  | Just [name] <- matchRegexUnifySpaces x "Data constructor not in scope: ([^ ]+)"
  = Just $ NotInScopeDataConstructor name
  | Just [name] <- matchRegexUnifySpaces x "Not in scope: data constructor [^‘]*‘([^’]*)’"
  = Just $ NotInScopeDataConstructor name
  | Just [name] <- matchRegexUnifySpaces x "ot in scope: type constructor or class [^‘]*‘([^’]*)’"
  = Just $ NotInScopeTypeConstructorOrClass name
  | Just [name] <- matchRegexUnifySpaces x "ot in scope: \\(([^‘ ]+)\\)"
  = Just $ NotInScopeThing name
  | Just [name] <- matchRegexUnifySpaces x "ot in scope: ([^‘ ]+)"
  = Just $ NotInScopeThing name
  | Just [name] <- matchRegexUnifySpaces x "ot in scope:[^‘]*‘([^’]*)’"
  = Just $ NotInScopeThing name
  | otherwise
  = Nothing

extractQualifiedModuleName :: T.Text -> Maybe T.Text
extractQualifiedModuleName x
  | Just [m] <- matchRegexUnifySpaces x "module named [^‘]*‘([^’]*)’"
  = Just m
  | otherwise 
  = Nothing 

-------------------------------------------------------------------------------------------------


mkRenameEdit :: Maybe T.Text -> Range -> T.Text -> TextEdit
mkRenameEdit contents range name =
    if maybeIsInfixFunction == Just True
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

-- | Returns the ranges for a binding in an import declaration
rangesForBinding :: ImportDecl GhcPs -> String -> [Range]
rangesForBinding ImportDecl{ideclHiding = Just (False, L _ lies)} b =
    concatMap (mapMaybe srcSpanToRange . rangesForBinding' b') lies
  where
    b' = wrapOperatorInParens (unqualify b)

    wrapOperatorInParens x = if isAlpha (head x) then x else "(" <> x <> ")"

    unqualify x = snd $ breakOnEnd "." x

rangesForBinding _ _ = []

rangesForBinding' :: String -> LIE GhcPs -> [SrcSpan]
rangesForBinding' b (L l x@IEVar{}) | showSDocUnsafe (ppr x) == b = [l]
rangesForBinding' b (L l x@IEThingAbs{}) | showSDocUnsafe (ppr x) == b = [l]
rangesForBinding' b (L l (IEThingAll _ x)) | showSDocUnsafe (ppr x) == b = [l]
rangesForBinding' b (L l (IEThingWith _ thing _  inners labels))
    | showSDocUnsafe (ppr thing) == b = [l]
    | otherwise =
        [ l' | L l' x <- inners, showSDocUnsafe (ppr x) == b] ++
        [ l' | L l' x <- labels, showSDocUnsafe (ppr x) == b]
rangesForBinding' _ _ = []

-- | Extends an import list with a new binding.
--   Assumes an import statement of the form:
--       import (qualified) A (..) ..
--   Places the new binding first, preserving whitespace.
--   Copes with multi-line import lists
addBindingToImportList :: IdentInfo -> T.Text -> Maybe T.Text
addBindingToImportList IdentInfo {parent = _parent, ..} importLine =
  case T.breakOn "(" importLine of
    (pre, T.uncons -> Just (_, rest)) ->
      case _parent of
        -- the binding is not a constructor, add it to the head of import list
        Nothing -> Just $ T.concat [pre, "(", rendered, addCommaIfNeeds rest]
        Just parent -> case T.breakOn parent rest of
          -- the binding is a constructor, and current import list contains its parent
          -- `rest'` could be 1. `,...)`
          --               or 2. `(),...)`
          --               or 3. `(ConsA),...)`
          --               or 4. `)`
          (leading, T.stripPrefix parent -> Just rest') -> case T.uncons (T.stripStart rest') of
            -- case 1: no children and parentheses, e.g. `import A(Foo,...)` --> `import A(Foo(Cons), ...)`
            Just (',', rest'') -> Just $ T.concat [pre, "(", leading, parent, "(", rendered, ")", addCommaIfNeeds rest'']
            -- case 2: no children but parentheses, e.g. `import A(Foo(),...)` --> `import A(Foo(Cons), ...)`
            Just ('(', T.uncons -> Just (')', rest'')) -> Just $ T.concat [pre, "(", leading, parent, "(", rendered, ")", rest'']
            -- case 3: children with parentheses, e.g. `import A(Foo(ConsA),...)` --> `import A(Foo(Cons, ConsA), ...)`
            Just ('(', T.breakOn ")" -> (children, rest''))
              | not (T.null children),
                -- ignore A(Foo({-...-}), ...)
                not $ "{-" `T.isPrefixOf` T.stripStart children
              -> Just $ T.concat [pre, "(", leading, parent, "(", rendered, ", ", children, rest'']
            -- case 4: no trailing, e.g. `import A(..., Foo)` --> `import A(..., Foo(Cons))`
            Just (')', _) -> Just $ T.concat [pre, "(", leading, parent, "(", rendered, ")", rest']
            _ -> Nothing
          -- current import list does not contain the parent, e.g. `import A(...)` --> `import A(Foo(Cons), ...)`
          _ -> Just $ T.concat [pre, "(", parent, "(", rendered, ")", addCommaIfNeeds rest]
    _ -> Nothing
  where
    addCommaIfNeeds r = case T.uncons (T.stripStart r) of
      Just (')', _) -> r
      _ -> ", " <> r

-- | 'matchRegex' combined with 'unifySpaces'
matchRegexUnifySpaces :: T.Text -> T.Text -> Maybe [T.Text]
matchRegexUnifySpaces message = matchRegex (unifySpaces message)

-- | Returns Just (the submatches) for the first capture, or Nothing.
matchRegex :: T.Text -> T.Text -> Maybe [T.Text]
matchRegex message regex = case message =~~ regex of
    Just (_ :: T.Text, _ :: T.Text, _ :: T.Text, bindings) -> Just bindings
    Nothing -> Nothing

setHandlersCodeLens :: PartialHandlers c
setHandlersCodeLens = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.codeLensHandler =
        withResponse RspCodeLens codeLens,
    LSP.executeCommandHandler =
        withResponseAndRequest
            RspExecuteCommand
            ReqApplyWorkspaceEdit
            commandHandler
    }

filterNewlines :: T.Text -> T.Text
filterNewlines = T.concat  . T.lines

unifySpaces :: T.Text -> T.Text
unifySpaces    = T.unwords . T.words

-- functions to help parse multiple import suggestions

-- | Returns the first match if found
regexSingleMatch :: T.Text -> T.Text -> Maybe T.Text
regexSingleMatch msg regex = case matchRegexUnifySpaces msg regex of
    Just (h:_) -> Just h
    _ -> Nothing

-- | Parses tuples like (‘Data.Map’, (app/ModuleB.hs:2:1-18)) and
-- | return (Data.Map, app/ModuleB.hs:2:1-18)
regExPair :: (T.Text, T.Text) -> Maybe (T.Text, T.Text)
regExPair (modname, srcpair) = do
  x <- regexSingleMatch modname "‘([^’]*)’"
  y <- regexSingleMatch srcpair "\\((.*)\\)"
  return (x, y)

-- | Process a list of (module_name, filename:src_span) values
-- | Eg. [(Data.Map, app/ModuleB.hs:2:1-18), (Data.HashMap.Strict, app/ModuleB.hs:3:1-29)]
regExImports :: T.Text -> Maybe [(T.Text, T.Text)]
regExImports msg = result
  where
    parts = T.words msg
    isPrefix = not . T.isPrefixOf "("
    (mod, srcspan) = partition isPrefix  parts
    -- check we have matching pairs like (Data.Map, (app/src.hs:1:2-18))
    result = if length mod == length srcspan then
               regExPair `traverse` zip mod srcspan
             else Nothing

matchRegExMultipleImports :: T.Text -> Maybe (T.Text, [(T.Text, T.Text)])
matchRegExMultipleImports message = do
  let pat = T.pack "Perhaps you want to add ‘([^’]*)’ to one of these import lists: *(‘.*\\))$"
  (binding, imports) <- case matchRegexUnifySpaces message pat of
                            Just [x, xs] -> Just (x, xs)
                            _ -> Nothing
  imps <- regExImports imports
  return (binding, imps)

renderIdentInfo :: IdentInfo -> T.Text
renderIdentInfo IdentInfo {parent, rendered}
  | Just p <- parent = p <> "(" <> rendered <> ")"
  | otherwise        = rendered
