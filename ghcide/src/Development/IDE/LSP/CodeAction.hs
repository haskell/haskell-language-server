-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0

{-# LANGUAGE DuplicateRecordFields #-}

-- | Go to the definition of a variable.
module Development.IDE.LSP.CodeAction
    ( setHandlersCodeAction
    ) where

import           Language.Haskell.LSP.Types
import Development.IDE.GHC.Compat
import Development.IDE.Core.Rules
import Development.IDE.LSP.Server
import qualified Data.HashMap.Strict as Map
import qualified Data.HashSet as Set
import qualified Language.Haskell.LSP.Core as LSP
import Language.Haskell.LSP.VFS
import Language.Haskell.LSP.Messages
import qualified Data.Rope.UTF16 as Rope
import Data.Char
import qualified Data.Text as T

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


suggestAction :: Maybe T.Text -> Diagnostic -> [(T.Text, [TextEdit])]
suggestAction contents Diagnostic{_range=_range@Range{..},..}

-- File.hs:16:1: warning:
--     The import of `Data.List' is redundant
--       except perhaps to import instances from `Data.List'
--     To import instances alone, use: import Data.List()
    | "The import of " `T.isInfixOf` _message
    , " is redundant" `T.isInfixOf` _message
    , let newlineAfter = maybe False (T.isPrefixOf "\n" . T.dropWhile (\x -> isSpace x && x /= '\n') . snd . textAtPosition _end) contents
    , let extend = newlineAfter && _character _start == 0 -- takes up an entire line, so remove the whole line
        = [("Remove import", [TextEdit (if extend then Range _start (Position (_line _end + 1) 0) else _range) ""])]

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

suggestAction _ _ = []


-- | All the GHC extensions
ghcExtensions :: Set.HashSet T.Text
ghcExtensions = Set.fromList $ map (T.pack . show) ghcEnumerateExtensions


textAtPosition :: Position -> T.Text -> (T.Text, T.Text)
textAtPosition (Position row col) x
    | (preRow, mid:postRow) <- splitAt row $ T.splitOn "\n" x
    , (preCol, postCol) <- T.splitAt col mid
        = (T.intercalate "\n" $ preRow ++ [preCol], T.intercalate "\n" $ postCol : postRow)
    | otherwise = (x, T.empty)


setHandlersCodeAction :: PartialHandlers
setHandlersCodeAction = PartialHandlers $ \WithMessage{..} x -> return x{
    LSP.codeActionHandler = withResponse RspCodeAction codeAction
    }
