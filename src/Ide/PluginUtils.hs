{-# LANGUAGE OverloadedStrings #-}
module Ide.PluginUtils where

import qualified Data.Text as T
import           Data.Maybe
import           Data.Algorithm.DiffOutput
import           Data.Algorithm.Diff
import qualified Data.HashMap.Strict as H
import           Language.Haskell.LSP.Types.Capabilities
import qualified Language.Haskell.LSP.Types            as J
import           Language.Haskell.LSP.Types

-- ---------------------------------------------------------------------

-- | Extend to the line below and above to replace newline character.
normalize :: Range -> Range
normalize (Range (Position sl _) (Position el _)) =
  Range (Position sl 0) (Position (el + 1) 0)

-- ---------------------------------------------------------------------

data WithDeletions = IncludeDeletions | SkipDeletions
  deriving Eq

-- | Generate a 'WorkspaceEdit' value from a pair of source Text
diffText :: ClientCapabilities -> (Uri,T.Text) -> T.Text -> WithDeletions -> WorkspaceEdit
diffText clientCaps old new withDeletions =
  let
    supports = clientSupportsDocumentChanges clientCaps
  in diffText' supports old new withDeletions

makeDiffTextEdit :: T.Text -> T.Text -> List TextEdit
makeDiffTextEdit f1 f2 = diffTextEdit f1 f2 IncludeDeletions

makeDiffTextEditAdditive :: T.Text -> T.Text -> List TextEdit
makeDiffTextEditAdditive f1 f2 = diffTextEdit f1 f2 SkipDeletions

diffTextEdit :: T.Text -> T.Text -> WithDeletions -> List TextEdit
diffTextEdit fText f2Text withDeletions = J.List r
  where
    r = map diffOperationToTextEdit diffOps
    d = getGroupedDiff (lines $ T.unpack fText) (lines $ T.unpack f2Text)

    diffOps = filter (\x -> (withDeletions == IncludeDeletions) || not (isDeletion x))
                     (diffToLineRanges d)

    isDeletion (Deletion _ _) = True
    isDeletion _ = False


    diffOperationToTextEdit :: DiffOperation LineRange -> J.TextEdit
    diffOperationToTextEdit (Change fm to) = J.TextEdit range nt
      where
        range = calcRange fm
        nt = T.pack $ init $ unlines $ lrContents to

    {-
      In order to replace everything including newline characters,
      the end range should extend below the last line. From the specification:
      "If you want to specify a range that contains a line including
      the line ending character(s) then use an end position denoting
      the start of the next line"
    -}
    diffOperationToTextEdit (Deletion (LineRange (sl, el) _) _) = J.TextEdit range ""
      where
        range = J.Range (J.Position (sl - 1) 0)
                        (J.Position el 0)

    diffOperationToTextEdit (Addition fm l) = J.TextEdit range nt
    -- fm has a range wrt to the changed file, which starts in the current file at l
    -- So the range has to be shifted to start at l
      where
        range = J.Range (J.Position (l' - 1) 0)
                        (J.Position (l' - 1) 0)
        l' = max l sl -- Needed to add at the end of the file
        sl = fst $ lrNumbers fm
        nt = T.pack $ unlines $ lrContents fm


    calcRange fm = J.Range s e
      where
        sl = fst $ lrNumbers fm
        sc = 0
        s = J.Position (sl - 1) sc -- Note: zero-based lines
        el = snd $ lrNumbers fm
        ec = length $ last $ lrContents fm
        e = J.Position (el - 1) ec  -- Note: zero-based lines


-- | A pure version of 'diffText' for testing
diffText' :: Bool -> (Uri,T.Text) -> T.Text -> WithDeletions -> WorkspaceEdit
diffText' supports (f,fText) f2Text withDeletions  =
  if supports
    then WorkspaceEdit Nothing (Just docChanges)
    else WorkspaceEdit (Just h) Nothing
  where
    diff = diffTextEdit fText f2Text withDeletions
    h = H.singleton f diff
    docChanges = J.List [docEdit]
    docEdit = J.TextDocumentEdit (J.VersionedTextDocumentIdentifier f (Just 0)) diff

-- ---------------------------------------------------------------------

clientSupportsDocumentChanges :: ClientCapabilities -> Bool
clientSupportsDocumentChanges caps =
  let ClientCapabilities mwCaps _ _ _ = caps
      supports = do
        wCaps <- mwCaps
        WorkspaceEditClientCapabilities mDc <- _workspaceEdit wCaps
        mDc
  in
    fromMaybe False supports