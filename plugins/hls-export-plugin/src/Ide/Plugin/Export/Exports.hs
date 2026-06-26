module Ide.Plugin.Export.Exports
  ( isExplicit
  , isExported
  , addExport
  , addConstructorExport
  , removeExport
  , removeConstructorExport
  ) where

import           Data.Maybe                         (isJust)
import           Data.Text                          (Text)
import           Data.Text.Utf16.Rope.Mixed         (Rope)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error          (srcSpanToRange)
import           Development.IDE.GHC.ExactPrint.CPP (spanHasCpp)
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Utils
import           Language.Haskell.GHC.ExactPrint    (makeDeltaAst)
import           Language.LSP.Protocol.Types

isExplicit :: ParsedSource -> Bool
isExplicit = isJust . hsmodExports . unLoc

-- | Also matches names appearing only as constructor children of an 'IEThingWith' parent.
isExported :: RdrName -> ParsedSource -> Bool
isExported n ps = case hsmodExports (unLoc ps) of
  Nothing          -> False
  Just (L _ items) -> any (covers . unLoc) items
  where
    nFS = rdrNameFS n
    covers ie = parentNameIs nFS ie || isInIE nFS ie

-- | Extract the export list and pick an edit strategy: splice surgically when
-- the span holds a CPP directive, otherwise reprint the whole transformed list.
withExportList
  :: Maybe Rope
  -> ParsedSource
  -> (LExportList -> Maybe LExportList)          -- ^ reprint transform
  -> (Range -> LExportList -> Maybe [TextEdit])  -- ^ list holds a directive
  -> Maybe [TextEdit]
withExportList msrc ps reprint onCpp = do
  exports <- hsmodExports (unLoc ps)
  full <- srcSpanToRange (getLoc exports)
  if spanHasCpp msrc full
    then onCpp full exports
    else do
      newList <- reprint (makeDeltaAst exports)
      Just [TextEdit full (printExportList newList)]

addExport :: Maybe Rope -> ParsedSource -> LIE GhcPs -> Maybe [TextEdit]
addExport msrc ps item =
  withExportList msrc ps (Just . appendIE item) $ \full _ ->
    Just [insertAfterOpen full (printIE item)]

addConstructorExport :: Maybe Rope -> RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
addConstructorExport msrc parent ctor ps =
  withExportList msrc ps (addCtorUnderParent parent ctor) $ \full exports ->
    (\txt -> [insertAfterOpen full txt]) <$> freshCtorEntry parent ctor (unLoc exports)

-- | Splice @itemTxt@ in right after the opening paren with a trailing comma,
-- @( <itemTxt>, <existing> )@.
insertAfterOpen :: Range -> Text -> TextEdit
insertAfterOpen (Range (Position sl sc) _) itemTxt =
  TextEdit (Range pos pos) (" " <> itemTxt <> ",")
  where
    -- `sc` is the column of `(`, so insert just past it.
    pos = Position sl (sc + 1)

-- | Reprinting would drop the directives the parser stripped, so unexport is
-- declined when the export list holds a directive.
removeExport :: Maybe Rope -> ParsedSource -> RdrName -> Maybe [TextEdit]
removeExport msrc ps name =
  withExportList msrc ps (removeMatchingIE matches) declineUnderCpp
  where
    matches = parentNameIs (rdrNameFS name)

removeConstructorExport :: Maybe Rope -> RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
removeConstructorExport msrc parent ctor ps =
  withExportList msrc ps (removeCtorUnderParent parent ctor) declineUnderCpp

-- | An 'onCpp' handler that declines: the edit has no safe surgical form, so it
-- is offered only when the list reprints cleanly.
declineUnderCpp :: Range -> LExportList -> Maybe [TextEdit]
declineUnderCpp _ _ = Nothing
