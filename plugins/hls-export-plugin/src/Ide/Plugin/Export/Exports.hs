module Ide.Plugin.Export.Exports
  ( isExplicit
  , isExported
  , addExport
  , addConstructorExport
  ) where

import           Data.Maybe                   (isJust)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error    (srcSpanToRange)
import           Ide.Plugin.Export.ExactPrint
import           Ide.Plugin.Export.Utils
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

replaceExportList :: ParsedSource -> (LExportList -> Maybe LExportList) -> Maybe [TextEdit]
replaceExportList ps f = do
  exports <- hsmodExports (unLoc ps)
  newList <- f (toDeltaExportList exports)
  r <- srcSpanToRange (getLoc exports)
  Just [TextEdit r (printExportList newList)]

addExport :: ParsedSource -> LIE GhcPs -> Maybe [TextEdit]
addExport ps item = replaceExportList ps (Just . appendIE item)

addConstructorExport :: RdrName -> RdrName -> ParsedSource -> Maybe [TextEdit]
addConstructorExport parent ctor ps = replaceExportList ps (addCtorUnderParent parent ctor)
