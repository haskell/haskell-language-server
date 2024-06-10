module Development.IDE.Plugin.Plugins.SuggestAddMissingFields
  ( suggestAddMissingFields
  ) where

import           Control.Monad                             (guard)
import           Data.Char
import qualified Data.Text                                 as T
import           Development.IDE.GHC.Compat
import           Development.IDE.Plugin.Plugins.Diagnostic
import           Development.IDE.Types.Location
import           Language.LSP.Protocol.Types               (Diagnostic (..),
                                                            TextEdit (TextEdit))
import           Text.Regex.TDFA                           (MatchResult (..),
                                                            (=~))

suggestAddMissingFields :: ParsedSource -> Diagnostic -> [(T.Text, TextEdit)]
suggestAddMissingFields parsedSrc Diagnostic{_range=_range,..}
    | Just constructorName <- extractConstructorName _message
    , missingFields <- processMissingFields (T.lines _message) =
      [proposeAddAllMissingFields constructorName missingFields]
    | otherwise = []
    where
      extractConstructorName = fmap (headOrThrow "impossible") . flip matchRegexUnifySpaces "Fields of ‘([^ ]*)’ not initialised"
      proposeAddAllMissingFields :: T.Text -> [T.Text] -> (T.Text, TextEdit)
      proposeAddAllMissingFields constructorName missingFields =
        ( "add all missing fields: " <> T.intercalate ", " missingFields
        , TextEdit _range $
          constructorName <> " {"
            <> T.intercalate ", " (fieldWithDummyValue <$> missingFields)
            <> "}"
        )
      fieldWithDummyValue field = field <> " = _"  <> field
      headOrThrow msg = \case
        [] -> error msg
        (x:_) -> x


newSrc :: ParsedSource -> Range -> T.Text -> [T.Text] -> ParsedSource
newSrc = undefined


processMissingFields :: [T.Text] -> [T.Text]
processMissingFields missingFieldLines = do
    missingFieldsSection <-
        getIndentedGroupsBy (=~ t " *• Fields of ‘([^ ]*)’ not initialised") missingFieldLines
    missingFieldLine <-
        mapHead
            (mrAfter . (=~ t " *• Fields of ‘([^ ]*)’ not initialised"))
            missingFieldsSection
    let missingField = T.strip $ T.takeWhile (/= ':') missingFieldLine
    guard $ not $ T.null missingField
    pure missingField
    where
        t = id @T.Text
        mapHead f (a:aa) = f a : aa
        mapHead _ []     = []

-- TODO: Extract this copy pasted code from FillHole.hs into something else

-- |
-- > getIndentedGroupsBy (" H" `isPrefixOf`) [" H1", "  l1", "  l2", " H2", "  l3"] = [[" H1", "  l1", "  l2"], [" H2", "  l3"]]
getIndentedGroupsBy :: (T.Text -> Bool) -> [T.Text] -> [[T.Text]]
getIndentedGroupsBy pred inp = case dropWhile (not.pred) inp of
    (l:ll) -> case span (\l' -> indentation l < indentation l') ll of
        (indented, rest) -> (l:indented) : getIndentedGroupsBy pred rest
    _ -> []

indentation :: T.Text -> Int
indentation = T.length . T.takeWhile isSpace
