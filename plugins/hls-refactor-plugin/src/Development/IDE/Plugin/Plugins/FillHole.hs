module Development.IDE.Plugin.Plugins.FillHole
  ( suggestFillHole
  ) where

import           Control.Monad                             (guard)
import           Data.Char
import qualified Data.Text                                 as T
import           Development.IDE.Plugin.Plugins.Diagnostic
import           Language.LSP.Protocol.Types               (Diagnostic (..),
                                                            TextEdit (TextEdit))
import           Text.Regex.TDFA                           (MatchResult (..),
                                                            (=~))

suggestFillHole :: Diagnostic -> [(T.Text, TextEdit)]
suggestFillHole Diagnostic{_range=_range,..}
    | Just holeName <- extractHoleName _message
    , (holeFits, refFits) <- processHoleSuggestions (T.lines _message) =
      let isInfixHole = _message =~ addBackticks holeName :: Bool in
        map (proposeHoleFit holeName False isInfixHole) holeFits
        ++ map (proposeHoleFit holeName True isInfixHole) refFits
    | otherwise = []
    where
      extractHoleName = fmap (headOrThrow "impossible") . flip matchRegexUnifySpaces "Found hole: ([^ ]*)"
      addBackticks text = "`" <> text <> "`"
      addParens text = "(" <> text <> ")"
      proposeHoleFit holeName parenthise isInfixHole name =
        case T.uncons name of
          Nothing -> error "impossible: empty name provided by ghc"
          Just (firstChr, _) ->
            let isInfixOperator = firstChr == '('
                name' = getOperatorNotation isInfixHole isInfixOperator name in
              ( "Replace " <> holeName <> " with " <> name
              , TextEdit _range (if parenthise then addParens name' else name')
              )
      getOperatorNotation True False name                    = addBackticks name
      getOperatorNotation True True name                     = T.drop 1 (T.dropEnd 1 name)
      getOperatorNotation _isInfixHole _isInfixOperator name = name
      headOrThrow msg = \case
        [] -> error msg
        (x:_) -> x

processHoleSuggestions :: [T.Text] -> ([T.Text], [T.Text])
processHoleSuggestions mm = (holeSuggestions, refSuggestions)
{-
    • Found hole: _ :: LSP.Handlers

      Valid hole fits include def
      Valid refinement hole fits include
        fromMaybe (_ :: LSP.Handlers) (_ :: Maybe LSP.Handlers)
        fromJust (_ :: Maybe LSP.Handlers)
        haskell-lsp-types-0.22.0.0:Language.LSP.Types.Window.$sel:_value:ProgressParams (_ :: ProgressParams
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
      guard $ not $ holeFit =~ t "Some hole fits suppressed"
      guard $ not $ T.null holeFit
      return holeFit
    refSuggestions = do -- @[]
      -- get the text indented under Valid refinement hole fits
      refinementSection <-
        getIndentedGroupsBy (=~ t " *Valid refinement hole fits include") mm
      case refinementSection of
        [] -> error "GHC provided invalid hole fit options"
        (_:refinementSection) -> do
          -- get the text for each hole fit
          holeFitLines <- getIndentedGroups refinementSection
          let holeFit = T.strip $ T.unwords holeFitLines
          guard $ not $ holeFit =~ t "Some refinement hole fits suppressed"
          return holeFit

    mapHead f (a:aa) = f a : aa
    mapHead _ []     = []

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

