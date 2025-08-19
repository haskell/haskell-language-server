module Development.IDE.Plugin.Plugins.FillHole
  ( suggestFillHole
  ) where

import           Control.Lens
import           Control.Monad                     (guard)
import           Data.Char
import qualified Data.Text                         as T
import           Development.IDE                   (FileDiagnostic,
                                                    fdLspDiagnosticL,
                                                    printOutputable)
import           Development.IDE.GHC.Compat        (defaultSDocContext,
                                                    renderWithContext, SDoc)
import           Development.IDE.GHC.Compat.Error  (TcRnMessageDetailed (..),
                                                    _TcRnMessageWithCtx,
                                                    _TcRnMessageWithInfo,
                                                    hole_occ,
                                                    msgEnvelopeErrorL)
import           Development.IDE.Types.Diagnostics (_SomeStructuredMessage,
                                                    fdStructuredMessageL)
import           GHC.Tc.Errors.Types               (ErrInfo (..))
import           Ide.PluginUtils                   (unescape)
import           Language.LSP.Protocol.Lens        (HasRange (..))
import           Language.LSP.Protocol.Types       (TextEdit (..))
import           Text.Regex.TDFA                   (MatchResult (..), (=~))
import GHC.Utils.Outputable (SDocContext(..))
import Development.IDE.Plugin.Plugins.Diagnostic (diagReportHoleError)

suggestFillHole :: FileDiagnostic -> [(T.Text, TextEdit)]
suggestFillHole diag
    | Just holeName <- extractHoleName diag
    , Just (ErrInfo ctx suppl) <- extractErrInfo diag
    , (holeFits, refFits) <- processHoleSuggestions $ T.lines (printErr suppl) = do
        let isInfixHole = printErr ctx =~ addBackticks holeName :: Bool in
          map (proposeHoleFit holeName False isInfixHole) holeFits
          ++ map (proposeHoleFit holeName True isInfixHole) refFits
    | otherwise = []
    where
      addBackticks text = "`" <> text <> "`"
      addParens text = "(" <> text <> ")"
      proposeHoleFit holeName parenthise isInfixHole name =
        case T.uncons name of
          Nothing -> error "impossible: empty name provided by ghc"
          Just (firstChr, _) ->
            let isInfixOperator = firstChr == '('
                name' = getOperatorNotation isInfixHole isInfixOperator name in
              ( "Replace " <> holeName <> " with " <> name
              , TextEdit
                    (diag ^. fdLspDiagnosticL . range)
                    (if parenthise then addParens name' else name')
              )
      getOperatorNotation True False name                    = addBackticks name
      getOperatorNotation True True name                     = T.drop 1 (T.dropEnd 1 name)
      getOperatorNotation _isInfixHole _isInfixOperator name = name

extractHoleName :: FileDiagnostic -> Maybe T.Text
extractHoleName diag = do
    hole <- diagReportHoleError diag
    Just $ printOutputable (hole_occ hole)

extractErrInfo :: FileDiagnostic -> Maybe ErrInfo
extractErrInfo diag = do
    (_, TcRnMessageDetailed errInfo _) <-
        diag
            ^? fdStructuredMessageL
            . _SomeStructuredMessage
            . msgEnvelopeErrorL
            . _TcRnMessageWithCtx
            . _TcRnMessageWithInfo

    Just errInfo

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
      -- Valid refinement hole fits line can contain a hole fit
      refinementFitLine <-
        mapHead
            (mrAfter . (=~ t " *Valid refinement hole fits include"))
            refinementSection
      let refinementHoleFit = T.strip $ T.takeWhile (/= ':') refinementFitLine
      guard $ not $ refinementHoleFit  =~ t "Some refinement hole fits suppressed"
      guard $ not $ T.null refinementHoleFit
      return refinementHoleFit

    mapHead f (a:aa) = f a : aa
    mapHead _ []     = []

-- |
-- > getIndentedGroupsBy (" H" `isPrefixOf`) [" H1", "  l1", "  l2", " H2", "  l3"] = [[" H1", "  l1", "  l2"], [" H2", "  l3"]]
getIndentedGroupsBy :: (T.Text -> Bool) -> [T.Text] -> [[T.Text]]
getIndentedGroupsBy pred inp = case dropWhile (not.pred) inp of
    (l:ll) -> case span (\l' -> indentation l < indentation l') ll of
        (indented, rest) -> (l:indented) : getIndentedGroupsBy pred rest
    _ -> []

indentation :: T.Text -> Int
indentation = T.length . T.takeWhile isSpace

printErr :: SDoc -> T.Text
printErr =
    unescape
    . T.pack
    . renderWithContext
        ( defaultSDocContext
            { sdocCanUseUnicode = False
            , sdocSuppressUniques = True
            }
        )
