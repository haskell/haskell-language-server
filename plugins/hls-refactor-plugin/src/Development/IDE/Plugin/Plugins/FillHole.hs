{-# LANGUAGE CPP #-}
module Development.IDE.Plugin.Plugins.FillHole
  ( suggestFillHole
  ) where

import           Control.Lens                              ((^.), (^?))
import           Control.Monad                             (guard)
import           Data.Char
import qualified Data.HashSet                              as Set
import qualified Data.Text                                 as T
import           Development.IDE                           (FileDiagnostic,
                                                            _message,
                                                            fdLspDiagnosticL,
                                                            printOutputable)
import           Development.IDE.GHC.Compat                (ParsedModule,
                                                            hsmodImports,
                                                            ideclAs, ideclName,
                                                            ideclQualified,
                                                            lookupOccEnv,
                                                            moduleNameString,
                                                            pm_parsed_source,
                                                            unLoc)
import           Development.IDE.GHC.Compat.Error          (TcRnMessageDetailed (TcRnMessageDetailed),
                                                            _TcRnMessageWithCtx,
                                                            _TcRnMessageWithInfo,
                                                            hole_occ,
                                                            msgEnvelopeErrorL)
import           Development.IDE.Plugin.Plugins.Diagnostic (diagReportHoleError)
import           Development.IDE.Types.Diagnostics         (_SomeStructuredMessage,
                                                            fdStructuredMessageL)
import           Development.IDE.Types.Exports             (ExportsMap (..),
                                                            mkVarOrDataOcc,
                                                            moduleNameText)
import           GHC.Tc.Errors.Types                       (ErrInfo (ErrInfo))
import           Language.Haskell.Syntax.ImpExp            (ImportDeclQualifiedStyle (..))
import           Language.LSP.Protocol.Lens                (HasRange (..))
import           Language.LSP.Protocol.Types               (TextEdit (TextEdit))
import           Text.Regex.TDFA                           (MatchResult (..),
                                                            (=~))

suggestFillHole :: ExportsMap -> ParsedModule -> FileDiagnostic -> [(T.Text, TextEdit)]
suggestFillHole exportsMap pm diag
    | Just holeName <- extractHoleName diag
#if MIN_VERSION_ghc(9,13,0)
    , Just _errInfo <- extractErrInfo diag
    , let supplText = _message (diag ^. fdLspDiagnosticL)
    , let ctxText = supplText
#else
    , Just (ErrInfo ctx suppl) <- extractErrInfo diag
    , let ctxText = printOutputable ctx
    , let supplText = printOutputable suppl
#endif
    , let (holeFits, refFits) = processHoleSuggestions (T.lines supplText)
    , let isInfixHole = ctxText =~ addBackticks holeName :: Bool =
        map (proposeHoleFit holeName False isInfixHole) holeFits
        ++
        map (proposeHoleFit holeName True isInfixHole) refFits
    | otherwise = []
    where
      qualify = qualifyFit exportsMap pm

      extractHoleName :: FileDiagnostic -> Maybe T.Text
      extractHoleName d = do
          hole <- diagReportHoleError d
          Just $ printOutputable (hole_occ hole)

      extractErrInfo :: FileDiagnostic -> Maybe ErrInfo
      extractErrInfo d = do
          (_, TcRnMessageDetailed errInfo _) <-
              d ^? fdStructuredMessageL
                 . _SomeStructuredMessage
                 . msgEnvelopeErrorL
                 . _TcRnMessageWithCtx
                 . _TcRnMessageWithInfo
          Just errInfo

      addBackticks :: T.Text -> T.Text
      addBackticks text = "`" <> text <> "`"

      addParens :: T.Text -> T.Text
      addParens text = "(" <> text <> ")"

      proposeHoleFit :: T.Text -> Bool -> Bool -> T.Text -> (T.Text, TextEdit)
      proposeHoleFit holeName parenthise isInfixHole name =
        case T.uncons name of
          Nothing -> error "impossible: empty name provided by ghc"
          Just (firstChr, _) ->
            let cleanName = qualify (stripUnique name)
                isInfixOperator = firstChr == '('
                name' = getOperatorNotation isInfixHole isInfixOperator cleanName
                replacement = if parenthise then addParens name' else name'
            in
               ( "Replace " <> holeName <> " with " <> cleanName
               , TextEdit (diag ^. fdLspDiagnosticL . range) replacement
               )

      getOperatorNotation :: Bool -> Bool -> T.Text -> T.Text
      getOperatorNotation True False name                    = addBackticks name
      getOperatorNotation True True name                     = T.drop 1 (T.dropEnd 1 name)
      getOperatorNotation _isInfixHole _isInfixOperator name = name

      stripUnique :: T.Text -> T.Text
      stripUnique t =
        case T.breakOnEnd "_" t of
          (prefix, suffix)
            | T.null prefix -> t
            | T.null suffix -> t
            | not (T.all isAlphaNum suffix) -> t
            | otherwise -> T.dropEnd (T.length suffix + 1) t

-- | Given the exports map, parsed module (for its imports), and a hole fit
-- name like "toException", return the qualified version like "E.toException"
-- if a qualifying import exists, otherwise return the name as it is.
qualifyFit :: ExportsMap -> ParsedModule -> T.Text -> T.Text
qualifyFit exportsMap pm fitName =
    case findQualifier of
        Nothing        -> fitName
        Just qualifier -> qualifier <> "." <> fitName
  where
    -- All modules that export this name
    exportingModules :: [T.Text]
    exportingModules =
        let occ      = mkVarOrDataOcc fitName
            identSet = lookupOccEnv (getExportsMap exportsMap) occ
            idents   = maybe [] Set.toList identSet
        in map moduleNameText idents

    -- All qualified imports from this file: (moduleName, qualifier)
    qualifiedImports :: [(T.Text, T.Text)]
    qualifiedImports =
        let imports = hsmodImports . unLoc . pm_parsed_source $ pm
        in [ (modName decl, qualifier decl)
           | i <- imports
           , let decl = unLoc i
           , isQualified decl
           ]

    isQualified decl = ideclQualified decl `elem` [QualifiedPre, QualifiedPost]

    modName decl =
        T.pack . moduleNameString . unLoc . ideclName $ decl

    qualifier decl =
        case ideclAs decl of
            Just alias -> T.pack . moduleNameString . unLoc $ alias
            Nothing    -> modName decl

    -- Find first qualified import whose module is in the exporting modules list
    findQualifier :: Maybe T.Text
    findQualifier =
        let exportingSet = exportingModules
        in fmap snd
         . safeHead
         . filter (\(modN, _) -> modN `elem` exportingSet)
         $ qualifiedImports

    safeHead []    = Nothing
    safeHead (x:_) = Just x


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

