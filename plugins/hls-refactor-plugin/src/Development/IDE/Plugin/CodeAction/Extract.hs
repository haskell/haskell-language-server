{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs     #-}

module Development.IDE.Plugin.CodeAction.Extract (textInRange, suggestExtractFunction, fromLspList) where

import           Control.Applicative                        ((<|>))
import           Data.Data                                  (Data)
import           Data.Foldable                              (foldl')
import           Data.Function
import           Data.Generics                              (Typeable,
                                                             everywhere, mkT)
import           Data.Maybe
import           Data.Monoid                                (Sum (..))
import           Data.Set                                   (Set)
import qualified Data.Set                                   as Set
import           Data.String                                (fromString)
import qualified Data.Text                                  as T
import           Debug.Trace
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat.Core            as Compat
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.Compat.Util
import           Development.IDE.GHC.Error                  (positionToRealSrcLoc,
                                                             rangeToSrcSpan)
import           Development.IDE.GHC.ExactPrint
import           Development.IDE.Types.Location
import           Generics.SYB                               (GenericQ,
                                                             everything,
                                                             everythingBut,
                                                             extQ)
import           Generics.SYB.Aliases                       (mkQ)
import           GHC                                        (AddEpAnn (AddEpAnn),
                                                             Anchor (..),
                                                             AnchorOperation (..),
                                                             DeltaPos (..),
                                                             EpAnn (..),
                                                             LocatedN,
                                                             NoEpAnns (..),
                                                             SrcSpanAnn' (SrcSpanAnn),
                                                             emptyComments,
                                                             realSrcSpan)
import           GHC.Types.SrcLoc                           (generatedSrcSpan)
import           Ide.PluginUtils                            (makeDiffTextEdit)
import           Language.Haskell.GHC.ExactPrint            (noAnnSrcSpanDP,
                                                             runTransform,
                                                             runTransformT,
                                                             uniqueSrcSpanT)
import           Language.Haskell.GHC.ExactPrint.ExactPrint (showAst)
import           Language.Haskell.GHC.ExactPrint.Transform  (d1)
import           Language.LSP.Types                         (List (..), UInt)
import           Language.LSP.Types.Capabilities
import Control.Monad.Identity (Identity(..))

-- | Returns True if two SrcSpan occupy the same text region (even if they have different file information)
isSameSrcSpanModuloFile :: SrcSpan -> SrcSpan -> Bool
isSameSrcSpanModuloFile span1 span2 = span1 `isSubspanOf` span2 && span2 `isSubspanOf` span1

-- | A type-safety-helpful newtype that represents that a Range has had its whitespace padding removed from both sides:
--
-- Original range:
--            1 + 1
--      ^               ^
--
-- UnpaddedRange:
--            1 + 1
--            ^   ^
-- See `removeWhitespacePadding` for more info
newtype UnpaddedRange = UnpaddedRange {unUnpaddedRange :: Range}

-- | This function removes whitespace padding from the Range provided by the user.
--
-- When the user selects their range that they would like to extract, we would like to ignore whitespace in their
-- selection, so that we can find expressions that are contained fully by said range. Consider the following selection:
--
--   foo = 1    +    2
--           ^       ^
-- In this case, while the range fully wraps `2`, it is not a valid extract function query, since it also wraps `+ 2`,
-- which is not an expression.
--
-- Therefore, in order to catch this case, we narrow the selection to:
--
--   foo = 1    +    2
--              ^    ^
-- and look for LHsExprs that precisely match the given Range, which ensures that we only accept a range if:
--  1. the range wraps an entire LHsExpr
--  2. the range wraps an LHsExpr exactly
--
-- More explanation of (2)... If a range does exactly match any LHsExpr, then we know it is an invalid range for
-- extract, since there is no whitespace padding in the range.
removeWhitespacePadding :: Range -> T.Text -> UnpaddedRange
removeWhitespacePadding range@(Range (Position sl sc) (Position el ec)) source =
  let selectedTxt = textInRange range source
      -- TODO We do not yet handle tabs gracefully
      isWhiteSpace c = elem c [' ', '\n']
      (paddingLeft, minimalOnLeft) = T.span isWhiteSpace selectedTxt
      (paddingRight, _) = T.span isWhiteSpace $ T.reverse minimalOnLeft
      whitespaceToDelta = \case
        ' '  -> (0 :: Sum UInt, -1 :: Sum UInt)
        '\n' -> (-1, 0)
        _    -> error "impossible"
      paddingToDelta = foldMap whitespaceToDelta . T.unpack
      (getSum -> rowsL, getSum -> colsL) = paddingToDelta paddingLeft
      (getSum -> rowsR, getSum -> colsR) = paddingToDelta paddingRight
     in UnpaddedRange $ Range (Position (sl - rowsL) (sc - colsL)) (Position (el - rowsR) (ec - colsR))

-- | Queries an AST for the topmost LHsExpr that exactly matches the given UnpaddedRange.
tryFindLargestExprInRange :: UnpaddedRange -> GenericQ (Maybe (SrcSpan, LHsExpr GhcPs))
tryFindLargestExprInRange range = everythingBut (<|>) $ mkQ (Nothing, False) firstContainedExprQ
  where
    -- When we find an LHsExpr that is fully contained by this range, we return the LHsExpr and early-exit the
    -- syb traversal (by returning True)
    firstContainedExprQ = \case
      lexpr@(L (SrcSpanAnn _ span@(RealSrcSpan realSrcSpan _)) _) :: LHsExpr GhcPs
        | rangeSrcSpan <- rangeToSrcSpan (fromString $ unpackFS $ srcSpanFile realSrcSpan) (unUnpaddedRange range)
        , isSameSrcSpanModuloFile rangeSrcSpan span
        -> (Just (span, lexpr), True)
      _ -> (Nothing, False)

-- | Ensures that there is at least one newline's difference between this LHsDecl and the previous decl. If the
-- original decl was on the next line after the anchor, with no padding, we simply return with no modifications
setAtLeastNewlineDp :: LHsDecl GhcPs -> LHsDecl GhcPs
setAtLeastNewlineDp (L srcSpanAnn decl) = L (mapAnchor (maybe nextLineAnchor setAtLeastNewline) srcSpanAnn) decl
  where
    setAtLeastNewline (Anchor realSrcSpan ancOp) =
              let ancOp' = case ancOp of
                    UnchangedAnchor                 -> nextLine2Dp
                    MovedAnchor (SameLine _)        -> nextLine2Dp
                    MovedAnchor (DifferentLine 0 _) -> nextLine2Dp
                    ancOp_                          -> ancOp_
                in Anchor realSrcSpan ancOp'

    nextLine2Dp = MovedAnchor (DifferentLine 2 0)
    nextLineAnchor = generatedAnchor nextLine2Dp

rdrNameQ :: GenericQ [RdrName]
rdrNameQ =everything (<>) $ mkQ [] $ \case name -> [name]

boundVarsQ :: forall a. Data a => a -> [RdrName]
boundVarsQ  = everything (<>) $ mkQ []
  (\case
    -- pat@(VarPat _ var :: Pat GhcPs) -> trace (showAst pat) [unLocA var]
    pat@(VarPat _ var :: Pat GhcPs) -> [unLocA var]
    -- e -> trace (showAst e) []
    e                               -> []
  )
  `extQ`
  (\case
    -- (FunBind {fun_id, fun_matches} :: HsBindLR GhcPs GhcPs) -> trace (showAst (fun_matches, boundVarsQ fun_matches)) [unLocA fun_id]
    (FunBind {fun_id, fun_matches} :: HsBindLR GhcPs GhcPs) -> [unLocA fun_id]
    PatBind {pat_lhs, pat_rhs}                              -> rdrNameQ pat_lhs -- TODO shadowing
    VarBind {var_id, var_rhs}                               -> [var_id] -- TODO shadowing
    PatSynBind {}                                           -> [] -- TODO
    AbsBinds {}                                             -> [] -- TODO
    )
  `extQ`
  (\case
    (HsValBinds _ binds :: HsLocalBindsLR GhcPs GhcPs) -> boundVarsQ binds
    HsIPBinds {}                                       -> [] -- TODO
    EmptyLocalBinds {}                                 -> []
  )
  -- Can't get this to match?
  -- `extQ`
  -- (\case
  --   (Match {m_pats} :: Match GhcPs (LHsExpr GhcPs)) -> trace (showAst m_pats) rdrNameQ m_pats
  -- )

usedVarsQ :: forall a. Data a => a -> [RdrName]
usedVarsQ  = everything (<>) $ mkQ [] $
  \case
    (HsVar _ var :: HsExpr GhcPs) -> [unLocA var]
    _                             -> []

suggestExtractFunction :: T.Text -> Annotated ParsedSource -> Range -> [(T.Text, [TextEdit])]
suggestExtractFunction srcText (Annotated parsedSource _) range =
  let minimalRange = removeWhitespacePadding range srcText
      edits = case tryFindLargestExprInRange minimalRange parsedSource of
          Nothing -> []
          Just (extractSpan, extractExpr) -> (\(a,_,_) -> a) $ runTransform $ do
            let newDefName = noLocA $ mkRdrUnqual $ mkVarOcc $ T.unpack "newDefinition"
            -- We do not actually modify anything, just find the bound variables
            (_, boundVars) <- modifySmallestDeclWithM (Identity . (extractSpan `isSubspanOf`))
                 (\ decl -> pure ([decl], boundVarsQ decl))
                  -- traceM $ "decl" <> showAst decl
                 parsedSource
            -- TODO
            let allVarsInExtracted = usedVarsQ extractExpr
            let boundVarsInExtracted = boundVarsQ extractExpr
            let args = (Set.fromList (fromMaybe [] boundVars) `Set.difference` Set.fromList boundVarsInExtracted)
                  `Set.intersection` Set.fromList allVarsInExtracted
            let argsList = Set.toList args
            -- traceM $ "allVarsInExtracted" <> showAst allVarsInExtracted
            -- traceM $ "boundVars" <> showAst boundVars
            -- traceM $ "args" <> showAst args
            -- traceM $ "extracted" <> showAst extractExpr
            newDef <- generateNewDecl newDefName argsList extractExpr
            let addNewDef = modifySmallestDeclWithM_
                  (Identity . (extractSpan `isSubspanOf`))
                  (\case
                    ldecl@(L (SrcSpanAnn (EpAnn anchor _ _) _) _) -> do
                      let lNewDef = L (SrcSpanAnn (EpAnn anchor mempty emptyComments) generatedSrcSpan) newDef
                      pure [lNewDef, setAtLeastNewlineDp ldecl]
                    _ -> error "todo")
                removeExpr = everywhere $ mkT $ \case
                  L (SrcSpanAnn epAnn span) _e :: LHsExpr GhcPs
                    | extractSpan == span ->
                      let go fExpr argExpr = HsApp
                            (EpAnn (Anchor (realSrcSpan generatedSrcSpan) $ MovedAnchor $ SameLine 0) NoEpAnns emptyComments)
                            (noLocA fExpr)
                            (L (noAnnSrcSpanDP generatedSrcSpan $ SameLine 1) $ HsVar noExtField (noLocA argExpr))
                      in L (SrcSpanAnn epAnn generatedSrcSpan) $ foldl' go (HsVar noExtField newDefName) argsList
                  lexpr -> lexpr
            let source' = removeExpr parsedSource
            source'' <- addNewDef $ makeDeltaAst source'
            let diff = makeDiffTextEdit (T.pack $ exactPrint parsedSource) (T.pack $ exactPrint $ makeDeltaAst source'')
            pure [("Extract function", fromLspList diff)]
    in edits
suggestExtractFunction _ _ _ = [] -- Could not find Annotated ParsedSource

fromLspList :: List a -> [a]
fromLspList (List a) = a

-- | Find the text delineated by a given Range from a source Text.
textInRange :: Range -> T.Text -> T.Text
textInRange (Range (Position (fromIntegral -> startRow) (fromIntegral -> startCol)) (Position (fromIntegral -> endRow) (fromIntegral -> endCol))) text =
    case compare startRow endRow of
      LT ->
        let (linesInRangeBeforeEndLine, endLineAndFurtherLines) = splitAt (endRow - startRow) linesBeginningWithStartLine
            (textInRangeInFirstLine, linesBetween) = case linesInRangeBeforeEndLine of
              [] -> ("", [])
              firstLine:linesInBetween -> (T.drop (startCol - 1) firstLine, linesInBetween)
            maybeTextInRangeInEndLine = T.take endCol <$> listToMaybe endLineAndFurtherLines
        in T.intercalate "\n" (textInRangeInFirstLine : linesBetween ++ maybeToList maybeTextInRangeInEndLine)
      EQ ->
        let line = fromMaybe "" (listToMaybe linesBeginningWithStartLine)
        in T.take (endCol - startCol) (T.drop (startCol - 1) line)
      GT -> ""
    where
      linesBeginningWithStartLine = drop startRow (T.splitOn "\n" text)

-- | From a function name and list of arguments, generate a new function with the given LHsExpr as the rhs.
generateNewDecl :: Monad m =>  LIdP GhcPs -> [RdrName] -> LHsExpr GhcPs -> TransformT m (HsDecl GhcPs)
generateNewDecl name args expr = do
  sp1 <- uniqueSrcSpanT
  let rhs = L generatedSrcSpan $ GRHS (epAnn generatedSrcSpan $ GrhsAnn (Just d1) (AddEpAnn AnnEqual d1)) [] expr
      grhss = GRHSs emptyComments [rhs] (EmptyLocalBinds noExtField)
      match = Match mempty (FunRhs name Prefix NoSrcStrict) ((\ arg -> L (noAnnSrcSpanDP generatedSrcSpan $ SameLine 1) (VarPat noExtField (noLocA arg))) <$> args) grhss
      lmatch = L (noAnnSrcSpanDP generatedSrcSpan $ SameLine 0) match
      mg = MG noExtField (L (noAnnSrcSpanDP sp1 $ SameLine 0) [lmatch]) FromSource
      funbind = FunBind noExtField name mg []
  pure $ ValD noExtField funbind
