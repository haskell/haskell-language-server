{-# LANGUAGE GADTs #-}

module Development.IDE.Plugin.CodeAction.Extract (textInRange, suggestExtractFunction, fromLspList) where

import           Control.Applicative                       ((<|>))
import           Data.Foldable                             (foldl')
import           Data.Generics                             (everywhere, mkT)
import           Data.Maybe
import           Data.Monoid                               (Sum (..))
import qualified Data.Set                                  as Set
import qualified Data.Text                                 as T
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat                as Compat
import           Development.IDE.GHC.Compat.ExactPrint
import           Development.IDE.GHC.ExactPrint
import           Development.IDE.Types.Location
import           Generics.SYB                              (GenericQ,
                                                            everything,
                                                            everythingBut, extQ)
import           Generics.SYB.Aliases                      (mkQ)
import           GHC                                       (AddEpAnn (AddEpAnn),
                                                            Anchor (..),
                                                            AnchorOperation (..),
                                                            DeltaPos (..),
                                                            EpAnn (..),
                                                            NoEpAnns (..),
                                                            SrcSpanAnn' (SrcSpanAnn),
                                                            emptyComments,
                                                            realSrcSpan)
import           GHC.Types.SrcLoc                          (generatedSrcSpan)
import           Ide.PluginUtils                           (makeDiffTextEdit)
import           Language.Haskell.GHC.ExactPrint           (noAnnSrcSpanDP,
                                                            runTransform,
                                                            uniqueSrcSpanT)
import           Language.Haskell.GHC.ExactPrint.Transform (d1)
import           Language.LSP.Types                        (List (..), UInt)
import           Language.LSP.Types.Capabilities

-- | Suggest a code action for extracting the expression indicated by the user's current Range selection.
--
-- The expression is extracted to the top level of the module, with a new function called `newDefinition`, and with
-- a best-guess at the newly-free-scoped variables.
--
-- Before:
--   foo = x + y + z
-- After
--   newDefinition y z = y + z
--   foo = x + newDefinition y z
--
-- Does not work with record wild cards, and subtle bugs relating to shadowed variables, which is documented throughout
-- this module.
suggestExtractFunction :: T.Text -> Annotated ParsedSource -> Range -> [(T.Text, [TextEdit])]
suggestExtractFunction srcText (Annotated parsedSource _) range =
  case tryFindLargestExprInRange minimalRange parsedSource of
    Nothing -> []
    Just (extractSpan, extractExpr) -> execTransform $ do
      argsList <- findFreeVarsPostExtract extractSpan extractExpr parsedSource
      newDef <- generateNewDecl newDefName argsList extractExpr
      let addNewDef = modifySmallestDeclWithM_
            (pure . (extractSpan `isSubspanOf`))
            (\case
              ldecl@(L (SrcSpanAnn (EpAnn anchor _ _) _) _) -> do
                let lNewDef = L (SrcSpanAnn (EpAnn anchor mempty emptyComments) generatedSrcSpan) newDef
                pure [lNewDef, setAtLeastNewlineDp ldecl]
              _ -> error "todo")
          replaceExtractExprWithVarQ = everywhere $ mkT $ \case
            L (SrcSpanAnn epAnn span) _e :: LHsExpr GhcPs
              | extractSpan == span ->
                let go fExpr argExpr = HsApp
                      (EpAnn (Anchor (realSrcSpan generatedSrcSpan) $ MovedAnchor $ SameLine 0) NoEpAnns emptyComments)
                      (noLocA fExpr)
                      (L (noAnnSrcSpanDP generatedSrcSpan $ SameLine 1) $ HsVar noExtField (noLocA argExpr))
                in L (SrcSpanAnn epAnn generatedSrcSpan) $ foldl' go (HsVar noExtField newDefName) argsList
            lexpr -> lexpr
      let source' = replaceExtractExprWithVarQ parsedSource
      source'' <- addNewDef $ makeDeltaAst source'
      let diff = makeDiffTextEdit (T.pack $ exactPrint parsedSource) (T.pack $ exactPrint $ makeDeltaAst source'')
      pure [("Extract function", fromLspList diff)]
  where
    minimalRange = removeWhitespacePadding range srcText
    newDefName = noLocA $ mkRdrUnqual $ mkVarOcc $ T.unpack "newDefinition"
    execTransform = (\(a,_,_) -> a) . runTransform
suggestExtractFunction _ _ _ = [] -- Could not find Annotated ParsedSource

-- | This function represents a simple guess of which variables are going to be out of scope after the expression
-- is extracted.  This strategy will probably work for >90% cases.
--
-- The strategy is that we assume that all variables bound in the extracted expr scope over any occurrence of said
-- variable in the same extracted expr. Assuming this, we can take the variables bound in the surrounding context minus
-- the variables bound in the expression. This gives us variables that were in scope that will no longer be in scope
-- post-extraction. Then we find the utilized variables by finding the intersection of in-scope variable and utilized
-- variables.
--
-- A simple counter-example to this strategy would occur by extracting the rhs of `foo`, "let tmp = ... in bar":

--    foo bar = let tmp = bar in let bar = 1 in bar

-- `bar` is used in the rhs of, but is also bound in the same expression, and so this strategy will detect the variable
-- not being out of scope post-extraction (even though it will be).
--
-- Thankfully this would just cause an unbound variable error post-extraction.
--
-- In order to have a more robust strategy without excessive maintenance burden, we would require output from
-- GHC's renamer phase, preferably in-tree.
--
-- Some known weaknesses:
-- TODO Some subtle name shadowing issues in the extracted expression. Hard to fix without more GHC help.
--      You can see this in the above example, where `bar` is actually a free variable, but the implementation will
--      not realize that the variable will be free post-extraction.
-- TODO Record `{..}` pattern matching syntax; we can find the fields in the matching record to fix this. See
--      hls-explicit-record-fields-plugin.
-- TODO When finding bound variables in the whole declaration, we find bound variables in the _whole_ declaration,
--      not just the surrounding Match. Therefore, as far as this implementation is concerned, variables in other
--      Match's scoped over the Match that contains the extracted expression.
findFreeVarsPostExtract :: (Monad m) => SrcSpan -> LHsExpr GhcPs -> ParsedSource -> TransformT m [RdrName]
findFreeVarsPostExtract extractSpan extractExpr parsedSource = do
      -- Find the variables that are bound in this declaration. Not finding the surrounding decl is a bug
      -- (since an expression) must necessarily occur in a top-level declaration.
      --
      -- TODO log error when we cannot find the containing decl
      boundVarsInWholeDeclMay <- querySmallestDeclWithM (pure . (extractSpan `isSubspanOf`)) (pure . boundVarsQ) parsedSource
      -- Variables bound in the entire declaration that contains the extracted expression
      let boundVarsInWholeDecl = fromMaybe [] boundVarsInWholeDeclMay
      -- Variables bound in the extracted expression
      let boundVarsInExtractedExpr = boundVarsQ extractExpr
      -- Variables used in the extracted expression. Note this does not detect record wildcards for record construction,
      -- such as `foo x y = Rec {..}`, in the case that Rec has a field x and/or y.
      let usedVarsInExtractedExpr = usedVarsQ extractExpr
      -- The bound variables that scope over the extracted expression is, in most cases, all bindings in the
      -- declarations minus the bindings in the extracted expression. This is a simplifying assumption.
      let boundVarsThatScopeOverExtractedExpr =
            Set.fromList boundVarsInWholeDecl `Set.difference` Set.fromList boundVarsInExtractedExpr
      -- The newly freed variables of the extracted expression are, most of the time, those variables scoping over
      -- the extracted expression that also exist in the extracted expression.
      --
      -- Anything else is assumed to be an import, which could be wrong in a case such as:
      --
      --   data Rec = Rec {x :: Int}
      --   foo Rec {..} y = x + y
      --
      -- This implementation erroneously produces:
      --
      --   data Rec = Rec {x :: Int}
      --   newDefinition y = x + y
      --   foo Rec {..} y = newDefinition y
      let postExtractFreeVars =
            boundVarsThatScopeOverExtractedExpr `Set.intersection` Set.fromList usedVarsInExtractedExpr
      pure $ Set.toList postExtractFreeVars

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
  deriving stock (Show, Eq)

-- | This function removes whitespace padding from the Range provided by the user.
--
-- When the user selects their range that they would like to extract, we would like to ignore whitespace in their
-- selection, so that we can find expressions that are contained fully by said range, but without accepting false
-- positives. Therefore, we remove whitespace padding, and then during matching of ranges, we only match expression
-- that have exactly the range selected by the user (whitespace agnostic) by performing a range _equality_ check.
--
-- If we were to check whether we can extract an expression with a subspan check (instead of equality), then we would
-- accept selections that are not valid extractions. For example:
--
--   foo = 1    +    2
--           ^       ^
-- In this case, while the range fully wraps `2`, it is not a valid extract function query, since it also wraps `+ 2`,
-- which is not an expression.
--
-- Therefore, in order to ensure that we do not suggest an extraction in this case, we narrow the selection to:
--
--   foo = 1    +    2
--              ^    ^
-- and look for LHsExprs that match the given Range using _equality_, which ensures that we only accept a range if:
--  1. the range wraps an entire LHsExpr
--  2. the range wraps an LHsExpr exactly (so as to not suggest false positives)
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

-- | Convert a GHC SrcSpan to an UnpaddedRange
--
-- TODO consolidate with Range as found in the rest of HLS. For some reason, the Range provided to code actions has
-- a character value of one less than the same Range used in the rest of HLS. Therefore, the typical Range <-> SrcSpan
-- conversions don't work because of an off-by-one.
srcSpanToUnpaddedRange :: SrcSpan -> Maybe UnpaddedRange
srcSpanToUnpaddedRange (UnhelpfulSpan _)           = Nothing
srcSpanToUnpaddedRange (Compat.RealSrcSpan real _) = Just $ realSrcSpanToUnpaddedRange real
  where
    realSrcSpanToUnpaddedRange :: RealSrcSpan -> UnpaddedRange
    realSrcSpanToUnpaddedRange real =
      UnpaddedRange $ Range (realSrcLocToPositionForUnpaddedRange $ Compat.realSrcSpanStart real)
            (realSrcLocToPositionForUnpaddedRange $ Compat.realSrcSpanEnd   real)
    realSrcLocToPositionForUnpaddedRange :: RealSrcLoc -> Position
    realSrcLocToPositionForUnpaddedRange real =
      Position (fromIntegral $ srcLocLine real - 1) (fromIntegral $ srcLocCol real)


-- | Queries an AST for the topmost LHsExpr that exactly matches the given UnpaddedRange.
tryFindLargestExprInRange :: UnpaddedRange -> GenericQ (Maybe (SrcSpan, LHsExpr GhcPs))
tryFindLargestExprInRange range = everythingBut (<|>) $ mkQ (Nothing, False) firstContainedExprQ
  where
    -- When we find an LHsExpr that is fully contained by this range, we return the LHsExpr and early-exit the
    -- syb traversal (by returning True)
    firstContainedExprQ = \case
      lexpr@(L (SrcSpanAnn _ span) _) :: LHsExpr GhcPs
        | Just spanRange <- srcSpanToUnpaddedRange span,
          range == spanRange
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

-- | Find all variable bindings in an AST.
--
-- NOTE As with the rest of the extract plugin, shadowing is considered out of scope, and we do a best guess.
-- TODO improve shadowing here if we decide to do it ourselves. Be careful, as there is certainly more to do than
-- just the TODOs that have already been added...
boundVarsQ :: GenericQ [RdrName]
boundVarsQ  = everything (<>) $ mkQ []
  (\case
    (VarPat _ var :: Pat GhcPs) -> [unLocA var]
    _                           -> []
  )
  `extQ`
  (\case
    (FunBind {fun_id} :: HsBindLR GhcPs GhcPs) -> [unLocA fun_id]
    PatBind {pat_lhs}                          -> rdrNameQ pat_lhs -- TODO shadowing
    VarBind {var_id}                           -> [var_id] -- TODO shadowing
    PatSynBind {}                              -> [] -- TODO shadowing
    AbsBinds {}                                -> [] -- TODO shadowing
    )
  `extQ`
  (\case
    (HsValBinds _ binds :: HsLocalBindsLR GhcPs GhcPs) -> boundVarsQ binds
    HsIPBinds {}                                       -> [] -- TODO shadowing
    EmptyLocalBinds {}                                 -> []
  )
  where
    rdrNameQ :: GenericQ [RdrName]
    rdrNameQ =everything (<>) $ mkQ [] $ \name -> [name]

-- | Find variable usages bindings in an AST.
usedVarsQ :: GenericQ [RdrName]
usedVarsQ  = everything (<>) $ mkQ [] $
  \case
    (HsVar _ var :: HsExpr GhcPs) -> [unLocA var]
    _                             -> []

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
