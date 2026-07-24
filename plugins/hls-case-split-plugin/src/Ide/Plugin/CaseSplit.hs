{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OrPatterns        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

module Ide.Plugin.CaseSplit
  ( caseSplitPluginCodeActionTitle
  , descriptor
  , Log
  ) where

import           Control.Applicative                   (ZipList (ZipList, getZipList),
                                                        (<|>))
import           Control.Arrow                         ((&&&))
import           Control.Lens                          (Fold, prism', (^.),
                                                        (^?))
import           Control.Monad                         (join, mzero)
import           Control.Monad.IO.Class                (MonadIO (liftIO))
import           Control.Monad.State.Strict            (MonadState (get, put),
                                                        State, evalState)
import           Control.Monad.Trans                   (lift)
import           Control.Monad.Trans.Except            (throwE)
import           Control.Monad.Trans.Maybe             (MaybeT, runMaybeT)
import           Data.Bifunctor                        (bimap)
import           Data.Data                             (Data)
import           Data.Function                         (on, (&))
import           Data.Generics.Schemes                 (everywhereM)
import           Data.List                             (minimumBy)
import           Data.List.Extra                       (chunksOf, dropEnd,
                                                        takeEnd)
import           Data.List.NonEmpty                    (NonEmpty ((:|)))
import qualified Data.List.NonEmpty                    as NE
import           Data.List.NonEmpty.Extra              ((|:))
import           Data.Maybe                            (isJust, listToMaybe,
                                                        mapMaybe)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Development.IDE                       (FileDiagnostic (fdStructuredMessage),
                                                        GetParsedModule (GetParsedModule),
                                                        GhcSessionDeps (GhcSessionDeps),
                                                        HscEnvEq (hscEnv),
                                                        IdeState (shakeExtras),
                                                        Pretty (pretty),
                                                        Recorder, WithPriority,
                                                        runAction,
                                                        srcSpanToRange)
import           Development.IDE.Core.FileStore        (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils      (activeDiagnosticsInRange,
                                                        runActionE, useE)
import qualified Development.IDE.Core.Shake            as Shake
import           Development.IDE.GHC.Compat            (ConLike (RealDataCon),
                                                        GhcMessage (GhcDsMessage),
                                                        HoleKind (HoleVar),
                                                        HsMatchContext (CaseAlt),
                                                        HscEnv (hsc_dflags), Id,
                                                        NamedThing (getName),
                                                        getLoc)
import           Development.IDE.GHC.Compat.Core       (AnnListItem,
                                                        EpAnnHsCase (EpAnnHsCase),
                                                        GrhsAnn (..),
                                                        HasSrcSpan,
                                                        HsLamVariant (LamCase),
                                                        HsMatchContext (LamAlt),
                                                        LocatedAn,
                                                        lann_trailing,
                                                        srcSpanStartCol,
                                                        srcSpanStartLine)
import qualified Development.IDE.GHC.Compat.Core       as Ext
import           Development.IDE.GHC.Compat.Error      (DsMessage (DsNonExhaustivePatterns),
                                                        msgEnvelopeErrorL)
import           Development.IDE.GHC.Compat.ExactPrint (d0, d1, exactPrint,
                                                        getEntryDP,
                                                        noAnnSrcSpanDP1,
                                                        setEntryDP)
import           Development.IDE.Types.Diagnostics     (FileDiagnostic (fdLspDiagnostic),
                                                        _SomeStructuredMessage)
import           GHC                                   (AnnList (AnnList),
                                                        AnnListBrackets (ListBraces),
                                                        DynFlags (extensions),
                                                        EpAnn (EpAnn),
                                                        EpToken (EpTok),
                                                        HasLoc (getHasLoc),
                                                        LMatch,
                                                        ParsedModule (pm_parsed_source),
                                                        realSrcSpan)
import           GHC.Driver.DynFlags                   (OnOff (On))
import           GHC.Hs                                (DeltaPos (deltaColumn),
                                                        EpAnnLam (EpAnnLam),
                                                        GhcPs,
                                                        HsRecFields (HsRecFields),
                                                        XCase, XLam, deltaPos,
                                                        getDeltaLine,
                                                        unnamedHoleRdrName)
import           GHC.HsToCore.Pmc.Solver.Types         (Nabla (nabla_tm_st),
                                                        PmAltCon (..),
                                                        PmAltConApp (..),
                                                        TmState (ts_facts),
                                                        VarInfo (vi_pos))
import           GHC.Parser.Annotation                 (EpUniToken (EpUniTok),
                                                        IsUnicodeSyntax (NormalSyntax, UnicodeSyntax),
                                                        TrailingAnn (AddSemiAnn),
                                                        addTrailingAnnToA,
                                                        emptyComments,
                                                        noSrcSpanA)
import           GHC.Types.Name.Reader                 (nameRdrName)
import           GHC.Types.SrcLoc                      (GenLocated (L),
                                                        SrcSpan (RealSrcSpan),
                                                        combineSrcSpans)
import           GHC.Types.Unique.SDFM                 (lookupUSDFM)
import           Ide.Logger                            ((<+>))
import           Ide.Plugin.Error                      (PluginError (PluginInternalError, PluginStaleResolve),
                                                        getNormalizedFilePathE)
import           Ide.PluginUtils                       (WithDeletions (IncludeDeletions),
                                                        diffText)
import           Ide.Types                             (PluginDescriptor (pluginHandlers, pluginPriority),
                                                        PluginId,
                                                        PluginMethodHandler,
                                                        defaultPluginDescriptor,
                                                        mkPluginHandler,
                                                        pluginGetClientCapabilities)
import           Language.Haskell.Syntax               (HsConDetails (PrefixCon, RecCon),
                                                        HsLocalBindsLR (EmptyLocalBinds),
                                                        LHsExpr,
                                                        MatchGroup (MG, mg_alts),
                                                        NoExtField (NoExtField),
                                                        Pat (..))
import           Language.Haskell.Syntax.Expr          (GRHS (GRHS),
                                                        GRHSs (GRHSs),
                                                        HsExpr (HsCase, HsHole, HsLam),
                                                        Match (..))
import qualified Language.LSP.Protocol.Lens            as L
import           Language.LSP.Protocol.Message         (Method (Method_TextDocumentCodeAction))
import qualified Language.LSP.Protocol.Message         as LSP
import           Language.LSP.Protocol.Types           (CodeAction (..),
                                                        CodeActionKind (CodeActionKind_QuickFix),
                                                        CodeActionParams (CodeActionParams, _range, _textDocument),
                                                        Range, isSubrangeOf,
                                                        type (|?) (InL, InR))
import qualified Language.LSP.Protocol.Types           as Diag (Diagnostic (_range))
import           Type.Reflection                       (eqTypeRep,
                                                        type (:~~:) (HRefl),
                                                        typeOf, typeRep)

data Log where
  LogShake :: Shake.Log -> Log
  LogWAEResponseError :: LSP.TResponseError LSP.Method_WorkspaceApplyEdit -> Log
  LogResolve :: Pretty a => a -> Log

instance Pretty Log where
  pretty = \case
    LogShake logMsg -> "LogShake " <+> pretty logMsg
    LogWAEResponseError rspErr -> "RequestWorkspaceApplyEdit Failed with " <+> pretty rspErr
    LogResolve msg -> "LogResolve " <+> pretty msg

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor _ plId = (defaultPluginDescriptor plId "Provides the split case code action")
  { pluginHandlers = mkPluginHandler LSP.SMethod_TextDocumentCodeAction suggestCaseSplitProvider
  , pluginPriority = 1
  }

suggestCaseSplitProvider :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
suggestCaseSplitProvider state _ CodeActionParams{ _textDocument, _range = cursor }
  = do
  nfp <- getNormalizedFilePathE $ _textDocument ^. L.uri

  verTxtDocId <- liftIO $ runAction "CaseSplit.GetVersionedTextDoc" state $ getVersionedTextDoc _textDocument

  (hsc_dflags . hscEnv -> dynFlags) <- runActionE "CaseSplit.GhcSessionDeps" state $ useE GhcSessionDeps nfp

  let arrowSyntax = if On Ext.UnicodeSyntax `elem` extensions dynFlags
                      then UnicodeSyntax
                      else NormalSyntax

  pm <- runActionE "CaseSplit.GetParsedModule" state $ useE GetParsedModule nfp

  fileDiags <- activeDiagnosticsInRange (shakeExtras state) nfp cursor

  fileDiagAndDsMsg <-
    if | (Nothing; Just []) <- fileDiags
          -> throwE $ PluginInternalError "Error in retrieving diagnostics at the cursor."
       | Just fileDiags@(_:_) <- fileDiags
          -> fileDiags
             -- pair each file diag with its ds messages, if any
             & map (id &&& getMaybeDsMsg)
             -- discard those with 'Nothing' ds messages and unwrap the surviving 'Just's
             & (mapMaybe sequence :: [(a, Maybe b)] -> [(a, b)])
             -- wrap back in the monad
             & pure

  (diag, pmAltsConApps) <-
    if | null fileDiagAndDsMsg
          -> throwE $ PluginInternalError "Error in retrieving diagnostics at the cursor."
       | otherwise
          -> fileDiagAndDsMsg
             -- obtain the innermost diag-and-message
             & minimumBy (ordSubrange `on` Diag._range . fdLspDiagnostic . fst)
             -- extract the 'Diagnostic' and the pattern-match constructors
             & bimap fdLspDiagnostic dsMsgToPmAlts
             & pure

  if | Nothing <- pmAltsConApps
          -> throwE PluginStaleResolve
     | Just [] <- pmAltsConApps
          -> pure $ InL [] -- This happens when the type of the expression is unknown.
     | -- encode the information that there's more than one construtor
       Just (NE.fromList -> pmAltsConApps) <- pmAltsConApps
       -- determine old and new text of the module
     , Just (old, new) <- makeEditText pm pmAltsConApps cursor arrowSyntax -> do
        caps <- lift pluginGetClientCapabilities
        -- compute the edit
        let edit = diffText caps (verTxtDocId, old) new IncludeDeletions
        -- return the action
        pure $ InL [InR
          $ CodeAction { _title       = caseSplitPluginCodeActionTitle
                       , _kind        = Just CodeActionKind_QuickFix
                       , _diagnostics = Just [diag]
                       , _isPreferred = Nothing
                       , _disabled    = Nothing
                       , _edit        = Just edit
                       , _command     = Nothing
                       , _data_       = Nothing }]
     | otherwise
          -> throwE $ PluginInternalError "Error in updating the AST."
  where

    getMaybeDsMsg :: FileDiagnostic -> Maybe DsMessage
    getMaybeDsMsg d = fdStructuredMessage d ^? _SomeStructuredMessage . msgEnvelopeErrorL . _DsMessage

    dsMsgToPmAlts :: DsMessage -> Maybe [PmAltConApp]
    dsMsgToPmAlts =
      \case DsNonExhaustivePatterns CaseAlt _ _ [identifier] nablas -> nablasToPmAlts identifier nablas
            DsNonExhaustivePatterns (LamAlt LamCase) _ _ [identifier] nablas -> nablasToPmAlts identifier nablas
            _ -> Nothing

caseSplitPluginCodeActionTitle :: Text
caseSplitPluginCodeActionTitle = "Add placeholders for the first `-fmax-uncovered-patterns` missing patterns"

-- | Retrieve list of pattern match constructors
-- for the type identified by the given 'Id'.
--
-- Relevant information at https://simon.peytonjones.org/assets/pdfs/lower-your-guards.pdf
nablasToPmAlts :: Id -> [Nabla] -> Maybe [PmAltConApp]
nablasToPmAlts identifier nablas = fmap concat $ traverse go nablas
  where
    go = fmap vi_pos
       . flip lookupUSDFM identifier
       . ts_facts
       . nabla_tm_st

-- | Assign an 'Ordering' to two 'Range's @r1@ and @r2@ of which either is assumed to be subset of the other.
-- Will throw a runtime error if @r1@ is not a subrange of @r2@ or vice versa.
ordSubrange :: Range -> Range -> Ordering
ordSubrange r1 r2
  | r1 == r2 = EQ
  | r1 `isSubrangeOf` r2 = LT
  | r2 `isSubrangeOf` r1 = GT
  | otherwise = error "ordSubrange: ranges are not subranges of each other"

_DsMessage :: Fold GhcMessage DsMessage
_DsMessage = prism' GhcDsMessage $ \case
  GhcDsMessage dsmsg -> Just dsmsg
  _ -> Nothing

type MissingPatterns = NonEmpty PmAltConApp

-- | Given a 'ParsedModule' this function uses 'exactPrint' to produce the
-- 'Text's of said module before and after the 'MissingPatterns' are appended
-- to the existing ones in the innermost @case@ expression enclosing the
-- 'Range' of the cursor, using the arrow style passed as the last
-- 'IsUnicodeSyntax' argument.
makeEditText :: ParsedModule -> MissingPatterns -> Range -> IsUnicodeSyntax -> Maybe (Text, Text)
makeEditText pm missingPs cursor arrowSyntax =

  let ps = pm_parsed_source pm
      old = T.pack $ exactPrint ps
      -- We want to update exactly one node of the AST, the one that is
      -- associated to the innermost @case@ expression containing the cursor,
      -- therefore:
      ps' = runMaybeT (everywhereM (go arrowSyntax) ps) -- we transform the 'ParsedSource' bottom-up
                                          -- (allowing failure, incidentally),
            `evalState` False -- and we pass a 'Bool' through 'State' to bail
                               -- out after one update.
      new = fmap (T.pack . exactPrint) ps'

  in sequence (old, new)

    where
      go :: forall a. Data a => IsUnicodeSyntax -> a -> MaybeT (State Bool) a
      go arrow node = do
          found <- get
          if | -- Proceed only if we haven't found & edited the node yet,
               not found
               -- only inspect nodes of the appropriate type,
             , Just HRefl <- typeOf node `eqTypeRep` typeRep @(HsExpr GhcPs)
               -- parse @case@-like expressions, and extract the 'SrcSpan' the
               -- whole expression occupies, as well as the indentation of the
               -- first alternative (see 'parseCaseLikeExpr' for more details),
             , Just (caseLikeNode, span, indent) <- parseCaseLikeExpr node
               -- make sure the cursor is somewhere in that span,
             , cursor `inSpan` span
               -> do -- take note we've found the node,
                     put True
                     -- make a match out of each missing pattern,
                     let missingPs' = traverse (makeMatch arrow) missingPs
                     -- extract existing matches
                     let existingMatches = getMatchGroup caseLikeNode
                     -- and append the missing to ones to them.
                     case appendMissingPats indent existingMatches =<< missingPs' of
                        -- If something goes wrong, we communicate abortion,
                        Nothing      -> mzero
                        -- otherwise we continue.
                        Just newPats -> pure $ setMatches caseLikeNode newPats
             -- Anything else, leave the node unchanged.
             | otherwise -> pure node

      -- | Predicate telling the given 'Range' falls within the given 'SrcSpan'.
      inSpan :: Range -> SrcSpan -> Bool
      inSpan range s = maybe False (range `isSubrangeOf`) (srcSpanToRange s)

-- | While @HsExpr GhcPs@ can contain any expression, the following refined
-- type can only contain a @case@ or a @\case@ expression.
data CaseOrLamCase = Case (XCase GhcPs) (LHsExpr GhcPs) (MatchGroup GhcPs (LHsExpr GhcPs))
                   | LambdaCase (XLam GhcPs) (MatchGroup GhcPs (LHsExpr GhcPs))

-- | Get the 'MatchGroup' out of a 'CaseOrLamCase'.
getMatchGroup :: CaseOrLamCase -> MatchGroup GhcPs (LHsExpr GhcPs)
getMatchGroup (Case _ _ mg)     = mg
getMatchGroup (LambdaCase _ mg) = mg

-- | Parse an @HsCase _ _ mg@ or @HsLam _ LamCase mg@ out of a @HsExpr GhcPs@,
-- and return:
--
--      - the input `HsExpr GhcPs` information, but wrapped in the refined
--        type 'CaseOrLamCase',
--      - the 'SrcSpan' the parsed expression occupies,
--      - the indentation of the first existing match (see also
--        'getIndentation').
parseCaseLikeExpr :: HsExpr GhcPs -> Maybe (CaseOrLamCase, SrcSpan, Maybe Int)

parseCaseLikeExpr (HsCase ext scrut ps)
  | EpAnnHsCase (EpTok caseTok) (EpTok ofTok) <- ext
  , let caseSSpan = getHasLoc caseTok
        ofSSpan = getHasLoc ofTok
  , MG _ (L (EpAnn endTok _ _) _) <- ps
  , let endSSpan = getHasLoc endTok
        span = caseExprSpan caseSSpan ofSSpan endSSpan
  = Just (Case ext scrut ps, span, getIndentation ps)

parseCaseLikeExpr (HsLam ext LamCase ps)
  | EpAnnLam (EpTok backslashTok) (Just caseTok) <- ext
  , let backslashSSpan = getHasLoc backslashTok
        caseSSpan = getHasLoc caseTok
  , MG _ (L (EpAnn endTok _ _) _) <- ps
  , let endSSpan = getHasLoc endTok
        span = caseExprSpan backslashSSpan caseSSpan endSSpan
  = Just (LambdaCase ext ps, span, getIndentation ps)

parseCaseLikeExpr _ = Nothing

-- | Given a 'MatchGroup', this function returns
--
--     - 'Nothing' if the matches are not braced,
--
--     - @Just i@ if the matches are braced, being @i@ equal to
--
--         - @indentation def@ when there's no matches,
--
--         - otherwise, the indentation of the first alternative with respect
--           to the @{@.
--
getIndentation :: MatchGroup GhcPs (LHsExpr GhcPs) -> Maybe Int
getIndentation (MG { mg_alts = L altsLoc existingMatches })
  = do openingBraceCol <- getOpeningBraceCol altsLoc
       let fstExistingIndent = do fstExistingCol <- getStartCol <$> listToMaybe existingMatches
                                  Just (fstExistingCol - openingBraceCol)
       fstExistingIndent <|> Just (indentation def)

-- | Given a @case@ or @\case@ expression wrapped in our refined
-- 'CaseOrLamCase' type and a 'MatchGroup', it creates an actual corresponding
-- @HsExpr GhcPs@ with that 'MatchGroup' in it.
setMatches :: CaseOrLamCase -> MatchGroup GhcPs (LHsExpr GhcPs) -> HsExpr GhcPs
setMatches (Case x s _) mg     = HsCase x s mg
setMatches (LambdaCase x _) mg = HsLam x LamCase mg

-- | Given the 'SrcSpan' of the @case@ token, the @of@ token, and the end of
-- the alternatives, this function combines them to return a 'SrcSpan' that goes
-- from the @case@ token to the end of the whole @case@ expression.
caseExprSpan :: SrcSpan -> SrcSpan -> SrcSpan -> SrcSpan
caseExprSpan caseSSpan _ endSSpan@(RealSrcSpan _ _) = combineSrcSpans caseSSpan endSSpan
caseExprSpan caseSSpan ofSSpan _ = combineSrcSpans caseSSpan ofSSpan

-- | Given a 'MatchGroup' and a list of 'LMatch'es, this function inserts the
-- latter matches in the former group, trying to honor the existing layout,
-- returning the new 'MatchGroup' in the 'Maybe' monad to account for failure.
--
-- For the meaning of the first argument of type @Maybe Int@, see
-- 'getIndentation'.
--
-- Honoring the existing layout means two things:
--
--   1. producing valid code, which means:
--
--      - adding semicolons wherever they are needed, i.e.
--
--        - if matches are braced, for every matches,
--
--        - otherwise, for all but the last matches for groups of matches
--          that are not aligned vertically, e.g.
--
--            - matches shown on the same line, which this plugin can produce,
--
--            - matches shown on different lines but in a "staircase" way,
--              which this plugin never produces).
--
--      - using the correct indentation when matches are not braced (when
--        matches are braced, the code will stay valid irrespective of the
--        indentation of the alternatives).
--
--   2. such valid code tries to adhere to the existing layout, which means:
--
--      - don't alter position of existing matches nor of the opening @{@;
--
--      - when matches are not braced, we align the first match we insert
--        with the pre-existing previous match
--
--      - we have to make some arbitrary decision
--
--        - when matches are not braced and no previous match exists,
--          we indent by @indentation def@ with respect to whatever layout
--          context is the current one;
--
--        - as regards the number of matches to print per line, we inspect the
--          last group of matches appearing on one line, to determine how many
--          matches per line we insert.
--
--        - when matches are braced, we also align them vertically (it would
--          not be necessary, in principle).
--
--
-- Refer to test cases to see practical examples.
appendMissingPats :: Maybe Int -> MatchGroup GhcPs (LHsExpr GhcPs) -> NonEmpty (LMatch GhcPs (LHsExpr GhcPs)) -> Maybe (MatchGroup GhcPs (LHsExpr GhcPs))
appendMissingPats mayIndent mg@(MG { mg_alts = L altsLoc existingMatches }) missingMatches
  = let -- Chunkify the matches to be inserted,
        missingGroup :| missingGroups = prettyChunksOf size missingMatches
        -- and let all chunks have a common size, which is
        size = case existingMatches of
                 [] -> 1 -- trivially 1 if there's no existing matches,
                      -- otherwise, set the size equal to the length
                      -- of the last group of @existingMatches@ that
                      -- are on the same line:
                 _ -> NE.length
                    $ NE.last
                    $ NE.groupBy1 isOnelined (NE.fromList existingMatches)

        -- Detect if the list of alternatives is between @{@ and @}@:
        isBraced = isJust $ getOpeningBraceCol altsLoc

        -- Finally, we lay out the missing matches:
        missingMatchesEP = -- indent the first group and the following ones (see discussion above)
                           mapFirst indentHead missingGroup :| map (mapFirst indentTail) missingGroups
                           -- add a semicolon to the end of each group only if the alternatives are braced
                         & (if isBraced then addSemicols else id)
                           -- put each group on its own line
                         & NE.map (mapFirst putOnNewLine)
                           -- join the groups
                         & join
                           -- turn into an ordinary list
                         & NE.toList
          where
            -- add semicolons
            addSemicols = NE.zipWith ($)
                                      -- for each one-line group of matches,
                                     (replicate (length missingGroups)
                                                -- only to the last match of the group
                                                (mapLast addSemiCol)
                                      -- except for the last group
                                      |: id)

            -- Indentation is complicated.
            --
            -- For a non-braced @case@-like expression, the first match **of the
            -- whole expression** (I mean, not the first match **to be inserted**)
            -- has some anchor that depends on the surrounding code, while the
            -- following matches all use their own predecessor as the anchor.
            --
            -- Otherwise (i.e. for a braced @case@-like expression), all matches
            -- including the first one have the same anchor that depends on the
            -- surrounding code.
            --
            -- Therefore, here's how we set the DeltaPos for the first and
            -- following matches:
            (setDPCol -> indentHead, setDPCol -> indentTail)
               = case mayIndent of
                   -- non-braced with some existing matches
                   Nothing | null existingMatches -> (indentation def, 0)
                   -- non-braced without existing matches
                   Nothing                        -> (0, 0)
                   -- braced
                   Just i                         -> (i, i)

        -- Only if there's braces do we need to make sure the last of the
        -- existing matches ends with @;@:
        existingMatchesEP = if isBraced
                               then dropEnd 1 existingMatches <> (addSemiCol <$> takeEnd 1 existingMatches)
                               else existingMatches

    in Just $ mg { mg_alts = L altsLoc (existingMatchesEP <> missingMatchesEP) }

-- | Accepts a @NonEmpty (LocatedAn AnnListItem a)@ and chunkifies it by the given 'size',
-- putting all matches of each chunk on the same line, leaving 1 space in between, and
-- keeping the code valid by adding semicolons to all but the last match of each chunk.
prettyChunksOf :: Int -> NonEmpty (LocatedAn AnnListItem a) -> NonEmpty (NonEmpty (LocatedAn AnnListItem a))
prettyChunksOf size allMatches = do
  -- For each chunk
  chunk <- chunksOf1 size allMatches
  pure $ fromZipList
       $ do -- of all the matches of chunk
            match       <- toZipList chunk
            -- from the second match onwards, they go the same line, one space apart
            putBeside   <- toZipList $ id :| repeat (setDP 0 1)
            -- all but the last match get a semicolon
            addSemicols <- toZipList $ replicate (length chunk - 1) addSemiCol |: id
            -- apply
            pure $ addSemicols $ putBeside match
  where
    toZipList = ZipList . NE.toList
    fromZipList = NE.fromList . getZipList

-- | Given a 'PmAltConApp', this function produces an 'LMatch' to be inserted
-- in the list of existing 'LMatch'es contained by a 'MatchGroup'.
--
-- The returned 'LMatch' is wrapped in 'Maybe' to account for failure, and it
-- is constructed in its entirety, by passing "default" values wherever
-- possible, except, obviously, for two:
--
--  - the constructor name,
--  - the arguments to the constructor, all rendered as individual underscores
--    when there's less than @maxUnderscores def@, or as a single @{}@ otherwise.
--
-- As regards the monad transformers,
--
-- The first argument of type 'UnicodeSyntax' simply contains the symbol used
-- for the arrow, which can be @->@ or @→@ depending on whether the 'UnicodeSyntax'
-- language extension is being used.
makeMatch :: IsUnicodeSyntax -> PmAltConApp -> Maybe (LMatch GhcPs (LHsExpr GhcPs))
makeMatch arrow PACA{ paca_con = PmAltConLike (RealDataCon dataCon)
                    , paca_ids
                    }
        = let -- Extract the name of the constructor
              ctorName = L noSrcSpanA $ nameRdrName $ getName dataCon
              -- assemble the construtor with the arguments, adding
              -- underscores or empty braces:
              ctor = case length paca_ids of
                              -- for low number of arguments
                              n | n <= maxUnderscores def
                                   -- create as many underscores as needed
                                -> ConPat { pat_con_ext = (Nothing, Nothing)
                                          , pat_con = ctorName
                                          , pat_args = PrefixCon $ map (const $ L noAnnSrcSpanDP1 $ WildPat NoExtField) paca_ids
                                          }
                                   -- otherwise use braces.
                              _ -> ConPat { pat_con_ext = (Just (EpTok d1), Just (EpTok d0))
                                          , pat_con = ctorName
                                          , pat_args = RecCon (HsRecFields NoExtField [] Nothing)
                                          }
          in do Just $ L noSrcSpanA
                 $ Match { m_ext = NoExtField
                         , m_ctxt = CaseAlt
                         , m_pats = L noSrcSpanA [L noSrcSpanA ctor]
                         , m_grhss = GRHSs emptyComments
                                           -- TODO: check whether ga_sep default choice is really not printing anything.
                                           (NE.singleton $ L noSrcSpanA $ GRHS (EpAnn noSrcSpanA
                                                                                      (GrhsAnn{ ga_vbar = Nothing
                                                                                              , ga_sep = Right $ EpUniTok d1 arrow })
                                                                                      emptyComments) []
                                                         $ L noSrcSpanA $ HsHole $ HoleVar $ L noAnnSrcSpanDP1 $ unnamedHoleRdrName)
                                           (EmptyLocalBinds NoExtField)
                         }
makeMatch _ _ = Nothing

data Default = Default {
  -- | Max number of underscores to show for the constructor of an alternative.
  -- Beyond this, the record syntax with empty braces is used.
  maxUnderscores :: Int
  -- | Indentation used when there's no existing alternatives to refer to.
  -- Such indentation is with respect to the current layout context.
, indentation    :: Int
  -- TODO other things that we could store here are:
  --
  --    - the maximum number of alternatives on one line
  --    - whether or not to put the @;@ for the last alternative
}

def :: Default
def = Default { maxUnderscores = 3
              , indentation = 2 }

-- | Predicate telling if two located annotations are (actually, start) on the
-- same line.
isOnelined :: LocatedAn ann e -> LocatedAn ann e -> Bool
isOnelined = (==) `on` getStartLine

-- | Given an @EpAnn (AnnList a)@ return the starting column of
-- its opening brace, if any, otherwise 'Nothing'.
getOpeningBraceCol :: EpAnn (AnnList a) -> Maybe Int
getOpeningBraceCol (EpAnn _ (AnnList _ (ListBraces (EpTok col) _) _ _ _) _) = Just $ getStartCol $ getHasLoc col
getOpeningBraceCol _ = Nothing

-- | Get the starting column of an 'HasSrcSpan'.
getStartCol :: HasSrcSpan a => a -> Int
getStartCol = srcSpanStartCol . realSrcSpan . getLoc

-- | Get the starting line of an 'HasSrcSpan'.
getStartLine :: HasSrcSpan a => a -> Int
getStartLine = srcSpanStartLine . realSrcSpan . getLoc

-- | Set the DeltaPos for the given annotation.
setDP :: Int -> Int -> LocatedAn t a -> LocatedAn t a
setDP deltaLine deltaColumn lann = setEntryDP lann $ deltaPos deltaLine deltaColumn

-- | Set the deltaColumn for the given annotation.
setDPCol :: Int -> LocatedAn t a -> LocatedAn t a
setDPCol deltaColumn lann = setEntryDP lann
                          $ (\d -> deltaPos (getDeltaLine d) deltaColumn)
                          $ getEntryDP lann

-- | Set the deltaLine for the given annotation.
setDPLine :: Int -> LocatedAn t a -> LocatedAn t a
setDPLine deltaLine lann = setEntryDP lann
                          $ (\d -> deltaPos deltaLine (deltaColumn d))
                          $ getEntryDP lann
-- | Useful helper.
putOnNewLine :: LocatedAn t a -> LocatedAn t a
putOnNewLine = setDPLine 1

-- | Add semicolon, unless one is already present.
addSemiCol :: LocatedAn AnnListItem a -> LocatedAn AnnListItem a
addSemiCol (L l@(EpAnn _ ls _) e)
  | none isSemiCol (lann_trailing ls)
  = L (addTrailingAnnToA (AddSemiAnn (EpTok d0)) emptyComments l) e
  where
    isSemiCol :: TrailingAnn -> Bool
    isSemiCol (AddSemiAnn _) = True
    isSemiCol _              = False
addSemiCol l = l

-- | Version of 'Data.List.Extra.chunksOf' (**not** to be confused with
-- 'Data.List.Split.chunksOf') for a 'NonEmpty' lists.
chunksOf1 :: Int -> NonEmpty a -> NonEmpty (NonEmpty a)
chunksOf1 n xs
  | n >= 1
  , (b:before, after) <- NE.splitAt n xs
    = (b :| before) :| case after of
                         [] -> []
                         _  -> map NE.fromList $ chunksOf n after
  | otherwise = error "chunksOf1: the `Int` argument should be ≥ 1"

-- | Maps a function @f@ over the first element of a 'NonEmpty' list.
mapFirst :: (a -> a) -> NonEmpty a -> NonEmpty a
mapFirst f (a :| as) = f a :| as

-- | Maps a function @f@ over the last element of a 'NonEmpty' list.
mapLast :: (a -> a) -> NonEmpty a -> NonEmpty a
mapLast f (a :| []) = f a :| []
mapLast f (a :| as) = a :| mapLast' f as
  where
    mapLast' f as = init as ++ [f $ last as]

-- | Convenient negation of 'any'.
none :: Foldable t => (a -> Bool) -> t a -> Bool
none p xs = not $ any p xs
