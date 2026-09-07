{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OrPatterns        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}

{- | __Implementation strategy__

  The present plugin achieves its target of appending the missing patterns to a
  non-exhaustive @case@ (or @\\case@) expression via the following strategy:

    1. the HLS utility 'activeDiagnosticsInRange' retrieves the
       @['FileDiagnostic']@ under the client-provided range,

    2. @'getInnermost' . 'extractDiagAndMissingCtors'@ is used to extract the
       'Diagnostic' and the 'NonEmpty' list of missing 'PmAltConApp' from the
       innermost ("innermost" intended according to 'isSubrangeOf') among the
       "non-exhaustive patterns" diagnostics (i.e. those containing a
       'DsMessage' constructed via 'DsNonExhaustivePatterns'),

    3. some functions running in the @'ExceptT' 'PluginError' ('HandlerM'
       'Config')@ monad are used retrieve some context necessary to construct
       the 'WorkspaceEdit' and to apply it:

          - the 'ParsedSource' describing the AST before the change to be
            applied,
          - whether the 'UnicodeSyntax' extension is in use,
          - the 'ClientCapabilities',
          - the 'VersionedTextDocumentIdentifier',

    4. 'graftMissingPatterns' uses 'everywhereM' to traverse the AST, for the
       purpose of

          - pinpointing the one node representing the innermost @case@ (or
            @\\case@) expression encompassing the client-provided range,

                - in this phase, the relevant @case@ expression is parsed
                  for detecting the current layout (whether the existing
                  alternatives, if any, are between @{@ and @}@, and in that
                  case, what's the indentation of the first existing
                  alternative),

          - turning the missing 'PmAltConApp's patterns (obtained from the
            diagnostic in step 2 above) into 'LMatch'es (to be inserted in the
            AST) via 'makeMatch',

               - 'makeMatch' can currently "fail" (by returning in 'Either')
                 because we don't support missing patterns that are not
                 'PmAltConLike' or, if they are, that are not 'RealDataCon', in
                 which case we simply log this fact and return an empty list of
                 'CodeAction's.

          - appending those 'LMatch'es to the existing ones, honoring the
            existing layout.
-}

module Ide.Plugin.CaseSplit
  ( caseSplitPluginCodeActionTitle
  , descriptor
  , Log
  ) where

import Language.Haskell.GHC.ExactPrint.Transform (
          appendMissingPats
        , MatchLayout(..)
        , Matches(..))
import           Control.Arrow                         ((&&&), (>>>))
import           Control.Lens                          ((^.), (^?))
import           Control.Monad                         ((>=>))
import           Control.Monad.Except                  (runExceptT, throwError)
import           Control.Monad.IO.Class                (MonadIO (liftIO))
import           Control.Monad.State.Strict            (MonadState (get, put),
                                                        State, evalState)
import           Control.Monad.Trans                   (lift)
import           Control.Monad.Trans.Except            (ExceptT)
import           Data.Data                             (Data)
import           Data.Function                         (on, (&))
import           Data.Generics.Schemes                 (everywhereM)
import           Data.List.NonEmpty                    (NonEmpty (),
                                                        nonEmpty)
import qualified Data.List.NonEmpty                    as NE
import           Data.Maybe                            (listToMaybe,
                                                        mapMaybe, maybeToList)
import           Data.Text                             (Text)
import qualified Data.Text                             as T
import           Development.IDE                       (FileDiagnostic (fdStructuredMessage),
                                                        GetParsedModule (GetParsedModule),
                                                        GhcSessionDeps (GhcSessionDeps),
                                                        HscEnvEq (hscEnv),
                                                        IdeState (shakeExtras),
                                                        Pretty (pretty), Range,
                                                        Recorder, WithPriority,
                                                        runAction,
                                                        spanContainsRange)
import           Development.IDE.Core.FileStore        (getVersionedTextDoc)
import           Development.IDE.Core.PluginUtils      (activeDiagnosticsInRange,
                                                        runActionE, useE)
import           Development.IDE.GHC.Compat            (ConLike (PatSynCon, RealDataCon),
                                                        HoleKind (HoleVar),
                                                        HsMatchContext (CaseAlt),
                                                        HscEnv (hsc_dflags), Id,
                                                        NamedThing (getName),
                                                        Outputable (ppr),
                                                        getLoc, showSDocUnsafe)
import           Development.IDE.GHC.Compat.Core       (EpAnnHsCase (EpAnnHsCase),
                                                        GrhsAnn (..),
                                                        HasSrcSpan,
                                                        HsLamVariant (LamCase),
                                                        HsMatchContext (LamAlt),
                                                        srcSpanStartCol)
import qualified Development.IDE.GHC.Compat.Core       as Ext
import           Development.IDE.GHC.Compat.Error      (DsMessage (DsNonExhaustivePatterns),
                                                        _DsMessage,
                                                        msgEnvelopeErrorL)
import           Development.IDE.GHC.Compat.ExactPrint (d0, d1, exactPrint,
                                                        noAnnSrcSpanDP1)
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
                                                        ParsedSource,
                                                        realSrcSpan)
import           GHC.Driver.DynFlags                   (OnOff (On))
import           GHC.Hs                                (EpAnnLam (EpAnnLam),
                                                        GhcPs,
                                                        HsRecFields (HsRecFields),
                                                        XCase, XLam,
                                                        unnamedHoleRdrName)
import           GHC.HsToCore.Pmc.Solver.Types         (Nabla (nabla_tm_st),
                                                        PmAltCon (..),
                                                        PmAltConApp (..),
                                                        TmState (ts_facts),
                                                        VarInfo (vi_pos))
import           GHC.Parser.Annotation                 (EpUniToken (EpUniTok),
                                                        IsUnicodeSyntax (NormalSyntax, UnicodeSyntax),
                                                        emptyComments,
                                                        noSrcSpanA)
import           GHC.Types.Name.Reader                 (nameRdrName)
import           GHC.Types.SrcLoc                      (GenLocated (L),
                                                        SrcSpan (RealSrcSpan),
                                                        combineSrcSpans)
import           GHC.Types.Unique.SDFM                 (lookupUSDFM)
import           Ide.Logger                            (Priority (Warning),
                                                        logWith)
import           Ide.Plugin.Error                      (PluginError,
                                                        getNormalizedFilePathE)
import           Ide.PluginUtils                       (WithDeletions (IncludeDeletions),
                                                        diffText)
import           Ide.Types                             (Config, HandlerM,
                                                        PluginDescriptor (pluginHandlers),
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
import           Language.LSP.Protocol.Types           (ClientCapabilities,
                                                        CodeAction (..),
                                                        CodeActionKind (CodeActionKind_QuickFix),
                                                        CodeActionParams (CodeActionParams, _range, _textDocument),
                                                        Diagnostic,
                                                        NormalizedFilePath,
                                                        TextDocumentIdentifier,
                                                        VersionedTextDocumentIdentifier,
                                                        WorkspaceEdit,
                                                        isSubrangeOf,
                                                        type (|?) (InL, InR))
import qualified Language.LSP.Protocol.Types           as Diag (Diagnostic (_range))
import           Type.Reflection                       (eqTypeRep,
                                                        type (:~~:) (HRefl),
                                                        typeOf, typeRep)

data Log where
  LogPatternNotSupportedYet :: String -> Log

instance Pretty Log where
  pretty (LogPatternNotSupportedYet unsupportedPat) = "The case-split plugin does not support the pattern " <> pretty unsupportedPat <> " yet."

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId = (defaultPluginDescriptor plId "Provides the split case code action")
  { pluginHandlers = mkPluginHandler LSP.SMethod_TextDocumentCodeAction (suggestCaseSplitProvider recorder)
  }

caseSplitPluginCodeActionTitle :: Text
caseSplitPluginCodeActionTitle = "Add placeholders for missing patterns"

suggestCaseSplitProvider :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
suggestCaseSplitProvider recorder state _ CodeActionParams{..}
  = do

  nfp <- getNormalizedFilePathE $ _textDocument ^. L.uri

  fileDiags <- activeDiagnosticsInRange (shakeExtras state) nfp _range

  let diagAndMissingCtors = getInnermost . extractDiagAndMissingCtors $ fileDiags

  arrowSyntax <- getArrowSyntax state nfp
  psOld <- getParsedSource state nfp
  caps <- lift pluginGetClientCapabilities
  verTxtDocId <- lift $ getVerTxtDocId state _textDocument

  codeAction <- case traverse (makeCodeAction caps verTxtDocId psOld arrowSyntax) diagAndMissingCtors of
                     Left unsupportedPat -> do logWith recorder Warning $ LogPatternNotSupportedYet unsupportedPat
                                               pure Nothing
                     Right cAct -> pure cAct

  pure $ InL $ InR <$> maybeToList codeAction

  where
    makeCodeAction :: ClientCapabilities
                   -> VersionedTextDocumentIdentifier
                   -> ParsedSource
                   -> IsUnicodeSyntax
                   -> (Diagnostic, MissingPatterns)
                   -> Either String CodeAction
    makeCodeAction caps verTxtDocId psOld arrowSyntax (diag, pmAltsConApps)
        = do psNew <- graftMissingPatterns psOld _range pmAltsConApps arrowSyntax
             pure $ make diag $ makeEditText caps verTxtDocId psOld psNew
      where
        make :: Diagnostic -> WorkspaceEdit -> CodeAction
        make diag edit
          = CodeAction { _title       = caseSplitPluginCodeActionTitle
                       , _kind        = Just CodeActionKind_QuickFix
                       , _diagnostics = Just [diag]
                       , _isPreferred = Nothing
                       , _disabled    = Nothing
                       , _edit        = Just edit
                       , _command     = Nothing
                       , _data_       = Nothing }

-- | Retrieve 'VersionedTextDocumentIdentifier' from the handler.
getVerTxtDocId :: IdeState -> TextDocumentIdentifier -> HandlerM Config VersionedTextDocumentIdentifier
getVerTxtDocId state textDoc = liftIO $ runAction "CaseSplit.GetVersionedTextDoc" state $ getVersionedTextDoc textDoc

-- | Retrieve 'ParsedSource' from the handler.
getParsedSource :: IdeState -> NormalizedFilePath -> ExceptT PluginError (HandlerM Config) ParsedSource
getParsedSource state nfp = pm_parsed_source <$> runActionE "CaseSplit.GetParsedModule"
                                                            state
                                                            (useE GetParsedModule nfp)

-- | Retrieve 'IsUnicodeSyntax' from the handler.
getArrowSyntax :: IdeState -> NormalizedFilePath -> ExceptT PluginError (HandlerM Config) IsUnicodeSyntax
getArrowSyntax state nfp = do
  (hsc_dflags . hscEnv -> dynFlags) <- runActionE "CaseSplit.GhcSessionDeps" state $ useE GhcSessionDeps nfp
  pure $ if On Ext.UnicodeSyntax `elem` extensions dynFlags
    then UnicodeSyntax
    else NormalSyntax

-- | Obtain a 'WorkspaceEdit' as 'diffText' of 'exactPrint'-ed versions of old
-- and new 'ParsedSource's.
makeEditText :: ClientCapabilities -> VersionedTextDocumentIdentifier -> ParsedSource -> ParsedSource -> WorkspaceEdit
makeEditText caps verTxtDocId psOld psNew = do
  let old = T.pack $ exactPrint psOld
  let new = T.pack $ exactPrint psNew
  diffText caps (verTxtDocId, old) new IncludeDeletions

-- | Type synonym for slighly improved readability.
type MissingPatterns = NonEmpty PmAltConApp

-- | Given a @[FileDiagnostic]@ retain only those relative
-- to the GHC-62161 diagnostic and extract the list of missing
-- patterns from those.
extractDiagAndMissingCtors :: [FileDiagnostic] -> [(Diagnostic, MissingPatterns)]
extractDiagAndMissingCtors = map -- For each 'FileDiagnostic',
                                 (fdLspDiagnostic -- extract is 'Diagnostic'
                                 &&&
                                 -- and 'Maybe' a 'NonEmpty' list of 'PmAltConApp',
                                 (getDsMessage >=> getPmAltConApps >=> nonEmpty))
                          -- finally, discard the irrelevant diagnostics.
                          >>> (mapMaybe sequence :: [(a, Maybe b)] -> [(a, b)])
  where

    getDsMessage :: FileDiagnostic -> Maybe DsMessage
    getDsMessage d = fdStructuredMessage d ^? _SomeStructuredMessage . msgEnvelopeErrorL . _DsMessage

    getPmAltConApps :: DsMessage -> Maybe [PmAltConApp]
    getPmAltConApps =
      \case DsNonExhaustivePatterns CaseAlt _ _ [identifier] nablas -> nablasToPmAlts identifier nablas
            DsNonExhaustivePatterns (LamAlt LamCase) _ _ [identifier] nablas -> nablasToPmAlts identifier nablas
            _ -> Nothing

-- | Get the innermost (in the sense of 'isSubrangeOf') @(Diagnostic, a)@,
-- accounting for failure.
getInnermost :: [(Diagnostic, a)] -> Maybe (Diagnostic, a)
getInnermost [] = Nothing
getInnermost (a : as) = foldl' go (Just a) as
  where
    go Nothing _ = Nothing
    go (Just acc) a = case (ordSubrange `on` Diag._range . fst) acc a of
      Just GT -> Just a
      Just _  -> Just acc
      Nothing -> Nothing -- If non-total order, give up.

-- | Assign an 'Ordering' to two 'Range's @r1@ and @r2@ according to the
-- 'isSubrangeOf' relationshipt between them. If neither 'isSubrangeOf' the
-- other, return `Nothing`.
ordSubrange :: Range -> Range -> Maybe Ordering
ordSubrange r1 r2
  | r1 == r2 = Just EQ
  | r1 `isSubrangeOf` r2 = Just LT
  | r2 `isSubrangeOf` r1 = Just GT
  | otherwise = Nothing

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

-- | Given a 'ParsedSource' and a 'Range' on it, this function uses a bottom-up
-- traversal of the AST to detect the innermost @case@/@\\case@ expression
-- encompassing the user-provided 'Range', and it appends the 'MissingPatterns' to
-- the existing ones, if any, using the syntax @->@ or @→@ depending on the
-- provided 'IsUnicodeSyntax'. The new 'ParsedSource' is returned in the
-- 'Maybe' monad to account for failure.
--
-- Implementation detail: since we want to update exactly one node of the AST
-- we run the computation in a 'State Bool' monad to bail out after one update.
graftMissingPatterns :: ParsedSource -> Range -> MissingPatterns -> IsUnicodeSyntax -> Either String ParsedSource
graftMissingPatterns ps range missingPs arrowSyntax
  = runExceptT (everywhereM go ps) `evalState` False
    where
      go :: forall a. Data a => a -> ExceptT String (State Bool) a
      go node = do
          found <- get
          if | -- Proceed only if we haven't found & edited the node yet,
               not found
               -- only inspect nodes of the appropriate type,
             , Just HRefl <- typeOf node `eqTypeRep` typeRep @(HsExpr GhcPs)
               -- parse the current @case@-like expressions into a 'CaseLike'
               -- (see also 'parseCaseLikeExpr' for more details),
             , Just (CaseLike {_expr, _span, _layout}) <- parseCaseLikeExpr node
               -- make sure the 'range' is somewhere in the span of that
               -- expression,
             , Just True <- _span `spanContainsRange` range
               -> do -- take note we've found the node,
                     put True
                     -- extract existing matches
                     let existingMatches = _matchGroup _expr
                     -- make a match out of each missing pattern,
                     case traverse (makeMatch arrowSyntax) missingPs of
                        -- If this sort of pattern is not supported, we abort,
                        Left unsupportedPat  -> throwError unsupportedPat
                        -- otherwise we continue
                        Right missingMatches -> -- by appending the missing matches to the existing ones
                                               appendMissingPats _layout existingMatches missingMatches
                                               -- and setting those matches in a new expression.
                                             & setMatches _expr
                                             & pure
             -- Anything else, leave the node unchanged.
             | otherwise -> pure node

-- | While @HsExpr GhcPs@ can contain any expression, the following refined
-- type can only contain a @case@ or a @\\case@ expression.
data CaseLikeExpr = Case { _extCase    :: XCase GhcPs
                         , _scrut      :: LHsExpr GhcPs
                         , _matchGroup :: MatchGroup GhcPs (LHsExpr GhcPs)
                         }
                  | LambdaCase { _extLamCase :: XLam GhcPs
                               , _matchGroup :: MatchGroup GhcPs (LHsExpr GhcPs)
                               }

-- | A 'CaseLikeExpr' enriched with the 'SrcSpan' it occupies, together with
-- its 'MatchLayout'.
data CaseLike = CaseLike { _expr   :: CaseLikeExpr
                         , _span   :: SrcSpan
                         , _layout :: MatchLayout
                         }

-- | Parse an @HsCase _ _ mg@ or @HsLam _ LamCase mg@ out of a @HsExpr GhcPs@
-- into the refined type 'ConLike'.
parseCaseLikeExpr :: HsExpr GhcPs -> Maybe CaseLike

parseCaseLikeExpr (HsCase ext scrut matchGroup)
  | EpAnnHsCase (EpTok caseTok) (EpTok ofTok) <- ext
  , let caseSSpan = getHasLoc caseTok
        ofSSpan = getHasLoc ofTok
  , MG _ (L (EpAnn endTok _ _) _) <- matchGroup
  , let endSSpan = getHasLoc endTok
        span = caseExprSpan caseSSpan ofSSpan endSSpan
  = Just $ CaseLike { _expr = Case ext scrut matchGroup
                    , _span = span
                    , _layout = parseMatchLayout matchGroup
                    }

parseCaseLikeExpr (HsLam ext LamCase matchGroup)
  | EpAnnLam (EpTok backslashTok) (Just caseTok) <- ext
  , let backslashSSpan = getHasLoc backslashTok
        caseSSpan = getHasLoc caseTok
  , MG _ (L (EpAnn endTok _ _) _) <- matchGroup
  , let endSSpan = getHasLoc endTok
        span = caseExprSpan backslashSSpan caseSSpan endSSpan
  = Just $ CaseLike { _expr = LambdaCase ext matchGroup
                    , _span = span
                    , _layout = parseMatchLayout matchGroup
                    }

parseCaseLikeExpr _ = Nothing

-- | Given a 'MatchGroup', this function returns its 'MatchLayout'.
parseMatchLayout :: MatchGroup GhcPs (LHsExpr GhcPs) -> MatchLayout
parseMatchLayout (MG { mg_alts = L altsLoc existingMatches })
  = case (getOpeningBraceCol altsLoc, getStartCol <$> listToMaybe existingMatches) of
      (Nothing, _) -> NonBraced
      (_, Nothing) -> Braced NoMatches
      (Just openingBraceCol, Just fstExistingMatchCol)
        -> let indent = fstExistingMatchCol - openingBraceCol
           in Braced $ SomeMatches indent

-- | Given a @case@ or @\\case@ expression wrapped in the refined 'CaseLikeExpr'
-- type and a 'MatchGroup', it creates an actual corresponding @HsExpr GhcPs@
-- with that 'MatchGroup' in it.
setMatches :: CaseLikeExpr -> MatchGroup GhcPs (LHsExpr GhcPs) -> HsExpr GhcPs
setMatches (Case x s _) mg     = HsCase x s mg
setMatches (LambdaCase x _) mg = HsLam x LamCase mg

-- | Given the 'SrcSpan' of the @case@ token, the @of@ token, and the end of
-- the alternatives, this function combines them to return a 'SrcSpan' that goes
-- from the @case@ token to the end of the whole @case@ expression.
caseExprSpan :: SrcSpan -> SrcSpan -> SrcSpan -> SrcSpan
caseExprSpan caseSSpan _ endSSpan@(RealSrcSpan _ _) = combineSrcSpans caseSSpan endSSpan
caseExprSpan caseSSpan ofSSpan _ = combineSrcSpans caseSSpan ofSSpan

-- | Given a 'IsUnicodeSyntax', describing whether to use @->@ or @→@, and a
-- 'PmAltConApp', this function produces an 'LMatch' (to be inserted in the
-- list of existing 'LMatch'es contained by a 'MatchGroup'), returning it into
-- a 'Maybe' to account for failure.
--
-- The 'LMatch' is constructed in its entirety, by passing "default" values wherever
-- possible, except, obviously, for two:
--
--  - the constructor name,
--  - the arguments to the constructor, all rendered as individual underscores
--    when there's less than @maxUnderscores def@, or as a single @{}@ otherwise.
makeMatch :: IsUnicodeSyntax -> PmAltConApp -> Either String (LMatch GhcPs (LHsExpr GhcPs))
makeMatch arrow pmAltConApp = makeLMatch <$> parseSimpleConMatch arrow pmAltConApp

parseSimpleConMatch :: IsUnicodeSyntax -> PmAltConApp -> Either String SimpleConMatch
parseSimpleConMatch arrow PACA{ paca_con = PmAltConLike con
                              , paca_ids
                              }
  = let dataCon = case con of
                    RealDataCon dataCon -> getName dataCon
                    PatSynCon dataCon   -> getName dataCon

        locatedCon = L noSrcSpanA $ nameRdrName dataCon

        conPat = if length paca_ids <= maxUnderscores def -- for low number of arguments
                     -- create as many underscores as needed
                   then ConPat { pat_con_ext = (Nothing, Nothing)
                               , pat_con = locatedCon
                               , pat_args = PrefixCon $ map (const $ L noAnnSrcSpanDP1 $ WildPat NoExtField) paca_ids
                               }
                     -- otherwise use braces.
                   else ConPat { pat_con_ext = (Just (EpTok d1), Just (EpTok d0))
                               , pat_con = locatedCon
                               , pat_args = RecCon (HsRecFields NoExtField [] Nothing)
                               }
    in Right
     $ SimpleConMatch { _arrow = arrow
                      , _conPat = conPat }

parseSimpleConMatch _ paca = Left $ showSDocUnsafe $ ppr paca

-- | Wrapper to the all the non-default info needed to construct an 'LMatch':
--
--      - the arrow syntax (@->@ or @→@),
--      - the constructor pattern (e.g. @Foo _ _@ for a binary constructor).
data SimpleConMatch = SimpleConMatch { _arrow  :: IsUnicodeSyntax
                                     , _conPat :: Pat GhcPs
                                     }

-- | Produce an 'LMatch' using defaults for all but the information contained
-- in the given a 'SimpleConMatch'.
makeLMatch :: SimpleConMatch -> LMatch GhcPs (LHsExpr GhcPs)
makeLMatch SimpleConMatch{..}
  = L noSrcSpanA $ Match { m_ext = NoExtField
                         , m_ctxt = CaseAlt
                         , m_pats = L noSrcSpanA [L noSrcSpanA _conPat]
                         , m_grhss = GRHSs emptyComments
                                           -- TODO: check whether ga_sep default choice is really not printing anything.
                                           (NE.singleton $ L noSrcSpanA $ GRHS (EpAnn noSrcSpanA
                                                                                      (GrhsAnn{ ga_vbar = Nothing
                                                                                              , ga_sep = Right $ EpUniTok d1 _arrow })
                                                                                      emptyComments) []
                                                         $ L noSrcSpanA $ HsHole $ HoleVar $ L noAnnSrcSpanDP1 $ unnamedHoleRdrName)
                                           (EmptyLocalBinds NoExtField)
                         }

-- | TODO: We could could make these values customizable via HLS plugin
-- settings.
--
-- Other things that we could store here are:
--
--    - the maximum number of alternatives on one line
--    - whether or not to put the @;@ for the last alternative
data Default = Default {
  -- | Max number of underscores to show for the constructor of an alternative.
  -- Beyond this, the record syntax with empty braces is used.
  maxUnderscores :: Int
  -- | Indentation used when there's no existing alternatives to refer to.
  -- Such indentation is with respect to the current layout context.
, indentation    :: Int
}

def :: Default
def = Default { maxUnderscores = 3
              , indentation = 2 }

-- | Given an @EpAnn (AnnList a)@ return the starting column of
-- its opening brace, if any, otherwise 'Nothing'.
getOpeningBraceCol :: EpAnn (AnnList a) -> Maybe Int
getOpeningBraceCol (EpAnn _ (AnnList _ (ListBraces (EpTok col) _) _ _ _) _) = Just $ getStartCol $ getHasLoc col
getOpeningBraceCol _ = Nothing

-- | Get the starting column of an 'HasSrcSpan'.
getStartCol :: HasSrcSpan a => a -> Int
getStartCol = srcSpanStartCol . realSrcSpan . getLoc
