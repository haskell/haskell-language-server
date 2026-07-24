{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Ide.Plugin.CaseSplit
  ( descriptor
  , Log
  ) where

-- TODO: Improve import list:
--
--      1. format all these inludes properly;
--      2. is there a convention for deciding which module to import each name from?
import qualified Data.Text                                as T
import           Development.IDE.Core.PluginUtils (runActionE, useE, activeDiagnosticsInRange)
import Ide.Plugin.Error
    ( PluginError(PluginInternalError),
      getNormalizedFilePathE,
      handleMaybeM )
import Ide.Types
    ( defaultPluginDescriptor,
      mkPluginHandler,
      pluginGetClientCapabilities,
      DynFlagsModifications(dynFlagsModifyGlobal),
      PluginDescriptor(pluginModifyDynflags, pluginHandlers,
                       pluginPriority),
      PluginId,
      PluginMethodHandler )
import qualified Language.LSP.Protocol.Message            as LSP
import           Language.LSP.Protocol.Message (Method(Method_TextDocumentCodeAction))
import qualified Development.IDE.Core.Shake               as Shake
import qualified Language.LSP.Protocol.Types as Diag (Diagnostic(_range))
import Language.LSP.Protocol.Types
    ( Range,
      CodeActionParams(_range, CodeActionParams, _textDocument),
      CodeActionKind(CodeActionKind_QuickFix),
      type (|?)(InR, InL),
      CodeAction(_data_, CodeAction, _title, _kind, _diagnostics,
                 _isPreferred, _disabled, _edit, _command),
      isSubrangeOf )
import Development.IDE.Types.Diagnostics (_SomeStructuredMessage, FileDiagnostic (fdLspDiagnostic))
import Development.IDE.GHC.Compat (GhcMessage (GhcDsMessage), HsMatchContext (CaseAlt), ConLike (RealDataCon), NamedThing (getName), HoleKind (HoleVar))
import Development.IDE.GHC.Compat.Error (DsMessage(DsNonExhaustivePatterns), msgEnvelopeErrorL)
import Data.Maybe (mapMaybe, fromJust)
import Control.Lens (Fold, prism', (^?), (<&>), (^.))
import GHC.HsToCore.Pmc.Solver.Types (PmAltConApp(..), PmAltCon(..), TmState (ts_facts), Nabla (nabla_tm_st), VarInfo (vi_pos))
import GHC.Types.Unique.SDFM ( lookupUSDFM )
import GHC.Types.Name.Reader (nameRdrName)
import GHC (DynFlags(maxUncoveredPatterns), ParsedModule (pm_parsed_source), HasLoc (getHasLoc), realSrcSpan, EpToken (EpTok), EpaLocation' (EpaSpan), AnnList (AnnList), AnnListBrackets (ListBraces, ListNone), LMatch)
import Development.IDE.GHC.Compat (getLoc)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Control.Monad.Trans.Maybe ( MaybeT(runMaybeT) )
import Development.IDE.GHC.Compat.ExactPrint
    ( exactPrint, d0, d1, noAnnSrcSpanDP1, setEntryDP )
import Data.Data (Data())
import Data.Generics.Schemes (everywhereM)
import Type.Reflection (eqTypeRep,
                        type (:~~:) (HRefl),
                        typeRep, typeOf)
import GHC.Hs (GhcPs, deltaPos, unnamedHoleRdrName)
import GHC.Types.SrcLoc (SrcSpan(RealSrcSpan), GenLocated (L), combineSrcSpans)
import Language.Haskell.Syntax.Expr (HsExpr (HsCase, HsHole), Match (..), GRHSs (GRHSs), GRHS (GRHS))
import Control.Monad.Trans (lift)
import Ide.PluginUtils (diffText, WithDeletions (IncludeDeletions))
import Development.IDE.Core.FileStore (getVersionedTextDoc)
import qualified Language.LSP.Protocol.Lens                   as L
import Language.Haskell.Syntax (MatchGroup (MG, mg_alts), LHsExpr, NoExtField (NoExtField), Pat (..), HsConDetails (PrefixCon), HsLocalBindsLR (EmptyLocalBinds))
import GHC (EpAnn(EpAnn))
import GHC.Parser.Annotation (noSrcSpanA, EpUniToken (EpUniTok), IsUnicodeSyntax (NormalSyntax), emptyComments, TrailingAnn (AddSemiAnn), addTrailingAnnToA, AnnList (al_brackets, al_anchor))
import qualified Data.List.NonEmpty.Extra as NE (singleton, minimumBy1, fromList)
import Development.IDE.GHC.Compat.Core (GrhsAnn(..), HasSrcSpan, srcSpanStartLine, LocatedAn, lann_trailing, AnnListItem, srcSpanStartCol, EpAnnHsCase (EpAnnHsCase), HsMatchContext (LamAlt), HsLamVariant (LamCase))
import Data.List.Extra (chunksOf)
import Data.List (groupBy)
import Data.Function (on, (&))
import Control.Arrow ((&&&))
import Development.IDE (Pretty (pretty), Recorder, WithPriority, IdeState (shakeExtras), FileDiagnostic (fdStructuredMessage), runAction, GetParsedModule (GetParsedModule), srcSpanToRange)
import Ide.Logger ((<+>))
import Control.Monad.State.Strict (State, evalState, MonadState (get, put))
import Data.Bifunctor (bimap)

data Log
  = LogShake Shake.Log
  | LogWAEResponseError (LSP.TResponseError LSP.Method_WorkspaceApplyEdit)
  | forall a. (Pretty a) => LogResolve a

instance Pretty Log where
  pretty = \case
    LogShake logMsg -> pretty logMsg
    LogWAEResponseError rspErr -> "RequestWorkspaceApplyEdit Failed with " <+> pretty rspErr
    LogResolve msg -> pretty msg

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor _ plId = (defaultPluginDescriptor plId "Provides a code action to split case")
  { pluginHandlers = mkPluginHandler LSP.SMethod_TextDocumentCodeAction suggestCaseSplitProvider
  , pluginPriority = 1
  , pluginModifyDynflags = mempty { dynFlagsModifyGlobal = \dynFlags -> dynFlags { maxUncoveredPatterns = 30 } }
  }

suggestCaseSplitProvider :: PluginMethodHandler IdeState 'Method_TextDocumentCodeAction
suggestCaseSplitProvider
  state
  _
  CodeActionParams{ _textDocument
                  , _range = range
                  }
  = do
  nfp <- getNormalizedFilePathE $ _textDocument ^. L.uri

  verTxtDocId <- liftIO $ runAction "what to put here?" state $ getVersionedTextDoc _textDocument

  pm <- runActionE "not important, as long as unique" state
      $ useE GetParsedModule nfp

  (fromJust -> fileDiags) <- activeDiagnosticsInRange (shakeExtras state) nfp range

  -- TODO: I don't like this stair-casing, but if I don't, then I have to invent a few names.
  -- Any better approach?
  case fileDiags
       -- pair each file diag with its ds messages, if any
     & fmap (id &&& getMaybeDsMsg)
       -- discard those without ds messages
     & mapMaybe sequence of
        -- if none left, return no action
        [] -> pure $ InL []
        -- if some left
        (fileDiagAndDsMsg)

            -> case fileDiagAndDsMsg
                  -- assume at least one entry
                  & NE.fromList
                  -- obtain the innermost
                  & NE.minimumBy1 (ordSubrange `on` Diag._range . fdLspDiagnostic . fst)
                  -- extract the `Diagnostic` and the pattern-match constructos
                  & bimap fdLspDiagnostic dsMsgToPmAlts
                  of

          (_, Nothing) -> error "This should not be possible."
          (_, Just []) -> pure $ InL [] -- This happens when the type of the expression is unknown
          (diag, Just pmAltsConApp) -> do

            (old, new) <- handleMaybeM (PluginInternalError "Unable to makeEditText")
                $ liftIO $ runMaybeT
                $ makeEditText pm pmAltsConApp range

            caps <- lift pluginGetClientCapabilities

            pure $ InL [InR
              $ CodeAction { _title       = "Add placeholders for all missing patterns"
                           , _kind        = Just CodeActionKind_QuickFix
                           , _diagnostics = Just [diag]
                           , _isPreferred = Nothing -- TODO: Just True?
                           , _disabled    = Nothing
                           , _edit        = Just $ diffText caps (verTxtDocId, old) new IncludeDeletions
                           , _command     = Nothing
                           , _data_       = Nothing }]

  where

    getMaybeDsMsg :: FileDiagnostic -> Maybe DsMessage
    getMaybeDsMsg d = fdStructuredMessage d ^? _SomeStructuredMessage . msgEnvelopeErrorL . _DsMessage

    dsMsgToPmAlts :: DsMessage -> Maybe [PmAltConApp]
    dsMsgToPmAlts =
      \case DsNonExhaustivePatterns !CaseAlt _ _ ![ids] !nablas ->
                Just $ nablas >>=
                  (\nabla -> let facts = ts_facts $ nabla_tm_st nabla
                             in case vi_pos <$> lookupUSDFM facts ids of
                                   Just [x] -> [x]
                                   Just [] -> []
                                   _ -> error "This should not be possible.")
            DsNonExhaustivePatterns (LamAlt LamCase) _ _ _ _ -> Just [] -- TODO: implement this
            _ -> error "This should not be possible."

-- Assign an `Ordering` to two `Range`s of which either is assumed to be subset of the other.
ordSubrange :: Range -> Range -> Ordering
ordSubrange r1 r2
  | r1 == r2 = EQ
  | r1 `isSubrangeOf` r2 = LT
  | r2 `isSubrangeOf` r1 = GT
  | otherwise = error "Not meant to be used this way"

_DsMessage :: Fold GhcMessage DsMessage
_DsMessage = prism' GhcDsMessage $ \case
  GhcDsMessage dsmsg -> Just dsmsg
  _ -> Nothing

makeEditText :: Monad m => ParsedModule -> MissingPatterns -> Range -> MaybeT m (T.Text, T.Text)
makeEditText pm missingPs range = do

  let ps = pm_parsed_source pm
      old = T.pack $ exactPrint ps
      ps' = everywhereM go ps -- We transform the `ParsedSource` bottom-up
            `evalState` False -- and we pass a `Bool` through `State` to update only one node.
      new = T.pack $ exactPrint ps'

  pure (old, new)

    where
      go :: forall d. Data d => d -> State Bool d
      go node = do
          found <- get
          if found
            then pure node
            else case typeOf node `eqTypeRep` typeRep @(HsExpr GhcPs) of
                    Nothing -> pure node
                    Just HRefl -> case node of
                      HsCase x@(EpAnnHsCase (EpTok (getHasLoc -> caseSSpan)) (EpTok (getHasLoc -> ofSSpan)))
                             e
                             existingPs@(MG _ (L (EpAnn (getHasLoc -> endSSpan) _ _) _))
                        | range `inSpan`  caseExprSpan caseSSpan ofSSpan endSSpan
                        -> do put True
                              pure $ HsCase x e $ addMissingPatterns (makeMatch <$> missingPs) existingPs
                      _ -> pure node

caseExprSpan :: SrcSpan -> SrcSpan -> SrcSpan -> SrcSpan
caseExprSpan caseSSpan _ endSSpan@(RealSrcSpan _ _) = combineSrcSpans caseSSpan endSSpan
caseExprSpan caseSSpan ofSSpan _ = combineSrcSpans caseSSpan ofSSpan

inSpan :: Range -> SrcSpan -> Bool
inSpan range s = maybe False (range `isSubrangeOf`) (srcSpanToRange s)

isOnelined :: LocatedAn ann e -> LocatedAn ann e -> Bool
isOnelined = (==) `on` getStartLine

isBraced :: EpAnn (AnnList a) -> Bool
isBraced (EpAnn _ (AnnList _ (ListBraces (EpTok (EpaSpan _)) _) _ _ _) _) = True
isBraced (EpAnn _ (AnnList _ ListNone                           _ _ _) _) = False
isBraced _ = error "Ooops"

getStartCol :: HasSrcSpan a => a -> Int
getStartCol = srcSpanStartCol . realSrcSpan . getLoc

getStartLine :: HasSrcSpan a => a -> Int
getStartLine = srcSpanStartLine . realSrcSpan . getLoc

setDP :: Int -> Int -> LocatedAn t a -> LocatedAn t a
setDP = (flip setEntryDP .) . deltaPos

-- Add semicolon, unless one is already present.
addSemiCol :: LocatedAn AnnListItem a -> LocatedAn AnnListItem a
addSemiCol (L l@(EpAnn _ ls _) e)
  | none isSemiCol (lann_trailing ls)
  = L (addTrailingAnnToA (AddSemiAnn (EpTok d0)) emptyComments l) e
addSemiCol l = l

addMissingPatterns :: [LMatch GhcPs (LHsExpr GhcPs)] -> MatchGroup GhcPs (LHsExpr GhcPs) -> MatchGroup GhcPs (LHsExpr GhcPs)
addMissingPatterns missing mg@(MG { mg_alts = L l [] })
  = mg { mg_alts = L l (fmt missing) }
    where
      fmt = if isBraced l
              then zipWith3 (.)
                            (replicate (length missing - 1) addSemiCol ++ [id])
                            (repeat $ setDP 1 defaultIndent)
              else zipWith ($)
                           (setDP 1 defaultIndent:repeat (setDP 1 0))

addMissingPatterns missing mg@(MG { mg_alts = L altsLoc@(EpAnn _ ann _) existings })
  = case ann of
      (fmap (getStartCol . getHasLoc) . al_anchor &&& al_brackets -> (Just anchor, brackets))
        -> mg { mg_alts = L altsLoc (alts anchor brackets) }
      _ -> error "This should not be possible"
  where
    alts anchor brackets
         = let -- groups of patterns on the same line
               ptrnGrps = groupBy isOnelined existings
               nGrps = length ptrnGrps
               -- length of the last group of ≥1 patterns written on one line
               nLastGr = ptrnGrps
                       & reverse
                       & head
                       & length
               (indent, addSemiCols)
                 = if isBraced altsLoc
                       then (anchor - case brackets of
                                     ListBraces (EpTok (getStartCol . getHasLoc -> col)) _ -> col
                                     _ -> error "this is impossible"
                            ,zipWith ($) (replicate (nGrps - 1) id
                                       ++ replicate (length missingGrps) (mapLast addSemiCol)
                                       ++ [id]))
                       else (0, id)
               missingGrps = chunksOf nLastGr missing
                           <&> \case [] -> error "Impossible"
                                     (m:ms) -> zipWith ($) (replicate (length ms) addSemiCol ++ [id])
                                                           (setDP 1 indent m:map (setDP 0 1) ms)

           in concat $ addSemiCols $ ptrnGrps ++ missingGrps

mapLast :: (a -> a) -> [a] -> [a]
mapLast f [a] = [f a]
mapLast f (a:as) = a:mapLast f as
mapLast _ [] = error "nope"

isSemiCol :: TrailingAnn -> Bool
isSemiCol (AddSemiAnn _) = True
isSemiCol _ = False

makeMatch :: PmAltConApp -> LMatch GhcPs (LHsExpr GhcPs)
makeMatch PACA{ paca_con = PmAltConLike (RealDataCon ctor), .. }
        = L noSrcSpanA
        $ Match { m_ext = NoExtField
                , m_ctxt = CaseAlt
                , m_pats = L noSrcSpanA
                         $ [L noSrcSpanA ConPat { pat_con_ext = (Nothing, Nothing)
                                                , pat_con = L noSrcSpanA $ nameRdrName $ getName ctor
                                                , pat_args = PrefixCon $ map (const $ L noAnnSrcSpanDP1 $ WildPat NoExtField) paca_ids
                                                }]
                , m_grhss = GRHSs emptyComments
                                  -- TODO: remember to respect unicode arrow of preceeding cases or the -XUnicodeSyntax (https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/unicode_syntax.html#extension-UnicodeSyntax)
                                  -- TODO: check whether ga_sep default choice is really not printing anything.
                                  (NE.singleton $ L noSrcSpanA $ GRHS (EpAnn noSrcSpanA
                                                                          (GrhsAnn{ ga_vbar = Nothing
                                                                                  , ga_sep = Right (EpUniTok d1 NormalSyntax) })
                                                                          emptyComments) []
                                                            $ L noSrcSpanA $ HsHole $ HoleVar $ L noAnnSrcSpanDP1
                                             $ unnamedHoleRdrName)
                                  (EmptyLocalBinds NoExtField)
                }
makeMatch _ = error "boom"

type MissingPatterns = [PmAltConApp]

defaultIndent :: Int
defaultIndent = 2

none :: Foldable t => (a -> Bool) -> t a -> Bool
none = all . (not <$>)
