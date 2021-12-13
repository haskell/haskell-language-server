{-# LANGUAGE RecordWildCards  #-}

{-# LANGUAGE NoMonoLocalBinds #-}

module Wingman.AbstractLSP.TacticActions where

import           Control.Monad (when)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Maybe (mapMaybeT)
import           Data.Foldable
import           Data.Maybe (listToMaybe)
import           Data.Proxy
import           Development.IDE hiding (rangeToRealSrcSpan)
import           Development.IDE.Core.UseStale
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint
import           Generics.SYB.GHC (mkBindListT, everywhereM')
import           Wingman.AbstractLSP.Types
import           Wingman.CaseSplit
import           Wingman.GHC (liftMaybe, isHole, pattern AMatch, unXPat)
import           Wingman.Judgements (jNeedsToBindArgs)
import           Wingman.LanguageServer (runStaleIde)
import           Wingman.LanguageServer.TacticProviders
import           Wingman.Machinery (runTactic, scoreSolution)
import           Wingman.Range
import           Wingman.Types
import Development.IDE.Core.Service (getIdeOptionsIO)
import Development.IDE.Types.Options (IdeTesting(IdeTesting), IdeOptions (IdeOptions, optTesting))


------------------------------------------------------------------------------
-- | An 'Interaction' for a 'TacticCommand'.
makeTacticInteraction
    :: TacticCommand
    -> Interaction
makeTacticInteraction cmd =
  Interaction $ Continuation @_ @HoleTarget cmd
    (SynthesizeCodeAction $ \env hj -> do
      pure $ commandProvider cmd $
            TacticProviderData
              { tpd_lspEnv    = env
              , tpd_jdg       = hj_jdg hj
              , tpd_hole_sort = hj_hole_sort hj
              }
    )
    $ \LspEnv{..} HoleJudgment{..} FileContext{..} var_name -> do
        let stale a = runStaleIde "tacticCmd" le_ideState fc_nfp a

        let span = fmap (rangeToRealSrcSpan (fromNormalizedFilePath fc_nfp)) hj_range
        TrackedStale _ pmmap <- mapMaybeT liftIO $ stale GetAnnotatedParsedSource
        pm_span <- liftMaybe $ mapAgeFrom pmmap span
        IdeOptions{optTesting = IdeTesting isTesting} <-
            liftIO $ getIdeOptionsIO (shakeExtras le_ideState)

        let t = commandTactic cmd var_name
            timeout = if isTesting then maxBound else cfg_timeout_seconds le_config * seconds

        liftIO $ runTactic timeout hj_ctx hj_jdg t >>= \case
          Left err ->
            pure
              $ pure
              $ ErrorMessages
              $ pure
              $ mkUserFacingMessage err
          Right rtr ->
            case rtr_extract rtr of
              L _ (HsVar _ (L _ rdr)) | isHole (occName rdr) ->
                pure
                  $ addTimeoutMessage rtr
                  $ pure
                  $ ErrorMessages
                  $ pure NothingToDo
              _ -> do
                for_ (rtr_other_solns rtr) $ \soln -> do
                  traceMX "other solution" $ syn_val soln
                  traceMX "with score" $ scoreSolution soln (rtr_jdg rtr) []
                traceMX "solution" $ rtr_extract rtr
                pure
                  $ addTimeoutMessage rtr
                  $ pure
                  $ GraftEdit
                  $ graftHole (RealSrcSpan (unTrack pm_span) Nothing) rtr


addTimeoutMessage :: RunTacticResults -> [ContinuationResult] -> [ContinuationResult]
addTimeoutMessage rtr = mappend
  [ ErrorMessages $ pure TimedOut
  | rtr_timed_out rtr
  ]


------------------------------------------------------------------------------
-- | The number of microseconds in a second
seconds :: Num a => a
seconds = 1e6


------------------------------------------------------------------------------
-- | Transform some tactic errors into a 'UserFacingMessage'.
mkUserFacingMessage :: [TacticError] -> UserFacingMessage
mkUserFacingMessage errs
  | elem OutOfGas errs = NotEnoughGas
mkUserFacingMessage [] = NothingToDo
mkUserFacingMessage _ = TacticErrors


------------------------------------------------------------------------------
-- | Graft a 'RunTacticResults' into the correct place in an AST. Correctly
-- deals with top-level holes, in which we might need to fiddle with the
-- 'Match's that bind variables.
graftHole
    :: SrcSpan
    -> RunTacticResults
    -> Graft (Either String) ParsedSource
graftHole span rtr
  | _jIsTopHole (rtr_jdg rtr)
      = genericGraftWithSmallestM
            (Proxy @(Located [LMatch GhcPs (LHsExpr GhcPs)])) span
      $ \dflags matches ->
          everywhereM'
            $ mkBindListT $ \ix ->
              graftDecl dflags span ix $ \name pats ->
              splitToDecl
                (case not $ jNeedsToBindArgs (rtr_jdg rtr) of
                   -- If the user has explicitly bound arguments, use the
                   -- fixity they wrote.
                   True -> matchContextFixity . m_ctxt . unLoc
                             =<< listToMaybe matches
                   -- Otherwise, choose based on the name of the function.
                   False -> Nothing
                )
                (occName name)
            $ iterateSplit
            $ mkFirstAgda (fmap unXPat pats)
            $ unLoc
            $ rtr_extract rtr
graftHole span rtr
  = graft span
  $ rtr_extract rtr


------------------------------------------------------------------------------
-- | Keep a fixity if one was present in the 'HsMatchContext'.
matchContextFixity :: HsMatchContext p -> Maybe LexicalFixity
matchContextFixity (FunRhs _ l _) = Just l
matchContextFixity _ = Nothing


------------------------------------------------------------------------------
-- | Helper function to route 'mergeFunBindMatches' into the right place in an
-- AST --- correctly dealing with inserting into instance declarations.
graftDecl
    :: DynFlags
    -> SrcSpan
    -> Int
    -> (RdrName -> [Pat GhcPs] -> LHsDecl GhcPs)
    -> LMatch GhcPs (LHsExpr GhcPs)
    -> TransformT (Either String) [LMatch GhcPs (LHsExpr GhcPs)]
graftDecl dflags dst ix make_decl (L src (AMatch (FunRhs (L _ name) _ _) pats _))
  | dst `isSubspanOf` src = do
      L _ dec <- annotateDecl dflags $ make_decl name pats
      case dec of
        ValD _ (FunBind { fun_matches = MG { mg_alts = L _ alts@(first_match : _)}
                  }) -> do
          -- For whatever reason, ExactPrint annotates newlines to the ends of
          -- case matches and type signatures, but only allows us to insert
          -- them at the beginning of those things. Thus, we need want to
          -- insert a preceeding newline (done in 'annotateDecl') on all
          -- matches, except for the first one --- since it gets its newline
          -- from the line above.
          when (ix == 0) $
            setPrecedingLinesT first_match 0 0
          pure alts
        _ -> lift $ Left "annotateDecl didn't produce a funbind"
graftDecl _ _ _ _ x = pure $ pure x

