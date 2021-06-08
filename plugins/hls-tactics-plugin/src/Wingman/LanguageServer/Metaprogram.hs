{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE NoMonoLocalBinds  #-}
{-# LANGUAGE RankNTypes #-}

module Wingman.LanguageServer.Metaprogram
  ( hoverProvider
  ) where

import           Control.Applicative (empty)
import           Control.Monad
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.List (find)
import           Data.Maybe
import qualified Data.Text as T
import           Data.Traversable
import           Development.IDE (positionToRealSrcLoc)
import           Development.IDE (realSrcSpanToRange)
import           Development.IDE.Core.RuleTypes
import           Development.IDE.Core.Shake (IdeState (..))
import           Development.IDE.Core.UseStale
import           Development.IDE.GHC.Compat
import           GhcPlugins (containsSpan, realSrcLocSpan)
import           Ide.Types
import           Language.LSP.Types
import           Prelude hiding (span)
import           Prelude hiding (span)
import           TcRnTypes (tcg_binds)
import           Wingman.GHC
import           Wingman.Judgements.SYB (metaprogramQ)
import           Wingman.LanguageServer
import           Wingman.Metaprogramming.Parser (attempt_it)
import           Wingman.Types


------------------------------------------------------------------------------
-- | Provide the "empty case completion" code lens
hoverProvider :: PluginMethodHandler IdeState TextDocumentHover
hoverProvider state plId (HoverParams (TextDocumentIdentifier uri) (unsafeMkCurrent -> pos) _)
  | Just nfp <- uriToNormalizedFilePath $ toNormalizedUri uri = do
      let loc = fmap (realSrcLocSpan . positionToRealSrcLoc nfp) pos

      cfg <- getTacticConfig plId
      liftIO $ fromMaybeT (Right Nothing) $ do
        holes <- getMetaprogramsAtSpan state nfp $ RealSrcSpan $ unTrack loc

        fmap (Right . Just) $
          case (find (flip containsSpan (unTrack loc) . unTrack . fst) holes) of
            Just (trss, program) -> do
              let tr_range = fmap realSrcSpanToRange trss
              HoleJudgment{hj_jdg=jdg, hj_ctx=ctx} <- judgementForHole state nfp tr_range cfg
              ps <- getParserState state nfp ctx
              z <- liftIO $ flip runReaderT ps $ attempt_it ctx jdg $ T.unpack program
              pure $ Hover
                { _contents = HoverContents
                            $ MarkupContent MkMarkdown
                            $ either T.pack T.pack z
                , _range = Just $ unTrack tr_range
                }
            Nothing -> empty
hoverProvider _ _ _ = pure $ Right Nothing


fromMaybeT :: Functor m => a -> MaybeT m a -> m a
fromMaybeT def = fmap (fromMaybe def) . runMaybeT


getMetaprogramsAtSpan
    :: IdeState
    -> NormalizedFilePath
    -> SrcSpan
    -> MaybeT IO [(Tracked 'Current RealSrcSpan, T.Text)]
getMetaprogramsAtSpan state nfp ss = do
    let stale a = runStaleIde "getMetaprogramsAtSpan" state nfp a

    TrackedStale tcg tcg_map <- fmap (fmap tmrTypechecked) $ stale TypeCheck

    let scrutinees = traverse (metaprogramQ ss . tcg_binds) tcg
    for scrutinees $ \aged@(unTrack -> (ss, program)) -> do
      case ss of
        RealSrcSpan r   -> do
          rss' <- liftMaybe $ mapAgeTo tcg_map $ unsafeCopyAge aged r
          pure (rss', program)
        UnhelpfulSpan _ -> empty


