{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Ide.Plugin.InlayHints.Fixity(fixityRule, fixityInlayHints) where

import           Control.DeepSeq                      (NFData (rnf), rwhnf)
import           Control.Monad.Except                 (ExceptT)
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Data.Either                          (isRight)
import           Data.Hashable                        (Hashable)
import qualified Data.Map.Strict                      as M
import           Data.Maybe                           (fromMaybe)
import qualified Data.Text                            as T
import           Development.IDE                      (Action,
                                                       GhcSessionDeps (GhcSessionDeps),
                                                       HieAstResult (HAR, refMap),
                                                       NormalizedFilePath,
                                                       Position (Position),
                                                       RuleResult, Rules,
                                                       TcModuleResult (tmrTypechecked),
                                                       TypeCheck (TypeCheck),
                                                       cmapWithPrio, define,
                                                       hscEnv, use_)
import           Development.IDE.Core.PluginUtils     (useWithStaleE)
import           Development.IDE.Core.PositionMapping (idDelta)
import           Development.IDE.Core.RuleTypes       (GetHieAst (GetHieAst))
import           Development.IDE.Core.Shake           (addPersistentRule)
import           Development.IDE.GHC.Compat           (Fixity (Fixity), Name,
                                                       TcGblEnv, defaultFixity,
                                                       initTcWithGbl,
                                                       lookupFixityRn,
                                                       mkRealSrcLoc,
                                                       realSrcLocSpan,
                                                       realSrcSpanEnd,
                                                       srcLocCol, srcLocLine)
import           Development.IDE.GHC.Compat.Core      (HscEnv)
import qualified Development.IDE.GHC.Compat.Util      as Util
import           Development.IDE.GHC.Util             (printOutputable)
import           GHC.Generics                         (Generic)
import           Ide.Logger                           (Recorder, WithPriority)
import           Ide.Plugin.Error                     (PluginError)
import           Ide.Plugin.InlayHints.Types          (InlayHintLog (LogShake))
import           Language.LSP.Protocol.Types          (InlayHint (InlayHint),
                                                       Null, maybeToNull,
                                                       type (|?) (InL))

-------

fixityInlayHints :: NormalizedFilePath -> ExceptT PluginError Action ([InlayHint] |? Null)
fixityInlayHints nfp = do
    (FixityMap fixmap, _) <- useWithStaleE GetFixity nfp
    pure $ maybeToNull $ toAbsInlayHints fixmap
    where
        toAbsInlayHints :: M.Map Position Fixity -> Maybe [InlayHint]
        toAbsInlayHints fixmap =
            Just (M.elems $ M.mapWithKey (\(Position x y) (Fixity _ pre direction) ->
              InlayHint
                (Position (x - 1) (y - 1))
                -- infixr => r
                (InL ((T.takeEnd 1 $ printOutputable direction)
                      <> printOutputable pre))
                Nothing Nothing Nothing Nothing Nothing Nothing
              ) fixmap)

-------

newtype FixityMap = FixityMap (M.Map Position Fixity)
instance Show FixityMap where
  show _ = "FixityMap"

instance NFData FixityMap where
  rnf (FixityMap xs) = rnf xs

instance NFData Fixity where
  rnf = rwhnf

data GetFixity = GetFixity deriving (Show, Eq, Generic)

instance Hashable GetFixity
instance NFData GetFixity

type instance RuleResult GetFixity = FixityMap

fixityRule :: Recorder (WithPriority InlayHintLog) -> Rules ()
fixityRule recorder = do
    define (cmapWithPrio LogShake recorder) $ \GetFixity nfp -> do
        HAR{refMap} <- use_ GetHieAst nfp
        -- deps necessary so that we can consult already loaded in ifaces instead of loading in duplicates
        env <- hscEnv <$> use_ GhcSessionDeps nfp
        tcGblEnv <- tmrTypechecked <$> use_ TypeCheck nfp
        fs <- lookupFixities env tcGblEnv $
            M.mapKeys (\(Right x) -> x)
            $ M.filterWithKey (\k _ -> isRight k)
            $ M.map
                (fmap $ (\loc ->
                    Position (fromIntegral $ srcLocLine loc)
                             (fromIntegral $ srcLocCol loc))
                . realSrcSpanEnd
                . fst)
                refMap
        pure ([], Just (FixityMap fs))

    -- Ensure that this plugin doesn't block on startup
    addPersistentRule GetFixity $ const $ pure $ Just (FixityMap M.empty, idDelta, Nothing)

-- | Convert a HieAST to FixityTree with fixity info gathered
lookupFixities :: MonadIO m => HscEnv -> TcGblEnv -> M.Map Name [Position] -> m (M.Map Position Fixity)
lookupFixities hscEnv tcGblEnv names
    = liftIO
    $ fmap (fromMaybe M.empty . snd)
    $ initTcWithGbl hscEnv tcGblEnv (realSrcLocSpan $ mkRealSrcLoc "<dummy>" 1 1)
    $ M.traverseMaybeWithKey (\_ v -> v)
    $ M.fromList
    $ concat
    $ M.elems
    $ M.mapWithKey lookupFixity names
  where
    lookupFixity name positions =
      fmap (,fixity) positions
      where
        fixity = do
          f <- Util.handleGhcException
            (const $ pure Nothing)
            (Just <$> lookupFixityRn name)
          if f == Just defaultFixity
          then pure Nothing
          else pure f
