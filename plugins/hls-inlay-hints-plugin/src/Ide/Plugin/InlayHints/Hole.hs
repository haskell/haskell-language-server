{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid restricted function" #-}

module Ide.Plugin.InlayHints.Hole (holeInlayHints, holeRule) where

import           Control.DeepSeq                      (NFData (rnf))
import           Control.Monad.Except                 (ExceptT)
import qualified Control.Monad.Extra                  as M
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Data.Hashable                        (Hashable)
import qualified Data.Map                             as M
import           Data.Maybe                           (maybeToList)
import qualified Data.Text                            as T
import qualified Debug.Trace                          as Debug
import           Development.IDE                      (Action,
                                                       GetHieAst (GetHieAst),
                                                       HieKind (HieFresh, HieFromDisk),
                                                       Position, Range (_end),
                                                       Recorder, RuleResult,
                                                       Rules, WithPriority,
                                                       cmapWithPrio, define,
                                                       printOutputable,
                                                       realSrcSpanToRange)
import           Development.IDE.Core.PluginUtils     (useE, useWithStaleMT)
import           Development.IDE.Core.PositionMapping (idDelta)
import           Development.IDE.Core.RuleTypes       (HieAstResult (..))
import           Development.IDE.Core.Shake           (addPersistentRule)
import           Development.IDE.GHC.Compat           (HieAST (..),
                                                       HieASTs (..),
                                                       HieFile (hie_types),
                                                       NodeInfo (..),
                                                       hieTypeToIface,
                                                       nodeAnnotations,
                                                       nodeInfo, nodeInfo',
                                                       recoverFullType)
import           Development.IDE.Types.Location       (NormalizedFilePath)
import           GHC.Generics                         (Generic)
import           Ide.Plugin.Error                     (PluginError)
import           Ide.Plugin.InlayHints.Types          (InlayHintLog (LogShake))
import           Language.LSP.Protocol.Types          (InlayHint (InlayHint),
                                                       Null, type (|?) (InL))

-------

holeInlayHints :: NormalizedFilePath -> ExceptT PluginError Action ([InlayHint] |? Null)
holeInlayHints nfp = do
    (HoleMap hmap) <- useE GetHoles nfp
    pure $ InL $ toAbsInlayHints hmap
    where
        toAbsInlayHints :: M.Map Position T.Text -> [InlayHint]
        toAbsInlayHints hmap =
            M.elems $ M.mapWithKey (\pos content ->
               InlayHint pos (InL $ " :: " <> content)
                Nothing Nothing Nothing Nothing Nothing Nothing) hmap

-------

holeRule :: Recorder (WithPriority InlayHintLog) -> Rules ()
holeRule recorder = do
    define (cmapWithPrio LogShake recorder) $ \GetHoles nfp -> do
        holes <- lookupHoles nfp
        pure ([], Just holes)

    -- Ensure that this plugin doesn't block on startup
    addPersistentRule GetHoles $ const $ pure $ Just (HoleMap M.empty, idDelta, Nothing)

-------

newtype HoleMap = HoleMap (M.Map Position T.Text)
instance Show HoleMap where
  show _ = "LocalBindingMap"

instance NFData HoleMap where
  rnf (HoleMap map) = rnf map

data GetHoles = GetHoles deriving (Show, Eq, Generic)

instance Hashable GetHoles
instance NFData GetHoles

type instance RuleResult GetHoles = HoleMap

lookupHoles
    :: NormalizedFilePath
    -> Action HoleMap
lookupHoles nfp
    = flip fmap (getAtPoints nfp) $ \case
        Just hm -> hm
        Nothing -> HoleMap M.empty

getAtPoints
    :: NormalizedFilePath
    -> Action (Maybe HoleMap)
getAtPoints nfp = runMaybeT $ do
  (har, _) <- useWithStaleMT GetHieAst nfp
  MaybeT $ liftIO $ Just . HoleMap <$> atPoints har


atPoints
  :: HieAstResult
  -> IO (M.Map Position T.Text)
atPoints (HAR _ hf _ _ (kind :: HieKind hietype)) =
    fmap M.fromList $ M.concatMapM inlayHintInfo $ M.elems (getAsts hf)
  where
    inlayHintInfo :: HieAST hietype -> IO [(Position, T.Text)]
    inlayHintInfo = return . inlayHintInfo'
      where
        inlayHintInfo' :: HieAST hietype -> [(Position, T.Text)]
        inlayHintInfo' ast = case nodeChildren ast of
            []   -> maybeToList (toPrettyName ast)
            asts -> concatMap inlayHintInfo' asts

        toPrettyName :: HieAST hietype -> Maybe (Position, T.Text)
        toPrettyName ast = (pos ast,) <$> toPrettyType ast

        pos :: HieAST hietype -> Position
        pos ast = _end (realSrcSpanToRange $ nodeSpan ast)

        info :: HieAST hietype -> NodeInfo hietype
        info ast = nodeInfoH kind ast

        prettyType :: hietype -> T.Text
        prettyType t = case kind of
          HieFresh -> printOutputable t
          HieFromDisk full_file -> printOutputable $ hieTypeToIface $ recoverFullType t (hie_types full_file)

        firstHole :: HieAST hietype -> Maybe hietype
        firstHole ast =
          let astInfo = info ast
              -- TODO: need ONLY hole
              -- Maybe there's only one solution left: regex diagnostic :(
              isHole a = (==) "HsUnboundVar" $ fst $ Debug.trace (case M.toList
                $ nodeIdentifiers astInfo of
                    [x] -> show $ printOutputable $ fst x
                    _   -> "oops"
                ) id $ Debug.traceShowId a
              nullIds = M.null $ nodeIdentifiers astInfo
           in case nodeType astInfo of
            (x:_) | (any isHole $ nodeAnnotations astInfo) && nullIds -> Just x
            _                                                         -> Nothing

        toPrettyType :: HieAST hietype -> Maybe T.Text
        toPrettyType = fmap prettyType . firstHole

-- In ghc9, nodeInfo is monomorphic, so we need a case split here
nodeInfoH :: HieKind a -> HieAST a -> NodeInfo a
nodeInfoH (HieFromDisk _) = nodeInfo'
nodeInfoH HieFresh        = nodeInfo
