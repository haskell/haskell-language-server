{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Ide.Plugin.InlayHints.LocalBinding (localBindingRule, localBindingInlayHints) where

import           Control.Applicative                  ((<|>))
import           Control.DeepSeq                      (NFData (rnf))
import           Control.Monad.Except                 (ExceptT)
import qualified Control.Monad.Extra                  as M
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Class            (MonadTrans (lift))
import           Control.Monad.Trans.Maybe            (MaybeT (..))
import           Data.Hashable                        (Hashable)
import qualified Data.Map                             as M
import           Data.Maybe                           (fromMaybe, mapMaybe)
import qualified Data.Text                            as T
import           Development.IDE                      (Action,
                                                       DocAndTyThingMap (DKMap),
                                                       GetDocMap (GetDocMap),
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
import           Development.IDE.GHC.Compat           (BindType (RegularBind),
                                                       ContextInfo (TyDecl, ValBind),
                                                       HieAST (..),
                                                       HieASTs (..),
                                                       HieFile (hie_types),
                                                       Identifier,
                                                       IdentifierDetails (..),
                                                       NodeInfo (..),
                                                       Scope (LocalScope),
                                                       hieTypeToIface,
                                                       lookupNameEnv, nodeInfo,
                                                       nodeInfo',
                                                       recoverFullType)
import           Development.IDE.Spans.Common         (TyThingMap,
                                                       safeTyThingType)
import           Development.IDE.Types.Location       (NormalizedFilePath)
import           GHC.Generics                         (Generic)
import           Ide.Plugin.Error                     (PluginError)
import           Ide.Plugin.InlayHints.Types          (InlayHintLog (LogShake))
import           Language.LSP.Protocol.Types          (InlayHint (InlayHint),
                                                       Null, type (|?) (InL))

-------

localBindingInlayHints :: NormalizedFilePath -> ExceptT PluginError Action ([InlayHint] |? Null)
localBindingInlayHints nfp = do
    (LocalBindingMap hmap) <- useE GetLocalBinding nfp
    pure $ InL $ toAbsInlayHints hmap
    where
        toAbsInlayHints :: M.Map Position T.Text -> [InlayHint]
        toAbsInlayHints hmap =
            M.elems $ M.mapWithKey (\pos content ->
               InlayHint pos (InL $ " :: " <> content)
                Nothing Nothing Nothing Nothing Nothing Nothing) hmap

-------

localBindingRule :: Recorder (WithPriority InlayHintLog) -> Rules ()
localBindingRule recorder = do
    define (cmapWithPrio LogShake recorder) $ \GetLocalBinding nfp -> do
        bindings <- lookupLocalBindings nfp
        pure ([], Just bindings)

    -- Ensure that this plugin doesn't block on startup
    addPersistentRule GetLocalBinding $ const $ pure $ Just (LocalBindingMap M.empty, idDelta, Nothing)

newtype LocalBindingMap = LocalBindingMap (M.Map Position T.Text)
instance Show LocalBindingMap where
  show _ = "LocalBindingMap"

instance NFData LocalBindingMap where
  rnf (LocalBindingMap map) = rnf map

data GetLocalBinding = GetLocalBinding deriving (Show, Eq, Generic)

instance Hashable GetLocalBinding
instance NFData GetLocalBinding

type instance RuleResult GetLocalBinding = LocalBindingMap

lookupLocalBindings
    :: NormalizedFilePath
    -> Action LocalBindingMap
lookupLocalBindings nfp
    = flip fmap (getAtPoints nfp) $ \case
        Just hm -> hm
        Nothing -> LocalBindingMap M.empty

getAtPoints
    :: NormalizedFilePath
    -> Action (Maybe LocalBindingMap)
getAtPoints nfp = runMaybeT $ do
  (har, _) <- useWithStaleMT GetHieAst nfp
  (DKMap _ km) <- lift $ maybe (DKMap mempty mempty) fst <$> runMaybeT (useWithStaleMT GetDocMap nfp)

  MaybeT
    $ liftIO
    $ Just . LocalBindingMap
    <$> atPoints har km


atPoints
  :: HieAstResult
  -> TyThingMap
  -> IO (M.Map Position T.Text)
atPoints (HAR _ hf _ _ (kind :: HieKind hietype)) km =
    fmap M.fromList $ M.concatMapM inlayHintInfo $ M.elems (getAsts hf)
  where
    inlayHintInfo :: HieAST hietype -> IO [(Position, T.Text)]
    inlayHintInfo = return . inlayHintInfo'
      where
        inlayHintInfo' :: HieAST hietype -> [(Position, T.Text)]
        inlayHintInfo' ast =
            M.toList $ M.mapMaybe id $ M.map prettyName (filteredIdentifiers (flattenAST ast))

        flattenAST :: HieAST hietype -> [HieAST hietype]
        flattenAST ast = case nodeChildren ast of
            []   -> [ast]
            asts -> concatMap flattenAST asts

        pos :: HieAST hietype -> Position
        pos ast = _end (realSrcSpanToRange $ nodeSpan ast)

        info :: HieAST hietype -> NodeInfo hietype
        info ast = nodeInfoH kind ast

        -- TODO: This is damn messy, but I don't know how to do it better
        filteredIdentifiers :: [HieAST hietype] -> M.Map Position (Identifier, IdentifierDetails hietype)
        filteredIdentifiers asts = M.fromList $ fmap (\(idr, (pos, idrd)) -> (pos, (idr, idrd))) $ M.toList $
            (M.fromList $ fmap (\((idr, idrd), pos) -> (idr, (pos, idrd))) localBindList)
            `M.difference`
            (M.fromList $ fmap (\((idr, idrd), pos) -> (idr, (pos, idrd))) hasSigDeclList)
            where
                allNodeIdentifiers' = fmap (\ast ->
                    (case M.toList $ nodeIdentifiers $ info ast of
                        [(k, v)] -> Just (k, v)
                        _        -> Nothing
                    , pos ast)
                    ) asts
                localBindList =
                    mapMaybe
                        (\(a, b) -> fmap (,b) a)
                        [ (node, pos) | (node, pos) <- allNodeIdentifiers', any (any bindLocal . identInfo . snd) node ]
                hasSigDeclList =
                    mapMaybe
                        (\(a, b) -> fmap (,b) a)
                        [ (node, pos) | (node, pos) <- allNodeIdentifiers', any (any hasSigDecl . identInfo . snd) node ]

                bindLocal, hasSigDecl :: ContextInfo -> Bool
                bindLocal (ValBind RegularBind (LocalScope _) _) = True
                bindLocal _                                      = False
                hasSigDecl TyDecl = True
                hasSigDecl _      = False

        prettyName :: (Identifier, IdentifierDetails hietype) -> Maybe T.Text
        prettyName (Right n, dets) =
          Just $ fromMaybe "?" ((prettyType <$> identType dets) <|> maybeKind)
          where
            maybeKind = fmap printOutputable $ safeTyThingType =<< lookupNameEnv km n
        prettyName (Left _, _) = Nothing

        prettyType :: hietype -> T.Text
        prettyType t = case kind of
          HieFresh -> printOutputable t
          HieFromDisk full_file -> printOutputable $ hieTypeToIface $ recoverFullType t (hie_types full_file)

-- In ghc9, nodeInfo is monomorphic, so we need a case split here
nodeInfoH :: HieKind a -> HieAST a -> NodeInfo a
nodeInfoH (HieFromDisk _) = nodeInfo'
nodeInfoH HieFresh        = nodeInfo
